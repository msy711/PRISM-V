import argparse
import json
import random
from itertools import product
from pathlib import Path

import numpy as np
import pandas as pd
import scipy.stats as st
import xgboost as xgb
from imblearn.over_sampling import SMOTE
from joblib import dump, load
from sklearn.ensemble import (
    GradientBoostingClassifier,
    StackingClassifier,
    VotingClassifier,
)
from sklearn.feature_selection import SelectFromModel
from sklearn.metrics import (
    accuracy_score,
    classification_report,
    confusion_matrix,
    roc_auc_score,
    roc_curve,
)
from sklearn.model_selection import cross_val_predict, cross_validate
from sklearn.utils import resample
from tqdm import tqdm

random.seed(78)


def get_clf(algorithm, svc_predict_proba=False):
    if algorithm == "svc":
        clf = svm.SVC(
            kernel="linear", C=1, random_state=42, probability=svc_predict_proba
        )
    elif algorithm == "gbc":
        clf = GradientBoostingClassifier(random_state=42)
    elif algorithm == "nb":
        clf = GaussianNB()
    elif algorithm == "ensemble_soft":
        estimators = [
            ("gbc", GradientBoostingClassifier(random_state=42)),
            ("xgb", xgb.XGBClassifier(objective="reg:squarederror", random_state=42)),
        ]
        clf = VotingClassifier(estimators, voting="soft")
    elif algorithm == "ensemble_votereg":
        estimators = [
            ("gbc", GradientBoostingClassifier(random_state=42)),
            ("xgb", xgb.XGBClassifier(objective="reg:squarederror", random_state=42)),
        ]
        clf = VotingRegressor(estimators)
    elif algorithm == "ensemble_stack":
        final_estimator = LogisticRegression(random_state=42)
        estimators = [
            ("gbc", GradientBoostingClassifier(random_state=42)),
            ("xgb", xgb.XGBClassifier(objective="reg:squarederror", random_state=42)),
        ]
        clf = StackingClassifier(estimators=estimators, final_estimator=final_estimator)
    elif algorithm == "xgb":
        clf = xgb.XGBClassifier(objective="reg:squarederror", random_state=42)
    else:
        raise ValueError(f"Wrong algo: {algorithm}")
    return clf


def mean(array):
    return sum(array) / len(array)


def confidence_interval(array, confidence):
    interval = st.t.interval(
        confidence, len(array) - 1, loc=np.mean(array), scale=st.sem(array)
    )
    return interval


def evaluate(evaluations):
    aucs, accuracies = [], []
    confusion_matrices, classification_reports = [], []
    f1s, precisions, sensitivities, specificities = [], [], [], []
    fprs, tprs, cuts = [], [], []
    for estimator, x, y in evaluations:
        preds = estimator.predict(x)
        y_proba = estimator.predict_proba(x)
        conf_mat = confusion_matrix(y, preds)
        confusion_matrices.append(conf_mat.tolist())
        auc = roc_auc_score(y, preds)
        aucs.append(auc)
        tn, fp, fn, tp = conf_mat.ravel()
        report = classification_report(y, preds, output_dict=True)
        classification_reports.append(report)
        accuracies.append(report["accuracy"])
        f1s.append(report["macro avg"]["f1-score"])
        precisions.append(report["macro avg"]["precision"])
        sensitivities.append(report["macro avg"]["recall"])
        specificity = tn / (tn + fp)
        specificities.append(specificity)
        fpr, tpr, cut = roc_curve(y, y_proba[:, 1])
        fprs.append(fpr.tolist())
        tprs.append(tpr.tolist())
        cuts.append(cut.tolist())
    return (
        aucs,
        accuracies,
        confusion_matrices,
        specificities,
        classification_reports,
        f1s,
        precisions,
        sensitivities,
        specificities,
        fprs,
        tprs,
        cuts,
    )


def run_experiment(run_id, split, algorithm, df, do_smote, model_save_path):
    feature_columns = [c for c in df.columns if c != "label"]
    X = df[feature_columns].values
    y = df["label"].values

    if do_smote:
        sm = SMOTE(random_state=78)
        X_oversampled, y_oversampled = sm.fit_resample(X, y)
        X = X_oversampled
        y = y_oversampled

    if split == "train":
        clf = get_clf(algorithm)
        scores = cross_validate(
            clf,
            X,
            y,
            cv=5,
            scoring=["roc_auc", "accuracy", "f1", "precision", "recall"],
            return_estimator=True,
            return_indices=True,
        )

        aucs = scores["test_roc_auc"].tolist()
        accuracies = scores["test_accuracy"].tolist()

        test_runs = [
            (est, X[indices], y[indices])
            for est, indices in zip(scores["estimator"], scores["indices"]["test"])
        ]

        (
            _,
            _,
            confusion_matrices,
            specificities,
            classification_reports,
            f1s,
            precisions,
            sensitivities,
            specificities,
            fprs,
            tprs,
            cuts,
        ) = evaluate(test_runs)


    else:
        clf = load(model_save_path)
        test_iterations = 200
        all_indices = list(range(X.shape[0]))

        sampled_indices = [
            resample(all_indices, n_samples=X.shape[0], random_state=i)
            for i, _ in enumerate(range(test_iterations))
        ]
        test_runs = [(clf, X[indices], y[indices]) for indices in sampled_indices]

        (
            aucs,
            accuracies,
            confusion_matrices,
            specificities,
            classification_reports,
            f1s,
            precisions,
            sensitivities,
            specificities,
            fprs,
            tprs,
            cuts,
        ) = evaluate(test_runs)

    #### Save model for external validation.
    important_features = None
    if split == "train":
        clf = get_clf(algorithm)
        clf.fit(X, y)
        dump(clf, model_save_path)
        if "ensemble" not in algorithm:
            importances = np.argsort(clf.feature_importances_)[::-1]
            important_features = [feature_columns[i] for i in importances]

    # Calculate additonal metrics.
    # fpr, tpr, cut = roc_curve(y, y_proba[:, 1])
    # conf_mat = confusion_matrix(y, preds)
    # report = classification_report(y, preds)

    entry = {
        "fprs": fprs,  # list of lists
        "tprs": tprs,  # list of lists
        "cuts": cuts,  # list of lists
        "confusion_matrices": confusion_matrices,  # list of lists
        "classification_reports": classification_reports,  # list of dicts
        "used_features": feature_columns,  # list of strings
        "aucs": aucs,  # float list
        "auc_mean": mean(aucs),  # float
        "auc_c90": confidence_interval(aucs, 0.90),  # float list
        "auc_c95": confidence_interval(aucs, 0.95),  # float list
        "auc_c99": confidence_interval(aucs, 0.99),  # float list
        "accuracies": accuracies,  # float list
        "accuracy_mean": mean(accuracies),  # float
        "accuracy_c90": confidence_interval(accuracies, 0.90),  # float list
        "accuracy_c95": confidence_interval(accuracies, 0.95),  # float list
        "accuracy_c99": confidence_interval(accuracies, 0.99),  # float list
        "f1s": f1s,  # float list
        "f1_mean": mean(f1s),  # float
        "f1_c90": confidence_interval(f1s, 0.90),  # float
        "f1_c95": confidence_interval(f1s, 0.95),  # float
        "f1_c99": confidence_interval(f1s, 0.99),  # float
        "precisions": precisions,  # float list
        "precision_mean": mean(precisions),  # float
        "precision_c90": confidence_interval(precisions, 0.90),  # float list
        "precision_c95": confidence_interval(precisions, 0.95),  # float list
        "precision_c99": confidence_interval(precisions, 0.99),  # float list
        "sensitivities": sensitivities,  # float list
        "sensitivity_mean": mean(sensitivities),  # float
        "sensitivity_c90": confidence_interval(sensitivities, 0.90),  # float list
        "sensitivity_c95": confidence_interval(sensitivities, 0.95),  # float list
        "sensitivity_c99": confidence_interval(sensitivities, 0.99),  # float list
        "specificities": specificities,  # float list
        "specificity_mean": mean(specificities),  # float
        "specificity_c90": confidence_interval(specificities, 0.90),  # float list
        "specificity_c95": confidence_interval(specificities, 0.95),  # float list
        "specificity_c99": confidence_interval(specificities, 0.99),  # float list
    }
    if important_features is not None:
        entry["important_features"] = important_features
    return entry


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-r", "--results-path", help="JSON Path to save results")
    parser.add_argument("-s", "--seed", help="Random seed", type=int)
    args = parser.parse_args()

    # Feature selection.
    feature_files = [
        ("cross", "train", "features/cross_train.csv"),
        ("cross", "test", "features/cross_test.csv"),
        ("longi", "internal", "features/longi_internal.csv"),
        ("longi", "external", "features/longi_external.csv"),
    ]
    smote = [False, True]
    algorithms = ["gbc", "xgb", "ensemble_soft"]

    results = {}
    for experiment_type, split, feature_file in feature_files:
        experiment_id = f"{experiment_type}_{split}"
        results[experiment_id] = {}

        print("======================", experiment_type, split, feature_file, sex)
        df = pd.read_csv(feature_file)

        if experiment_type == "cross":
            acoustic_features = acoustic_features_cross
        else:
            acoustic_features = acoustic_features_longi

        dfs = [
            (
                "all",
                df[
                    original_features
                    + acoustic_features
                    + linguistic_features
                    + ["label"]
                ],
            ),
            ("acoustic", df[acoustic_features + ["label"]]),
            ("linguistic", df[linguistic_features + ["label"]]),
        ]

        experiments = list(product(smote, algorithms))
        for do_smote, algorithm in experiments:
            print(f"------ SMOTE: {do_smote}, ALGORITHM: {algorithm}")

    if args.results_path is not None:
        with open(args.results_path, "w") as wf:
            json.dump(results, wf, indent=4, ensure_ascii=False)
