import argparse
import json
import random
from itertools import product
from pathlib import Path

import numpy as np
import pandas as pd
import xgboost as xgb
from imblearn.over_sampling import SMOTE
from joblib import dump, load
from sklearn.ensemble import (GradientBoostingClassifier, StackingClassifier,
                              VotingClassifier)
from sklearn.feature_selection import SelectFromModel
from sklearn.metrics import (accuracy_score, classification_report,
                             confusion_matrix, roc_auc_score, roc_curve)
from sklearn.model_selection import cross_val_predict, cross_validate
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
            scoring=["roc_auc", "accuracy"],
            return_estimator=True,
        )
        y_proba = cross_val_predict(clf, X, y, cv=5, method="predict_proba")
        preds = np.argmax(y_proba, axis=1)
        auc = sum(scores["test_roc_auc"]) / len(scores["test_roc_auc"])
        accuracy = sum(scores["test_accuracy"]) / len(scores["test_accuracy"])
    else:
        clf = load(model_save_path)
        y_proba = clf.predict_proba(X)
        preds = clf.predict(X)
        auc = roc_auc_score(y, preds)
        accuracy = accuracy_score(y, preds)

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
    fpr, tpr, cut = roc_curve(y, y_proba[:, 1])
    conf_mat = confusion_matrix(y, preds)
    report = classification_report(y, preds)

    entry = {
        "auc": auc,
        "accuracy": accuracy,
        "fpr": fpr.tolist(),
        "tpr": tpr.tolist(),
        "cut": cut.tolist(),
        "confusion_matrix": conf_mat.tolist(),
        "classification_report": report,
        "used_features": feature_columns,
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
