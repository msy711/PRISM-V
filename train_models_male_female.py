import argparse
import json
import random
from itertools import product

import pandas as pd
from tqdm import tqdm

from classify import run_experiment
from collect_features import demographic_features, original_features

random.seed(78)


def get_custom_features(prev_results, experiment_type, algorithm):
    if experiment_type == "cross" and algorithm == "gbc":
        acoustic_result = prev_results["cross_train_acoustic_20_gbc_smoteFalse"]
        linguistic_result = prev_results["cross_train_linguistic_21_gbc_smoteFalse"]
        compact_result = prev_results["cross_train_all_19_gbc_smoteFalse"]
    elif experiment_type == "cross" and algorithm == "xgb":
        acoustic_result = prev_results["cross_train_acoustic_11_xgb_smoteFalse"]
        linguistic_result = prev_results["cross_train_linguistic_15_xgb_smoteFalse"]
        compact_result = prev_results["cross_train_all_17_xgb_smoteFalse"]
    elif experiment_type == "longi" and algorithm == "gbc":
        acoustic_result = prev_results["longi_train_acoustic_7_gbc_smoteFalse"]
        linguistic_result = prev_results["longi_train_linguistic_21_gbc_smoteFalse"]
        compact_result = prev_results["longi_train_all_5_gbc_smoteFalse"]
    elif experiment_type == "longi" and algorithm == "xgb":
        acoustic_result = prev_results["longi_train_acoustic_6_xgb_smoteFalse"]
        linguistic_result = prev_results["longi_train_linguistic_12_xgb_smoteFalse"]
        compact_result = prev_results["longi_train_all_19_xgb_smoteFalse"]
    return (
        acoustic_result["used_features"],
        linguistic_result["used_features"],
        compact_result["used_features"],
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", "--prev-results", help="Previous results")
    parser.add_argument("-r", "--results-path", help="JSON Path to save results")
    args = parser.parse_args()

    with open(args.prev_results) as rf:
        prev_results = json.load(rf)

    # Feature selection.
    feature_files = [
        ("cross", "train", "features/cross_train.csv", "male"),
        ("cross", "test", "features/cross_test.csv", "male"),
        ("cross", "train", "features/cross_train.csv", "female"),
        ("cross", "test", "features/cross_test.csv", "female"),
        # ("longi", "train", "features/longi_internal.csv"),
        # ("longi", "test", "features/longi_external.csv"),
    ]
    smote = [False, True]
    algorithms = ["gbc", "xgb"]
    model_types = list(product(smote, algorithms))

    results = {}
    for experiment_type, split, feature_file, sex in feature_files:
        df = pd.read_csv(feature_file)
        if sex == "male":
            df = df[df["sex"] == 0]
        else:
            df = df[df["sex"] == 1]

        for do_smote, algorithm in model_types:
            (
                acoustic_features,
                linguistic_features,
                compact_features,
            ) = get_custom_features(prev_results, experiment_type, algorithm)
            dfs = [
                ("acoustic", acoustic_features + ["label"]),
                ("linguistic", linguistic_features + ["label"]),
                # ("longi", "train", "features/longi_internal.csv"),
                # ("longi", "test", "features/longi_external.csv"),
            ]
    smote = [False]
    algorithms = ["gbc", "xgb"]
    model_types = list(product(smote, algorithms))

    results = {}
    for experiment_type, split, feature_file, sex in feature_files:
        df = pd.read_csv(feature_file)
        if sex == "male":
            df = df[df["sex"] == 0]
        else:
            df = df[df["sex"] == 1]

        for do_smote, algorithm in model_types:
            (
                acoustic_features,
                linguistic_features,
                compact_features,
            ) = get_custom_features(prev_results, experiment_type, algorithm)
            dfs = [
                ("null", ["age", "suicide_hx"] + ["label"]),
                ("acoustic", acoustic_features + ["label"]),
                ("linguistic", linguistic_features + ["label"]),
                ("speech", acoustic_features + linguistic_features + ["label"]),
                (
                    "demographic+speech",
                    ["age"] + acoustic_features + linguistic_features + ["label"],
                ),
                ("compact", compact_features + ["label"]),
            ]

            for name, features in tqdm(dfs):
                name = f"{sex}_{name}"
                run_id = f"{experiment_type}_{split}_{name}_{algorithm}_smote{do_smote}"
                model_path = (
                    f"models/s_{experiment_type}_{name}_{algorithm}_smote{do_smote}"
                )
                res = run_experiment(
                    run_id, split, algorithm, df[features], do_smote, model_path
                )
                results[run_id] = res

    if args.results_path is not None:
        with open(args.results_path, "w") as wf:
            json.dump(results, wf, indent=4, ensure_ascii=False)
