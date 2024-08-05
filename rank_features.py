import argparse
import json
from itertools import product

import pandas as pd
from tqdm import tqdm

from classify import run_experiment
from collect_features import (acoustic_features, acoustic_features_cross,
                              acoustic_features_longi, linguistic_features,
                              original_features)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", "--prev-results", help="Previous results")
    parser.add_argument("-r", "--results-path", help="JSON Path to save results")
    args = parser.parse_args()

    with open(args.prev_results) as rf:
        prev_results = json.load(rf)

    # Feature selection.
    feature_files = [
        ("cross", "train", "features/cross_train.csv"),
        ("cross", "test", "features/cross_test.csv"),
        ("longi", "train", "features/longi_internal.csv"),
        ("longi", "test", "features/longi_external.csv"),
    ]
    smote = [False, True]
    algorithms = ["gbc", "xgb"]
    model_types = list(product(smote, algorithms))

    candidate_features = {
        "all": original_features + acoustic_features + linguistic_features + ["label"],
        "acoustic": acoustic_features,
        "linguistic": linguistic_features,
    }

    results = {}
    for experiment_type, split, feature_file in feature_files:
        df = pd.read_csv(feature_file)

        for feature_type in ["all", "acoustic", "linguistic"]:
            for do_smote, algorithm in model_types:
                prev_run_id = f"{experiment_type}_{split}_{feature_type}_{algorithm}_smote{do_smote}"
                prev = prev_results[prev_run_id.replace("test", "train")]
                important_features = prev["important_features"]
                dfs = [
                    (f"{feature_type}_{i}", df[important_features[:i] + ["label"]])
                    for i in range(1, min(len(important_features), 101))
                ]

                for name, features in tqdm(dfs):
                    run_id = (
                        f"{experiment_type}_{split}_{name}_{algorithm}_smote{do_smote}"
                    )
                    model_path = f"models/rf_{experiment_type}_{name}_{algorithm}_smote{do_smote}"
                    res = run_experiment(
                        run_id, split, algorithm, features, do_smote, model_path
                    )
                    results[run_id] = res

    if args.results_path is not None:
        with open(args.results_path, "w") as wf:
            json.dump(results, wf, indent=4, ensure_ascii=False)
