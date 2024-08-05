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
    parser.add_argument("-r", "--results-path", help="Results path")
    args = parser.parse_args()

    # Feature selection.
    feature_files = [
        ("cross", "train", "features/cross_train.csv"),
        ("cross", "test", "features/cross_test.csv"),
        ("longi", "train", "features/longi_internal.csv"),
        ("longi", "test", "features/longi_external.csv"),
    ]
    smote = [False, True]
    algorithms = ["gbc", "xgb", "ensemble_soft"]
    model_types = list(product(smote, algorithms))

    results = {}
    for experiment_type, split, feature_file in feature_files:
        df = pd.read_csv(feature_file)

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

        for name, df in tqdm(dfs):
            for do_smote, algorithm in model_types:
                run_id = f"{experiment_type}_{split}_{name}_{algorithm}_smote{do_smote}"
                model_path = (
                    f"models/{experiment_type}_{name}_{algorithm}_smote{do_smote}"
                )
                res = run_experiment(run_id, split, algorithm, df, do_smote, model_path)

                results[run_id] = res

    if args.results_path is not None:
        with open(args.results_path, "w") as wf:
            json.dump(results, wf, indent=4, ensure_ascii=False)
