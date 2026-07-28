"""Blom rank-based inverse normal transform + MixedLM analysis.

For each acoustic feature and each speaker role (client / counselor):
    1. Rank-transform the feature with Blom's formula and map to the
       standard normal quantile (rank-based inverse normal transformation).
    2. Fit y ~ cond + session + (1|initials) with statsmodels MixedLM,
       where:
         - cond   is coded 0 = f2f, 1 = t2t, so a positive beta means
                  t2t is higher than f2f.
         - session is coded 0 = session 1, 1 = session 2, so a positive
                  beta means session 2 is higher than session 1.
         - initials (person id) is the random-intercept grouping variable.
    3. Collect Estimate / SE / z / p for each term across all features,
       and add a Benjamini-Hochberg adjusted p-value per term.
"""
import argparse
import warnings

import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
from scipy.stats import norm
from statsmodels.stats.multitest import multipletests

META_COLS = [
    "stem", "initials", "session", "cond", "group", "speaker",
    "speech_sec", "n_seg",
]


def blom_transform(x: pd.Series) -> pd.Series:
    """Rank-based inverse normal (Blom) transformation."""
    ranks = x.rank(method="average")
    n = len(x)
    return pd.Series(norm.ppf((ranks - 3 / 8) / (n + 1 / 4)), index=x.index)


def run_mixedlm_for_feature(data: pd.DataFrame, feature: str) -> dict:
    y = blom_transform(data[feature])
    model_df = pd.DataFrame({
        "y": y,
        "cond_num": data["cond_num"],
        "session_num": data["session_num"],
        "initials": data["initials"],
    })

    row = {"feature": feature}
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            fit = smf.mixedlm(
                "y ~ cond_num + session_num",
                model_df,
                groups=model_df["initials"],
            ).fit()

        for term, label in [("cond_num", "cond"), ("session_num", "session")]:
            row[f"{label}_beta"] = fit.params[term]
            row[f"{label}_se"] = fit.bse[term]
            row[f"{label}_z"] = fit.tvalues[term]
            row[f"{label}_p"] = fit.pvalues[term]
        row["converged"] = fit.converged
        row["error"] = ""
    except Exception as exc:  # noqa: BLE001
        for label in ["cond", "session"]:
            row[f"{label}_beta"] = np.nan
            row[f"{label}_se"] = np.nan
            row[f"{label}_z"] = np.nan
            row[f"{label}_p"] = np.nan
        row["converged"] = False
        row["error"] = str(exc)

    return row


def run_speaker_group(df: pd.DataFrame, speaker: str) -> pd.DataFrame:
    data = df[df["speaker"] == speaker].copy()
    data["cond_num"] = (data["cond"] == "t2t").astype(int)
    data["session_num"] = (data["session"] == 2).astype(int)

    feature_cols = [c for c in df.columns if c not in META_COLS]

    rows = [run_mixedlm_for_feature(data, feature) for feature in feature_cols]
    results = pd.DataFrame(rows)

    for label in ["cond", "session"]:
        p = results[f"{label}_p"]
        valid = p.notna()
        adj = np.full(len(p), np.nan)
        if valid.sum() > 0:
            adj[valid.values] = multipletests(p[valid], method="fdr_bh")[1]
        results[f"{label}_p_bh"] = adj

    return results


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input-csv", required=True, help="Input CSV path (e.g. compare_by_speaker.csv)")
    parser.add_argument("-o", "--output-prefix", required=True, help="Prefix for output CSV files, one per speaker role")
    args = parser.parse_args()

    df = pd.read_csv(args.input_csv)

    for speaker in sorted(df["speaker"].unique()):
        results = run_speaker_group(df, speaker)
        out_path = f"{args.output_prefix}_{speaker}.csv"
        results.to_csv(out_path, index=False)
        print(f"[{speaker}] wrote {len(results)} feature results to {out_path}")
