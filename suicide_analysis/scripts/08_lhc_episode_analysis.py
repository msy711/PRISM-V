"""
Episode-level (Life History Calendar) analysis, linked to subject-level
suicide-attempt data by study_id, for the 2998 MDD cases present in both files.

1) Identifies, for each lifetime attempter, which depressive episode temporally
   contains their first suicide attempt, and compares the stressful-life-event
   severity of that "attempt episode" against the person's other episodes
   (within-subject paired Wilcoxon test) -- i.e. is the attempt-linked episode
   more severely stress-provoked than a person's typical episode?
2) Tests the 'kindling' pattern: does the proportion of episodes that occur
   'out of the blue' (without a preceding severe stressor) rise with episode
   number / recurrence, and is kindling status related to attempt history?
"""
import pandas as pd
import numpy as np
from scipy import stats
import os

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
lhc = pd.read_pickle(os.path.join(OUT, "episode_level_clean.pkl"))
df = pd.read_pickle(os.path.join(OUT, "subject_level_with_factors.pkl"))
cases = df[df.case == 1].copy()

lhc = lhc.merge(cases[["study_id", "sattempt_lifetime", "dep.suicide.first.age", "dep.suicide.number"]],
                 on="study_id", how="inner")
print("Linked episode rows:", len(lhc), "subjects:", lhc.study_id.nunique())

lhc["ep_end_age"] = lhc["episode_age"] + lhc["episode_duration_weeks"].fillna(0) / 52.0

# ---- 1) attempt-episode identification & severity comparison ----
attempters = lhc[lhc.sattempt_lifetime == 1].dropna(subset=["dep.suicide.first.age"])
rows = []
for sid, g in attempters.groupby("study_id"):
    A = g["dep.suicide.first.age"].iloc[0]
    g = g.sort_values("episode_number")
    contains = g[(g.episode_age <= A) & (g.ep_end_age >= A)]
    if len(contains) >= 1:
        target_idx = contains.index[0]
    else:
        target_idx = (g["episode_age"] - A).abs().idxmin()
    lhc.loc[target_idx, "is_attempt_episode"] = 1

lhc["is_attempt_episode"] = lhc["is_attempt_episode"].fillna(0)
print("Episodes flagged as attempt-linked:", int(lhc["is_attempt_episode"].sum()))

sev_vars = ["severity_composite", "sle_count", "or_max", "n_dependent", "n_independent"]
paired_rows = []
for var in sev_vars:
    diffs = []
    for sid, g in lhc.groupby("study_id"):
        att = g.loc[g.is_attempt_episode == 1, var]
        oth = g.loc[g.is_attempt_episode == 0, var]
        if len(att) == 1 and len(oth) >= 1 and att.notna().all() and oth.notna().any():
            diffs.append(att.values[0] - oth.mean())
    diffs = pd.Series(diffs).dropna()
    if len(diffs) > 10:
        stat, p = stats.wilcoxon(diffs)
        paired_rows.append(dict(var=var, n_pairs=len(diffs), mean_diff=diffs.mean(),
                                 median_diff=diffs.median(), wilcoxon_p=p))
paired_df = pd.DataFrame(paired_rows)
paired_df.to_csv(os.path.join(OUT, "lhc_attempt_episode_severity.csv"), index=False)
print("\n=== Attempt-episode vs subject's other episodes (paired) ===")
print(paired_df.to_string(index=False))

# ---- 2) kindling: out-of-blue proportion by episode number ----
kind = lhc.dropna(subset=["out_of_blue"]).groupby("episode_number")["out_of_blue"].agg(["mean", "count"])
kind.to_csv(os.path.join(OUT, "lhc_kindling_by_episode_number.csv"))
print("\n=== Proportion of 'out of the blue' (non-event-provoked) episodes by episode number ===")
print(kind)

# trend test (logistic regression of out_of_blue on episode_number, robust SE clustered by subject)
import statsmodels.api as sm
import statsmodels.formula.api as smf
kdat = lhc.dropna(subset=["out_of_blue", "episode_number"]).copy()
mod = smf.logit("out_of_blue ~ episode_number", data=kdat).fit(disp=0,
      cov_type="cluster", cov_kwds={"groups": kdat["study_id"]})
print(mod.summary().tables[1])
with open(os.path.join(OUT, "lhc_kindling_trend.txt"), "w") as f:
    f.write(str(mod.summary()))

# subject-level: max episode number reached & whether ever attempted vs kindling proportion
subj_kind = lhc.groupby("study_id").agg(
    n_ep=("episode_number", "max"),
    prop_oob=("out_of_blue", "mean"),
    sattempt=("sattempt_lifetime", "first"),
).dropna()
g0 = subj_kind.loc[subj_kind.sattempt == 0, "prop_oob"]
g1 = subj_kind.loc[subj_kind.sattempt == 1, "prop_oob"]
t, p = stats.ttest_ind(g1, g0, equal_var=False)
print(f"\nProportion of out-of-blue episodes: attempters mean={g1.mean():.3f} (n={len(g1)}), "
      f"non-attempters mean={g0.mean():.3f} (n={len(g0)}), t={t:.2f}, p={p:.4g}")
with open(os.path.join(OUT, "lhc_oob_attempt_compare.txt"), "w") as f:
    f.write(f"attempters mean={g1.mean():.4f} n={len(g1)}\nnon-attempters mean={g0.mean():.4f} n={len(g0)}\nt={t:.3f} p={p:.4g}\n")

# subject-level mean severity_composite: attempters vs non
subj_sev = lhc.groupby("study_id").agg(
    mean_sev=("severity_composite", "mean"), sattempt=("sattempt_lifetime", "first")
).dropna()
gs0 = subj_sev.loc[subj_sev.sattempt == 0, "mean_sev"]
gs1 = subj_sev.loc[subj_sev.sattempt == 1, "mean_sev"]
t2, p2 = stats.ttest_ind(gs1, gs0, equal_var=False)
print(f"\nMean per-episode severity_composite: attempters={gs1.mean():.3f} (n={len(gs1)}), "
      f"non-attempters={gs0.mean():.3f} (n={len(gs0)}), t={t2:.2f}, p={p2:.4g}")
with open(os.path.join(OUT, "lhc_severity_attempt_compare.txt"), "w") as f:
    f.write(f"attempters mean={gs1.mean():.4f} n={len(gs1)}\nnon-attempters mean={gs0.mean():.4f} n={len(gs0)}\nt={t2:.3f} p={p2:.4g}\n")

print("\nDone.")
