"""
Time-to-first-suicide-attempt analysis (from depression onset), among MDD cases.
Kaplan-Meier estimator (hand-implemented; event = first lifetime suicide attempt,
time origin = age of depression onset, censoring = current age for non-attempters),
with log-rank comparisons across key risk strata (childhood sexual abuse, family
history of mood disorder, melancholic subtype, early vs later illness onset).
"""
import pandas as pd
import numpy as np
import os

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
df = pd.read_pickle(os.path.join(OUT, "subject_level_with_factors.pkl"))
cases = df[df.case == 1].copy()

cases["time"] = np.where(cases["sattempt_lifetime"] == 1,
                          cases["dep.suicide.first.age"] - cases["age_onset"],
                          cases["age"] - cases["age_onset"])
cases["event"] = cases["sattempt_lifetime"].fillna(0)
cases = cases[(cases["time"] >= 0) & cases["time"].notna()]
print("Analytic n for time-to-attempt =", len(cases))
print("Events (attempts):", int(cases["event"].sum()))

def km_estimator(time, event):
    df_ = pd.DataFrame({"time": time, "event": event}).sort_values("time")
    times = sorted(df_.loc[df_.event == 1, "time"].unique())
    n_at_risk = len(df_)
    s = 1.0
    out = [(0, 1.0, n_at_risk)]
    for t in times:
        n_risk = (df_["time"] >= t).sum()
        d = ((df_["time"] == t) & (df_["event"] == 1)).sum()
        s *= (1 - d / n_risk)
        out.append((t, s, n_risk))
    return pd.DataFrame(out, columns=["time", "survival", "n_risk"])

def logrank_test(t1, e1, t2, e2):
    df1 = pd.DataFrame({"time": t1, "event": e1, "grp": 1})
    df2 = pd.DataFrame({"time": t2, "event": e2, "grp": 2})
    d = pd.concat([df1, df2])
    times = sorted(d.loc[d.event == 1, "time"].unique())
    O1 = E1 = V = 0.0
    for t in times:
        at_risk = d[d.time >= t]
        n = len(at_risk); n1 = (at_risk.grp == 1).sum(); n2 = n - n1
        events = at_risk[at_risk.time == t]
        o = (events.event == 1).sum()
        o1 = ((events.event == 1) & (events.grp == 1)).sum()
        if n > 1:
            e1_ = o * n1 / n
            v = o * (n1 / n) * (n2 / n) * (n - o) / (n - 1)
        else:
            e1_ = 0; v = 0
        O1 += o1; E1 += e1_; V += v
    chi2 = (O1 - E1) ** 2 / V if V > 0 else np.nan
    from scipy.stats import chi2 as chi2dist
    p = 1 - chi2dist.cdf(chi2, df=1)
    return chi2, p

overall_km = km_estimator(cases["time"], cases["event"])
overall_km.to_csv(os.path.join(OUT, "km_overall.csv"), index=False)
print("\nMedian/IQR years from onset to first attempt (attempters only):")
print(cases.loc[cases.event == 1, "time"].describe())
print("\n% of attempters whose first attempt occurred within 1 year of onset:",
      (cases.loc[cases.event == 1, "time"] <= 1).mean().round(3))
print("within 5 years:", (cases.loc[cases.event == 1, "time"] <= 5).mean().round(3))

strata_defs = {
    "CSA (any vs none)": ("csa_any", {0: "No CSA", 1: "CSA"}),
    "Family history of mood disorder": (None, None),  # handled separately (continuous median split)
    "Melancholic subtype": ("melancholia_bin", {0: "No", 1: "Yes"}),
    "Early onset (<=25) vs later": (None, None),
}

results_summary = []

def compare(cases_, groupvar, labels, name):
    sub = cases_.dropna(subset=[groupvar])
    groups = sorted(sub[groupvar].unique())
    if len(groups) != 2:
        return
    g0 = sub[sub[groupvar] == groups[0]]
    g1 = sub[sub[groupvar] == groups[1]]
    chi2, p = logrank_test(g0["time"], g0["event"], g1["time"], g1["event"])
    km0 = km_estimator(g0["time"], g0["event"]); km0["group"] = labels[groups[0]]
    km1 = km_estimator(g1["time"], g1["event"]); km1["group"] = labels[groups[1]]
    combined = pd.concat([km0, km1])
    fname = name.replace(" ", "_").replace("(", "").replace(")", "").replace("/", "-")
    combined.to_csv(os.path.join(OUT, f"km_{fname}.csv"), index=False)
    rate0 = g0["event"].mean(); rate1 = g1["event"].mean()
    print(f"\n[{name}] logrank chi2={chi2:.2f}, p={p:.4g}; "
          f"attempt-rate {labels[groups[0]]}={rate0:.3f} (n={len(g0)}), "
          f"{labels[groups[1]]}={rate1:.3f} (n={len(g1)})")
    results_summary.append(dict(stratum=name, chi2=chi2, p=p,
                                 group0=labels[groups[0]], n0=len(g0), rate0=rate0,
                                 group1=labels[groups[1]], n1=len(g1), rate1=rate1))

compare(cases, "csa_any", {0: "No CSA", 1: "CSA"}, "CSA (any vs none)")
compare(cases, "melancholia_bin", {0: "No", 1: "Yes"}, "Melancholic subtype")

cases["fh_dep_any_bin"] = (cases["fh_dep_ratio"] > 0).astype(float)
compare(cases, "fh_dep_any_bin", {0: "No FH", 1: "FH+"}, "Family history of depression")

cases["early_onset"] = (cases["age_onset"] <= 25).astype(float)
compare(cases, "early_onset", {0: "Onset >25", 1: "Onset <=25"}, "Early (<=25) vs later onset")

pd.DataFrame(results_summary).to_csv(os.path.join(OUT, "km_logrank_summary.csv"), index=False)
print("\nDone.")
