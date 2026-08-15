"""
Univariate screen: within MDD cases (n=3000), compare subjects with vs without
a lifetime suicide attempt across a broad panel of candidate risk factors drawn
from the literature (clinical severity, comorbidity, personality/neuroticism,
childhood adversity, parental bonding proxies, social support, family history,
life events). Produces an OR / p-value table used to select variables for the
multivariable model, and a case-vs-control demographic table.
"""
import pandas as pd
import numpy as np
from scipy import stats
import statsmodels.api as sm
import os

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
df = pd.read_pickle(os.path.join(OUT, "subject_level_clean.pkl"))
cases = df[df.case == 1].copy()

def uni_or(data, outcome, var):
    d = data[[outcome, var]].dropna()
    if d[var].nunique() < 2 or d[outcome].nunique() < 2:
        return None
    x = sm.add_constant(d[var].astype(float))
    try:
        model = sm.Logit(d[outcome].astype(float), x).fit(disp=0)
        or_ = np.exp(model.params[var])
        ci_lo, ci_hi = np.exp(model.conf_int().loc[var])
        p = model.pvalues[var]
        return dict(var=var, n=len(d), OR=or_, CI_low=ci_lo, CI_high=ci_hi, p=p)
    except Exception as e:
        return dict(var=var, n=len(d), OR=np.nan, CI_low=np.nan, CI_high=np.nan, p=np.nan, error=str(e))

candidates = [
    "age", "age_onset", "illness_duration", "n_episodes", "dsm_symptom_count",
    "atypical_bin", "manic_bin", "psychotic_bin", "melancholia_bin", "dysthymia_bin",
    "gad_bin", "panic_bin", "psy_inpatient",
    "Nscore_", "n_wished_dead",
    "csa_any",
    "fh_dep_ratio", "fh_man_ratio", "fh_dep_any", "fh_man_any",
    "SLEscore_", "SCLscore_",
]

rows = []
for v in candidates:
    r = uni_or(cases, "sattempt_lifetime", v)
    if r:
        rows.append(r)

screen = pd.DataFrame(rows).sort_values("p")
screen.to_csv(os.path.join(OUT, "univariate_screen_attempt.csv"), index=False)
print(screen.to_string(index=False))

# ---- categorical variables (chi-square) ----
cat_vars = ["education", "marital", "employment", "alcohol", "substance", "occupation"]
cat_rows = []
for v in cat_vars:
    d = cases[[v, "sattempt_lifetime"]].dropna()
    tab = pd.crosstab(d[v], d["sattempt_lifetime"])
    chi2, p, dof, exp = stats.chi2_contingency(tab)
    cat_rows.append(dict(var=v, n=len(d), chi2=chi2, dof=dof, p=p))
cat_screen = pd.DataFrame(cat_rows).sort_values("p")
cat_screen.to_csv(os.path.join(OUT, "univariate_screen_attempt_categorical.csv"), index=False)
print("\n--- categorical ---")
print(cat_screen.to_string(index=False))

# ---- MDD vs Control demographic/clinical comparison table ----
demo_rows = []
for v in ["age", "Nscore_", "SLEscore_", "csa_any"]:
    d0 = df.loc[df.case == 0, v].dropna()
    d1 = df.loc[df.case == 1, v].dropna()
    t, p = stats.ttest_ind(d1, d0, equal_var=False)
    demo_rows.append(dict(var=v, control_mean=d0.mean(), control_sd=d0.std(),
                           mdd_mean=d1.mean(), mdd_sd=d1.std(), t=t, p=p))
demo = pd.DataFrame(demo_rows)
demo.to_csv(os.path.join(OUT, "case_control_demo_compare.csv"), index=False)
print("\n--- case vs control ---")
print(demo.to_string(index=False))
