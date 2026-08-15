"""
Multivariable regression models for suicide-related outcomes:
  Model 1: lifetime suicide attempt (within MDD cases)
  Model 2: multiple (repeat) vs single attempt (within attempters)
  Model 3: current suicidal ideation (SCL item, full sample incl. controls)
Also: bivariate checks of PBI / social-support factor scores vs attempt.
"""
import pandas as pd
import numpy as np
import statsmodels.api as sm
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import roc_auc_score
import os

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
df = pd.read_pickle(os.path.join(OUT, "subject_level_with_factors.pkl"))

edu_map = {"None-primary": 0, "JuniorMiddle": 1, "Technical-Junior": 2,
           "SeniorMiddle": 2, "Bachelor": 4, "AboveBachelor": 5}
df["edu_level"] = df["education"].map(edu_map)

cases = df[df.case == 1].copy()

# ---------- bivariate: factor scores vs lifetime attempt ----------
print("=== Bivariate: psychosocial factor scores vs lifetime suicide attempt (MDD cases) ===")
fac_rows = []
for v in ["pbi_father_care","pbi_father_overprotect","pbi_mother_care","pbi_mother_overprotect",
          "social_conflict","social_support"]:
    d = cases[[v, "sattempt_lifetime"]].dropna()
    d[v] = (d[v] - d[v].mean()) / d[v].std()
    x = sm.add_constant(d[v])
    m = sm.Logit(d["sattempt_lifetime"], x).fit(disp=0)
    or_ = np.exp(m.params[v]); ci = np.exp(m.conf_int().loc[v]); p = m.pvalues[v]
    fac_rows.append(dict(var=v, n=len(d), OR_per_SD=or_, CI_low=ci[0], CI_high=ci[1], p=p))
fac_df = pd.DataFrame(fac_rows)
print(fac_df.to_string(index=False))
fac_df.to_csv(os.path.join(OUT, "factor_scores_vs_attempt.csv"), index=False)

# ---------- Model 1: multivariable logistic regression, lifetime attempt ----------
predictors = [
    "age", "edu_level", "age_onset", "n_episodes", "dsm_symptom_count",
    "melancholia_bin", "atypical_bin", "manic_bin", "gad_bin", "panic_bin",
    "Nscore_", "csa_any", "fh_dep_ratio", "fh_man_ratio", "SLEscore_",
    "pbi_father_care", "pbi_father_overprotect", "pbi_mother_care", "pbi_mother_overprotect",
    "social_conflict", "social_support",
]
marital_dum = pd.get_dummies(cases["marital"], prefix="marital", drop_first=True)
model_df = pd.concat([cases[predictors + ["sattempt_lifetime"]], marital_dum], axis=1).dropna()
print(f"\nModel 1 analytic n = {len(model_df)}")

X = model_df.drop(columns="sattempt_lifetime")
# standardize continuous predictors for comparable ORs
cont_vars = ["age","edu_level","age_onset","n_episodes","dsm_symptom_count","Nscore_",
             "fh_dep_ratio","fh_man_ratio","SLEscore_","pbi_father_care","pbi_father_overprotect",
             "pbi_mother_care","pbi_mother_overprotect","social_conflict","social_support"]
for c in cont_vars:
    X[c] = (X[c] - X[c].mean()) / X[c].std()
X = X.astype(float)
Xc = sm.add_constant(X)
y = model_df["sattempt_lifetime"].astype(float)

m1 = sm.Logit(y, Xc).fit(disp=0, maxiter=200)
res1 = pd.DataFrame({
    "OR": np.exp(m1.params), "CI_low": np.exp(m1.conf_int()[0]), "CI_high": np.exp(m1.conf_int()[1]),
    "p": m1.pvalues,
}).drop("const")
res1 = res1.sort_values("p")
print("\n=== Model 1: Multivariable predictors of lifetime suicide attempt (MDD cases) ===")
print(res1.round(4).to_string())
res1.to_csv(os.path.join(OUT, "model1_attempt_multivariable.csv"))

# 5-fold CV AUC
skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)
aucs = []
Xv = Xc.values; yv = y.values
for tr, te in skf.split(Xv, yv):
    try:
        mm = sm.Logit(yv[tr], Xv[tr]).fit(disp=0, maxiter=200)
        pred = mm.predict(Xv[te])
        aucs.append(roc_auc_score(yv[te], pred))
    except Exception as e:
        print("fold failed", e)
print(f"\nModel 1 5-fold CV AUC = {np.mean(aucs):.3f} (SD {np.std(aucs):.3f})")
with open(os.path.join(OUT, "model1_auc.txt"), "w") as f:
    f.write(f"5-fold CV AUC = {np.mean(aucs):.4f} (SD {np.std(aucs):.4f})\nfolds={aucs}\n")

# ---------- Model 2: repeat vs single attempt (among attempters) ----------
attempters = cases[cases.sattempt_lifetime == 1].copy()
model2_df = pd.concat([attempters[predictors + ["sattempt_multiple"]],
                        pd.get_dummies(attempters["marital"], prefix="marital", drop_first=True)], axis=1).dropna()
X2 = model2_df.drop(columns="sattempt_multiple")
for c in cont_vars:
    X2[c] = (X2[c] - X2[c].mean()) / X2[c].std()
X2 = X2.astype(float)
X2c = sm.add_constant(X2)
y2 = model2_df["sattempt_multiple"].astype(float)
print(f"\nModel 2 analytic n = {len(model2_df)}  (multiple-attempt rate = {y2.mean():.3f})")
m2 = sm.Logit(y2, X2c).fit(disp=0, maxiter=200)
res2 = pd.DataFrame({
    "OR": np.exp(m2.params), "CI_low": np.exp(m2.conf_int()[0]), "CI_high": np.exp(m2.conf_int()[1]),
    "p": m2.pvalues,
}).drop("const").sort_values("p")
print("\n=== Model 2: predictors of repeat (vs single) suicide attempt ===")
print(res2.round(4).to_string())
res2.to_csv(os.path.join(OUT, "model2_repeat_attempt.csv"))

# ---------- Model 3: current suicidal ideation, full sample (case+control) ----------
full_predictors = ["case", "age", "edu_level", "Nscore_", "csa_any",
                    "pbi_father_care", "pbi_father_overprotect", "pbi_mother_care", "pbi_mother_overprotect",
                    "social_conflict", "social_support"]
model3_df = df[full_predictors + ["si_current_any"]].dropna()
X3 = model3_df.drop(columns="si_current_any")
cont3 = ["age","edu_level","Nscore_","pbi_father_care","pbi_father_overprotect",
         "pbi_mother_care","pbi_mother_overprotect","social_conflict","social_support"]
for c in cont3:
    X3[c] = (X3[c] - X3[c].mean()) / X3[c].std()
X3 = X3.astype(float)
X3c = sm.add_constant(X3)
y3 = model3_df["si_current_any"].astype(float)
print(f"\nModel 3 analytic n = {len(model3_df)}  (full sample, case+control)")
m3 = sm.Logit(y3, X3c).fit(disp=0, maxiter=200)
res3 = pd.DataFrame({
    "OR": np.exp(m3.params), "CI_low": np.exp(m3.conf_int()[0]), "CI_high": np.exp(m3.conf_int()[1]),
    "p": m3.pvalues,
}).drop("const").sort_values("p")
print("\n=== Model 3: predictors of CURRENT suicidal ideation (full sample) ===")
print(res3.round(4).to_string())
res3.to_csv(os.path.join(OUT, "model3_current_si_fullsample.csv"))

df.to_pickle(os.path.join(OUT, "subject_level_with_factors.pkl"))
print("\nDone.")
