"""
Risk factors for suicide-attempt LETHALITY, among lifetime attempters (n~=800):
  Model A: ordinal regression on physical-damage severity (5 levels, proportional-odds)
  Model B: logistic regression on high- vs low-lethality method (hanging/jumping vs
           overdose/other -- a standard method-lethality dichotomy in the suicide literature)
"""
import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.miscmodels.ordinal_model import OrderedModel
import os

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
df = pd.read_pickle(os.path.join(OUT, "subject_level_with_factors.pkl"))
att = df[(df.case == 1) & (df.sattempt_lifetime == 1)].copy()
print("Attempters:", len(att))

att["high_lethality_method"] = att["dep.suicide.lethal"].map({
    "Hanging": 1, "Jumping": 1, "Overdose": 0, "Other": 0
})
print(att["dep.suicide.lethal"].value_counts(dropna=False))
print(att["suicide_damage_ord"].value_counts(dropna=False))

predictors = [
    "dep.suicide.first.age", "edu_level" if "edu_level" in att.columns else None,
    "melancholia_bin", "atypical_bin", "manic_bin", "gad_bin", "panic_bin", "psy_inpatient",
    "Nscore_", "csa_any", "fh_dep_ratio", "fh_man_ratio", "SLEscore_", "dsm_symptom_count",
    "n_episodes", "sattempt_multiple",
]
predictors = [p for p in predictors if p]
if "edu_level" not in att.columns:
    edu_map = {"None-primary": 0, "JuniorMiddle": 1, "Technical-Junior": 2, "SeniorMiddle": 2,
               "Bachelor": 4, "AboveBachelor": 5}
    att["edu_level"] = att["education"].map(edu_map)

label_map = {
    "dep.suicide.first.age": "첫 시도 연령", "edu_level": "교육 수준", "melancholia_bin": "멜랑콜리아 아형",
    "atypical_bin": "비정형 증상", "manic_bin": "조증 증상 동반", "gad_bin": "범불안장애 동반",
    "panic_bin": "공황장애 동반", "psy_inpatient": "정신과 입원력", "Nscore_": "신경증 성향",
    "csa_any": "아동기 성적학대", "fh_dep_ratio": "우울증 가족력 비율", "fh_man_ratio": "조증 가족력 비율",
    "SLEscore_": "생애 스트레스 사건 총점", "dsm_symptom_count": "우울 증상 개수", "n_episodes": "삽화 횟수",
    "sattempt_multiple": "반복 시도(2회 이상)",
}

def std(x):
    return (x - x.mean()) / x.std()

# ================= Model A: ordinal damage severity =================
dA = att[predictors + ["suicide_damage_ord"]].dropna()
print(f"\nModel A (ordinal damage severity) n={len(dA)}")
XA = dA[predictors].copy()
for c in predictors:
    XA[c] = std(XA[c])
yA = dA["suicide_damage_ord"].astype(int)
modA = OrderedModel(yA, XA, distr="logit")
resA = modA.fit(method="bfgs", disp=False, maxiter=300)
print(resA.summary())

paramsA = resA.params[predictors]
# approximate CIs
bse = resA.bse[predictors]
ci_lo = paramsA - 1.96 * bse
ci_hi = paramsA + 1.96 * bse
pvals = resA.pvalues[predictors]
outA = pd.DataFrame({
    "OR": np.exp(paramsA), "CI_low": np.exp(ci_lo), "CI_high": np.exp(ci_hi), "p": pvals
}).sort_values("p")
outA.index = [label_map.get(i, i) for i in outA.index]
outA.to_csv(os.path.join(OUT, "lethality_damage_ordinal.csv"))
print("\n=== Model A results (OR = odds of higher damage-severity category, per SD) ===")
print(outA.round(4).to_string())

# ================= Model B: high-lethality method (logistic) =================
dB = att[predictors + ["high_lethality_method"]].dropna()
print(f"\nModel B (high-lethality method) n={len(dB)}, rate={dB.high_lethality_method.mean():.3f}")
XB = dB[predictors].copy()
for c in predictors:
    XB[c] = std(XB[c])
XBc = sm.add_constant(XB)
yB = dB["high_lethality_method"].astype(float)
modB = sm.Logit(yB, XBc).fit(disp=0, maxiter=300)
outB = pd.DataFrame({
    "OR": np.exp(modB.params), "CI_low": np.exp(modB.conf_int()[0]), "CI_high": np.exp(modB.conf_int()[1]),
    "p": modB.pvalues,
}).drop("const").sort_values("p")
outB.index = [label_map.get(i, i) for i in outB.index]
outB.to_csv(os.path.join(OUT, "lethality_method_logistic.csv"))
print("\n=== Model B results (OR of using a high-lethality method: hanging/jumping vs overdose/other) ===")
print(outB.round(4).to_string())

print("\nDone.")
