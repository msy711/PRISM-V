"""
Exploratory factor analysis of:
  (a) Parental Bonding (father & mother, 16 items each) -> Care / Overprotection
  (b) Social network quality (friends & relatives, 14 items) -> Support / Conflict
Saves factor loadings, fit indices, and per-subject factor scores that are
later used as predictors of suicidality.
"""
import pandas as pd
import numpy as np
import os
from factor_analyzer import FactorAnalyzer
from factor_analyzer.factor_analyzer import calculate_kmo, calculate_bartlett_sphericity

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
df = pd.read_pickle(os.path.join(OUT, "subject_level_clean.pkl"))

def run_efa(data, cols, n_factors, label, rotation="oblimin"):
    d = data[cols].dropna()
    kmo_all, kmo_model = calculate_kmo(d)
    chi2, p = calculate_bartlett_sphericity(d)
    fa = FactorAnalyzer(n_factors=n_factors, rotation=rotation, method="minres")
    fa.fit(d)
    loadings = pd.DataFrame(fa.loadings_, index=cols, columns=[f"F{i+1}" for i in range(n_factors)])
    var = fa.get_factor_variance()
    var_df = pd.DataFrame(var, index=["SS_loadings", "Prop_var", "Cum_var"],
                           columns=[f"F{i+1}" for i in range(n_factors)])
    print(f"\n=== {label}: KMO={kmo_model:.3f}, Bartlett chi2={chi2:.1f} (p={p:.2g}), n={len(d)} ===")
    print(loadings.round(3))
    print(var_df.round(3))
    loadings.to_csv(os.path.join(OUT, f"efa_loadings_{label}.csv"))
    var_df.to_csv(os.path.join(OUT, f"efa_variance_{label}.csv"))
    scores = pd.DataFrame(fa.transform(d), index=d.index, columns=[f"{label}_F{i+1}" for i in range(n_factors)])
    return loadings, scores, dict(kmo=kmo_model, bartlett_chi2=chi2, bartlett_p=p, n=len(d))

pbi_father = [c for c in df.columns if c.startswith("pbi.father")]
pbi_mother = [c for c in df.columns if c.startswith("pbi.mother")]
sl_support = ['sl.fr.care.frq','sl.fr.xs.demand.frq','sl.fr.criticize.frq','sl.fr.interest.frq',
              'sl.fr.arguments.frq','sl.rel.care.frq','sl.rel.xs.demand.frq','sl.rel.criticize.frq',
              'sl.rel.interest.frq','sl.rel.arguments.frq']

fit_summary = {}
loadF, scoreF, fitF = run_efa(df, pbi_father, 2, "pbi_father")
loadM, scoreM, fitM = run_efa(df, pbi_mother, 2, "pbi_mother")
loadS, scoreS, fitS = run_efa(df, sl_support, 2, "social_support")
fit_summary["pbi_father"] = fitF
fit_summary["pbi_mother"] = fitM
fit_summary["social_support"] = fitS

# merge factor scores back to main df (by index)
df = df.join(scoreF).join(scoreM).join(scoreS)

# Orient factors by sign of key marker items so that higher score = more of the
# named construct (Care, Overprotection, Support)
def orient(df_, score_col, loadings, marker_item, factor_col):
    if loadings.loc[marker_item, factor_col] < 0:
        df_[score_col] = -df_[score_col]
    return df_

# father: F with high loading on 'warm' = Care; other = Overprotection (check sign via 'control')
print("\nFather loadings (for orientation):\n", loadF.round(2))
print("\nMother loadings (for orientation):\n", loadM.round(2))
print("\nSupport loadings (for orientation):\n", loadS.round(2))

fit_df = pd.DataFrame(fit_summary).T
fit_df.to_csv(os.path.join(OUT, "efa_fit_summary.csv"))

# All marker items already load positively on their labelled factor -> no sign flips needed.
df = df.rename(columns={
    "pbi_father_F1": "pbi_father_care", "pbi_father_F2": "pbi_father_overprotect",
    "pbi_mother_F1": "pbi_mother_care", "pbi_mother_F2": "pbi_mother_overprotect",
    "social_support_F1": "social_conflict", "social_support_F2": "social_support",
})

# Cronbach's alpha for each factor's high-loading items (|loading|>=0.4), for reference
import pingouin as pg
def alpha_for(loadings, factor, items, thresh=0.4):
    hi = loadings.index[loadings[factor].abs() >= thresh].tolist()
    d = df[hi].dropna().copy()
    for it in hi:
        if loadings.loc[it, factor] < 0:
            d[it] = (d[it].max() + d[it].min()) - d[it]  # reverse score
    a = pg.cronbach_alpha(data=d)
    return hi, a

rel_rows = []
for label, loadings in [("pbi_father_care", loadF), ("pbi_father_overprotect", loadF),
                         ("pbi_mother_care", loadM), ("pbi_mother_overprotect", loadM),
                         ("social_conflict", loadS), ("social_support", loadS)]:
    fcol = "F1" if label in ("pbi_father_care","pbi_mother_care","social_conflict") else "F2"
    hi, a = alpha_for(loadings, fcol, None)
    rel_rows.append(dict(factor=label, n_items=len(hi), alpha=a[0], ci=str(a[1])))
    print(label, "items:", hi, "alpha:", round(a[0],3))
pd.DataFrame(rel_rows).to_csv(os.path.join(OUT, "efa_reliability.csv"), index=False)

df.to_pickle(os.path.join(OUT, "subject_level_with_factors.pkl"))
print("\nSaved subject_level_with_factors.pkl with columns:",
      [c for c in df.columns if c.startswith(("pbi_father","pbi_mother","social_"))])
