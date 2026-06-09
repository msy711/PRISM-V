"""
All liwc & F0 features (inverse-normal transformed) × HAMD/PHQ/BAI/BHOL
=========================================================================
Rank-based inverse normal transformation (Blom) applied per feature
before LME — equivalent to R's invnorm function.

invnorm: qnorm((rank - 0.5) / n)  [ties.method = 'average']

F0  model: outcome ~ feat_invnorm + time + age + sex + C(Dx) + AP_dose + (1|id)
LIWC model: outcome ~ feat_invnorm + time + age + sex + C(Dx) + edu_yrs + (1|id)

FDR correction: within each feature-group (F0/LIWC) × outcome cell.
"""

import pandas as pd
import numpy as np
from scipy.stats import rankdata, norm
import statsmodels.formula.api as smf
from statsmodels.stats.multitest import multipletests
import warnings, os, time
warnings.filterwarnings('ignore')

# ─────────────────────────────────────────────
# Inverse normal transformation (Blom, as in R)
# ─────────────────────────────────────────────
def invnorm(x):
    """Rank-based inverse normal transform. Returns N(0,1)-scaled values."""
    x = np.asarray(pd.to_numeric(x, errors='coerce'), dtype=float)
    ok = ~np.isnan(x)
    y  = np.full(len(x), np.nan)
    if ok.sum() <= 1:
        return y
    ranks  = rankdata(x[ok], method='average')
    y[ok]  = norm.ppf((ranks - 0.5) / ok.sum())
    return y

# ─────────────────────────────────────────────
# 1. Data
# ─────────────────────────────────────────────
df = pd.read_csv('data/prism_softImpute4_smile_with_items.csv', low_memory=False)
df['time_numeric'] = df['case_episode'].map(
    {'baseline': 0, '2m': 2, '4m': 4, '8m': 8, '12m': 12})
df['Dx'] = df['Dx'].astype(str)
for col in ['age', 'sex', 'edu_yrs', 'AP_dose', 'HAMD', 'PHQ', 'BAI', 'BHOL']:
    df[col] = pd.to_numeric(df[col], errors='coerce')

F0_FEATS   = [c for c in df.columns if c.startswith('F0final_sma')]
LIWC_FEATS = [c for c in df.columns if c.startswith('liwc_')]
for col in F0_FEATS + LIWC_FEATS:
    df[col] = pd.to_numeric(df[col], errors='coerce')

OUTCOMES = ['HAMD', 'PHQ', 'BAI', 'BHOL']
BASE     = 'time_numeric + age + sex + C(Dx, Treatment("BPII"))'

print(f"F0 features:   {len(F0_FEATS)}")
print(f"LIWC features: {len(LIWC_FEATS)}")
print(f"Total models:  {(len(F0_FEATS) + len(LIWC_FEATS)) * len(OUTCOMES)}")

# ─────────────────────────────────────────────
# 2. LME runner with invnorm on features
# ─────────────────────────────────────────────
def run_lme_invnorm(df, features, outcomes, cov_str):
    needed = (['id', 'time_numeric', 'age', 'sex', 'Dx', 'AP_dose', 'edu_yrs']
              + outcomes + features)
    sub = df[[c for c in needed if c in df.columns]].dropna().reset_index(drop=True)
    print(f"  Complete cases: {len(sub)} rows | {sub['id'].nunique()} patients")

    all_rows = []
    for outcome in outcomes:
        y_m, y_s = sub[outcome].mean(), sub[outcome].std()

        for feat in features:
            tmp = sub[['id', outcome, feat, 'time_numeric',
                        'age', 'sex', 'Dx', 'AP_dose', 'edu_yrs']].dropna().copy()

            # apply invnorm to feature
            inv = invnorm(tmp[feat].values)
            if np.nanstd(inv) == 0:
                continue
            tmp['feat_in'] = inv

            # z-score outcome
            tmp['Y'] = (tmp[outcome] - y_m) / y_s

            fitted = None
            for method in ['lbfgs', 'powell', 'nm']:
                try:
                    fitted = smf.mixedlm(
                        f'Y ~ feat_in + {cov_str}', tmp, groups=tmp['id']
                    ).fit(reml=True, method=method)
                    break
                except Exception:
                    continue

            if fitted is not None:
                all_rows.append({
                    'outcome': outcome,
                    'feature': feat,
                    'coef':    fitted.params['feat_in'],
                    'SE':      fitted.bse['feat_in'],
                    'CI_lo':   fitted.conf_int().loc['feat_in', 0],
                    'CI_hi':   fitted.conf_int().loc['feat_in', 1],
                    'p':       fitted.pvalues['feat_in'],
                    'n':       len(tmp),
                })

    res = pd.DataFrame(all_rows)
    for outcome in outcomes:
        mask = res['outcome'] == outcome
        if mask.sum() > 0:
            _, q, _, _ = multipletests(res.loc[mask, 'p'], method='fdr_bh')
            res.loc[mask, 'q_fdr'] = q
    return res

# ─────────────────────────────────────────────
# 3. Run
# ─────────────────────────────────────────────
t0 = time.time()
print("\n── F0 features (invnorm) ──")
res_f0 = run_lme_invnorm(df, F0_FEATS, OUTCOMES, BASE + ' + AP_dose')
res_f0['group'] = 'F0'
print(f"  Done in {time.time()-t0:.0f}s  |  {(res_f0['q_fdr']<0.05).sum()} FDR-sig cells")

t1 = time.time()
print("\n── LIWC features (invnorm) ──")
res_liwc = run_lme_invnorm(df, LIWC_FEATS, OUTCOMES, BASE + ' + edu_yrs')
res_liwc['group'] = 'LIWC'
print(f"  Done in {time.time()-t1:.0f}s  |  {(res_liwc['q_fdr']<0.05).sum()} FDR-sig cells")

# ─────────────────────────────────────────────
# 4. Save & summarise
# ─────────────────────────────────────────────
os.makedirs('results', exist_ok=True)
res_all = pd.concat([res_f0, res_liwc], ignore_index=True)
res_all.to_csv('results/feature_scale_lme_invnorm.csv', index=False)
print(f"\n✓ results/feature_scale_lme_invnorm.csv  ({len(res_all)} rows)")

print("\n── FDR sig counts per outcome × group ──")
for grp in ['F0', 'LIWC']:
    sub = res_all[res_all['group'] == grp]
    for outcome in OUTCOMES:
        n_fdr = ((sub['outcome'] == outcome) & (sub['q_fdr'] < 0.05)).sum()
        n_nom = ((sub['outcome'] == outcome) & (sub['p'] < 0.05)).sum()
        tot   = (sub['outcome'] == outcome).sum()
        print(f"  {grp:<5} × {outcome:<5}: {n_fdr}/{tot} FDR-sig  |  {n_nom} nominal")
