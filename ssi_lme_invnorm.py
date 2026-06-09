"""
SSI ~ liwc / F0 features  — invnorm-transformed features
Equivalent to analysis_mixed_effects_SSI_v2.py but with invnorm per feature.
"""

import pandas as pd
import numpy as np
from scipy.stats import rankdata, norm
import statsmodels.formula.api as smf
from statsmodels.stats.multitest import multipletests
import warnings, os
warnings.filterwarnings('ignore')

def invnorm(x):
    x = np.asarray(pd.to_numeric(x, errors='coerce'), dtype=float)
    ok = ~np.isnan(x)
    y  = np.full(len(x), np.nan)
    if ok.sum() <= 1:
        return y
    ranks = rankdata(x[ok], method='average')
    y[ok] = norm.ppf((ranks - 0.5) / ok.sum())
    return y

# ── Data ─────────────────────────────────────────────────────
df = pd.read_csv('data/prism_softImpute4_smile_with_items.csv', low_memory=False)
df['time_numeric'] = df['case_episode'].map(
    {'baseline': 0, '2m': 2, '4m': 4, '8m': 8, '12m': 12})
df['Dx'] = df['Dx'].astype(str)
for c in ['SSI', 'age', 'sex', 'edu_yrs', 'AP_dose']:
    df[c] = pd.to_numeric(df[c], errors='coerce')

F0_FEATS   = [c for c in df.columns if c.startswith('F0final_sma')]
LIWC_FEATS = [c for c in df.columns if c.startswith('liwc_')]
for c in F0_FEATS + LIWC_FEATS:
    df[c] = pd.to_numeric(df[c], errors='coerce')

BASE = 'time_numeric + age + sex + C(Dx, Treatment("BPII"))'

# ── Runner ────────────────────────────────────────────────────
def run(df, features, cov_str):
    needed = ['id', 'SSI', 'time_numeric', 'age', 'sex', 'Dx',
              'AP_dose', 'edu_yrs'] + features
    sub = df[[c for c in needed if c in df.columns]].dropna().reset_index(drop=True)
    y_m, y_s = sub['SSI'].mean(), sub['SSI'].std()

    rows = []
    for feat in features:
        tmp = sub[['id', 'SSI', feat, 'time_numeric',
                   'age', 'sex', 'Dx', 'AP_dose', 'edu_yrs']].dropna().copy()
        inv = invnorm(tmp[feat].values)
        if np.nanstd(inv) == 0:
            continue
        tmp['feat_in'] = inv
        tmp['Y']       = (tmp['SSI'] - y_m) / y_s

        fitted = None
        for method in ['lbfgs', 'powell', 'nm']:
            try:
                fitted = smf.mixedlm(
                    f'Y ~ feat_in + {cov_str}', tmp, groups=tmp['id']
                ).fit(reml=True, method=method)
                break
            except Exception:
                continue
        if fitted:
            rows.append({
                'feature':  feat,
                'coef':     fitted.params['feat_in'],
                'SE':       fitted.bse['feat_in'],
                'CI_lower': fitted.conf_int().loc['feat_in', 0],
                'CI_upper': fitted.conf_int().loc['feat_in', 1],
                'p_value':  fitted.pvalues['feat_in'],
                'n_obs':    len(tmp),
                'n_subjects': tmp['id'].nunique(),
            })

    res = pd.DataFrame(rows)
    _, q, _, _ = multipletests(res['p_value'], method='fdr_bh')
    res['p_fdr'] = q
    return res.sort_values('p_value').reset_index(drop=True)

print("Running F0 → SSI (invnorm)...")
f0_res = run(df, F0_FEATS, BASE + ' + AP_dose')
print(f"  F0 FDR-sig: {(f0_res['p_fdr']<0.05).sum()}  nominal: {(f0_res['p_value']<0.05).sum()}")

print("Running LIWC → SSI (invnorm)...")
liwc_res = run(df, LIWC_FEATS, BASE + ' + edu_yrs')
print(f"  LIWC FDR-sig: {(liwc_res['p_fdr']<0.05).sum()}  nominal: {(liwc_res['p_value']<0.05).sum()}")

os.makedirs('results', exist_ok=True)
f0_res.to_csv('results/lme_invnorm_F0_SSI_results.csv', index=False)
liwc_res.to_csv('results/lme_invnorm_LIWC_SSI_results.csv', index=False)
print("✓ Saved.")
