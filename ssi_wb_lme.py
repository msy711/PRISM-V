"""
SSI ~ F0 / LIWC  —  Within-Between Person Decomposition
=========================================================
Decomposes each feature into:
  feat_between  = patient-level mean  (stable trait)
  feat_within   = feat - feat_between (time-varying state)

Model per feature:
  SSI_z ~ feat_within + feat_between
          + time + age + sex + C(Dx, Treatment("BPII"))
          + covariate + (1|id)

FDR correction applied separately for within- and between-person effects.
"""

import pandas as pd
import numpy as np
from scipy.stats import rankdata, norm
import statsmodels.formula.api as smf
from statsmodels.stats.multitest import multipletests
import warnings, os
warnings.filterwarnings('ignore')


# ── Inverse Normal Transformation ────────────────────────────
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


# ── Within-Between Runner ─────────────────────────────────────
def run_wb(df, features, cov_str):
    needed = ['id', 'SSI', 'time_numeric', 'age', 'sex', 'Dx',
              'AP_dose', 'edu_yrs'] + features
    sub = df[[c for c in needed if c in df.columns]].dropna().reset_index(drop=True)
    y_m, y_s = sub['SSI'].mean(), sub['SSI'].std()

    rows = []
    for feat in features:
        tmp = sub[['id', 'SSI', feat, 'time_numeric',
                   'age', 'sex', 'Dx', 'AP_dose', 'edu_yrs']].dropna().copy()

        # Invnorm transform
        inv = invnorm(tmp[feat].values)
        if np.nanstd(inv) == 0:
            continue
        tmp['feat_x'] = inv

        # Within-between decomposition
        tmp['feat_between'] = tmp.groupby('id')['feat_x'].transform('mean')
        tmp['feat_within']  = tmp['feat_x'] - tmp['feat_between']

        # Skip if either component is constant
        if tmp['feat_within'].std() == 0 or tmp['feat_between'].std() == 0:
            continue

        tmp['Y'] = (tmp['SSI'] - y_m) / y_s

        fitted = None
        for method in ['lbfgs', 'powell', 'nm']:
            try:
                fitted = smf.mixedlm(
                    f'Y ~ feat_within + feat_between + {cov_str}',
                    tmp, groups=tmp['id']
                ).fit(reml=True, method=method)
                break
            except Exception:
                continue
        if fitted is None:
            continue

        ci = fitted.conf_int()
        rows.append({
            'feature':        feat,
            # Within-person (state) effect
            'b_within':       fitted.params['feat_within'],
            'SE_within':      fitted.bse['feat_within'],
            'CI_lo_within':   ci.loc['feat_within', 0],
            'CI_hi_within':   ci.loc['feat_within', 1],
            'p_within':       fitted.pvalues['feat_within'],
            # Between-person (trait) effect
            'b_between':      fitted.params['feat_between'],
            'SE_between':     fitted.bse['feat_between'],
            'CI_lo_between':  ci.loc['feat_between', 0],
            'CI_hi_between':  ci.loc['feat_between', 1],
            'p_between':      fitted.pvalues['feat_between'],
            'n_obs':          len(tmp),
            'n_subjects':     tmp['id'].nunique(),
        })

    res = pd.DataFrame(rows)
    if len(res) == 0:
        return res

    # FDR separately for within and between
    _, res['q_within'],  _, _ = multipletests(res['p_within'],  method='fdr_bh')
    _, res['q_between'], _, _ = multipletests(res['p_between'], method='fdr_bh')
    return res.sort_values('p_within').reset_index(drop=True)


# ── Run ───────────────────────────────────────────────────────
print("=" * 65)
print("  Within-Between Decomposition: F0 / LIWC -> SSI (invnorm)")
print("=" * 65)

print("\nRunning F0 ...")
f0_res = run_wb(df, F0_FEATS, BASE + ' + AP_dose')
print(f"  Within  FDR-sig: {(f0_res['q_within'] <0.05).sum()}  "
      f"nominal: {(f0_res['p_within'] <0.05).sum()}")
print(f"  Between FDR-sig: {(f0_res['q_between']<0.05).sum()}  "
      f"nominal: {(f0_res['p_between']<0.05).sum()}")

print("\nRunning LIWC ...")
liwc_res = run_wb(df, LIWC_FEATS, BASE + ' + edu_yrs')
print(f"  Within  FDR-sig: {(liwc_res['q_within'] <0.05).sum()}  "
      f"nominal: {(liwc_res['p_within'] <0.05).sum()}")
print(f"  Between FDR-sig: {(liwc_res['q_between']<0.05).sum()}  "
      f"nominal: {(liwc_res['p_between']<0.05).sum()}")

# ── Print top results ─────────────────────────────────────────
def print_top(res, label, n=10):
    cols = ['feature', 'b_within', 'SE_within', 'p_within', 'q_within',
                       'b_between','SE_between','p_between','q_between']
    print(f"\n  {label} — top {n} by p_within:")
    print(f"  {'Feature':<30} {'b_W':>7} {'SE_W':>6} {'p_W':>9} {'q_W':>9}  "
          f"{'b_B':>7} {'p_B':>9} {'q_B':>9}")
    print(f"  {'-'*85}")
    for _, r in res.head(n).iterrows():
        sig_w = '[FDR]' if r['q_within'] <0.05 else ('*' if r['p_within'] <0.05 else '')
        sig_b = '[FDR]' if r['q_between']<0.05 else ('*' if r['p_between']<0.05 else '')
        feat = r['feature'].replace('F0final_sma_','F0_').replace('liwc_','')
        print(f"  {feat:<30} {r['b_within']:>7.3f} {r['SE_within']:>6.3f} "
              f"{r['p_within']:>9.4f} {r['q_within']:>9.4f}  "
              f"{r['b_between']:>7.3f} {r['p_between']:>9.4f} {r['q_between']:>9.4f}  "
              f"{sig_w} / {sig_b}")

print_top(f0_res,   "F0",   n=15)
print_top(liwc_res, "LIWC", n=15)

# ── Save ──────────────────────────────────────────────────────
os.makedirs('results', exist_ok=True)
f0_res.to_csv('results/wb_lme_F0_SSI.csv',   index=False)
liwc_res.to_csv('results/wb_lme_LIWC_SSI.csv', index=False)
print("\n✓ results/wb_lme_F0_SSI.csv")
print("✓ results/wb_lme_LIWC_SSI.csv")
