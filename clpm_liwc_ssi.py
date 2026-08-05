"""
Cross-Lagged Panel Model: LIWC(T) -> SSI(T+1)
+ Reverse: SSI(T) -> liwc_death / liwc_quantifiers (T+1)

Model:
  SSI(T+1)_z ~ feat(T)_invnorm + SSI(T)_z + time_gap
               + age + sex + C(Dx) + edu_yrs + (1|id)
"""

import pandas as pd
import numpy as np
from scipy.stats import rankdata, norm
import statsmodels.formula.api as smf
from statsmodels.stats.multitest import multipletests
import warnings, os
warnings.filterwarnings('ignore')

os.chdir('/home/user/PRISM-V')

# ── helpers ────────────────────────────────────────────────────
def invnorm(x):
    x = np.asarray(pd.to_numeric(x, errors='coerce'), dtype=float)
    ok = ~np.isnan(x)
    y = np.full(len(x), np.nan)
    if ok.sum() <= 1: return y
    y[ok] = norm.ppf((rankdata(x[ok], method='average') - 0.5) / ok.sum())
    return y

# ── data ───────────────────────────────────────────────────────
df = pd.read_csv('data/prism_softImpute4_smile_with_items.csv', low_memory=False)
df['time_numeric'] = df['case_episode'].map(
    {'baseline':0,'2m':2,'4m':4,'8m':8,'12m':12})
df['Dx'] = df['Dx'].astype(str)
for c in ['SSI','age','sex','edu_yrs','AP_dose']:
    df[c] = pd.to_numeric(df[c], errors='coerce')

LIWC = [c for c in df.columns if c.startswith('liwc_')]
for c in LIWC:
    df[c] = pd.to_numeric(df[c], errors='coerce')

print(f"Rows: {len(df)} | Patients: {df['id'].nunique()}")
print(f"LIWC features: {len(LIWC)}")

# ── build T -> T+1 pairs ───────────────────────────────────────
df = df.sort_values(['id','time_numeric'])
df_lag = (df.assign(SSI_t1   = df.groupby('id')['SSI'].shift(-1),
                    time_t1  = df.groupby('id')['time_numeric'].shift(-1))
            .dropna(subset=['SSI_t1']))
df_lag['time_gap'] = df_lag['time_t1'] - df_lag['time_numeric']

print(f"Lagged pairs: {len(df_lag)} | Patients: {df_lag['id'].nunique()}")
print("\nTransitions (T -> T+1):")
print(pd.crosstab(df_lag['time_numeric'], df_lag['time_t1']))

# z-score SSI at T and T+1 globally
ssi_m, ssi_s = df_lag['SSI'].mean(), df_lag['SSI'].std()
df_lag['SSI_ar'] = (df_lag['SSI']   - ssi_m) / ssi_s   # autoregressive
df_lag['Y']      = (df_lag['SSI_t1'] - ssi_m) / ssi_s  # outcome

# ── CLPM runner ────────────────────────────────────────────────
BASE = 'SSI_ar + time_gap + age + sex + C(Dx, Treatment("BPII")) + edu_yrs'

def run_clpm(data, features):
    rows = []
    for feat in features:
        tmp = data[['id','Y','SSI_ar',feat,'time_gap',
                    'age','sex','Dx','edu_yrs']].dropna().copy()
        if len(tmp) < 10: continue

        tmp['feat_t'] = invnorm(tmp[feat].values)
        if np.nanstd(tmp['feat_t']) == 0: continue
        tmp = tmp[~np.isnan(tmp['feat_t'])]
        if len(tmp) < 10: continue

        fitted = None
        for method in ['lbfgs','powell','nm']:
            try:
                fitted = smf.mixedlm(
                    f'Y ~ feat_t + {BASE}', tmp, groups=tmp['id']
                ).fit(reml=True, method=method)
                break
            except Exception: continue
        if fitted is None: continue

        b  = fitted.params.get('feat_t', np.nan)
        se = fitted.bse.get('feat_t', np.nan)
        p  = fitted.pvalues.get('feat_t', np.nan)
        b_ar = fitted.params.get('SSI_ar', np.nan)
        p_ar = fitted.pvalues.get('SSI_ar', np.nan)
        ci = fitted.conf_int()
        lo = ci.loc['feat_t', 0] if 'feat_t' in ci.index else np.nan
        hi = ci.loc['feat_t', 1] if 'feat_t' in ci.index else np.nan

        rows.append(dict(feature=feat, b_cross=b, SE_cross=se,
                         CI_lo=lo, CI_hi=hi, p_cross=p,
                         b_AR=b_ar, p_AR=p_ar,
                         n=len(tmp), n_subj=tmp['id'].nunique()))

    res = pd.DataFrame(rows)
    if len(res) == 0: return res
    _, res['q_cross'], _, _ = multipletests(res['p_cross'], method='fdr_bh')
    return res.sort_values('p_cross').reset_index(drop=True)

# ── Forward: LIWC(T) -> SSI(T+1) ──────────────────────────────
print("\n" + "="*65)
print("  LIWC(T) -> SSI(T+1)  [controlling for SSI(T)]")
print("="*65)
res_fwd = run_clpm(df_lag, LIWC)

fdr_sig = (res_fwd['q_cross'] < 0.05).sum()
nom_sig = (res_fwd['p_cross'] < 0.05).sum()
print(f"FDR-sig: {fdr_sig}  |  nominal: {nom_sig}\n")

print(f"  {'Feature':<30} {'b_CL':>7} {'SE':>6} {'p':>9} {'q_FDR':>9} {'b_AR':>7}")
print(f"  {'-'*72}")
for _, r in res_fwd.head(20).iterrows():
    sig = '[FDR]' if r['q_cross']<0.05 else ('*' if r['p_cross']<0.05 else '')
    feat = r['feature'].replace('liwc_','')
    print(f"  {feat:<30} {r['b_cross']:>7.3f} {r['SE_cross']:>6.3f} "
          f"{r['p_cross']:>9.4f} {r['q_cross']:>9.4f} {r['b_AR']:>7.3f}  {sig}")

# ── Reverse: SSI(T) -> feat(T+1) ──────────────────────────────
def run_reverse(data, feat_name):
    """Does SSI(T) predict feat(T+1) after controlling for feat(T)?"""
    df_r = data.copy()
    df_r = df_r.sort_values(['id','time_numeric'])
    df_r['feat_next'] = df_r.groupby('id')[feat_name].shift(-1)
    df_r = df_r.dropna(subset=['feat_next'])

    df_r['Y_feat']   = invnorm(df_r['feat_next'].values)
    df_r['feat_t']   = invnorm(df_r[feat_name].values)

    tmp = df_r[['id','Y_feat','feat_t','SSI_ar','time_gap',
                'age','sex','Dx','edu_yrs']].dropna()
    if len(tmp) < 10: return None

    fitted = None
    for method in ['lbfgs','powell','nm']:
        try:
            fitted = smf.mixedlm(
                f'Y_feat ~ SSI_ar + feat_t + {BASE.replace("SSI_ar + ","")}',
                tmp, groups=tmp['id']
            ).fit(reml=True, method=method)
            break
        except Exception: continue
    if fitted is None: return None

    return dict(
        b_SSI_to_feat = fitted.params.get('SSI_ar', np.nan),
        p_SSI_to_feat = fitted.pvalues.get('SSI_ar', np.nan),
        b_AR_feat     = fitted.params.get('feat_t', np.nan),
        p_AR_feat     = fitted.pvalues.get('feat_t', np.nan),
        n             = len(tmp)
    )

print("\n" + "="*65)
print("  Reverse direction: SSI(T) -> feat(T+1)")
print("="*65)
for feat in ['liwc_death','liwc_quantifiers','liwc_focus_present','liwc_inaction']:
    r = run_reverse(df_lag, feat)
    if r:
        print(f"\n  SSI(T) -> {feat.replace('liwc_','')}(T+1):")
        print(f"    b = {r['b_SSI_to_feat']:.3f}, p = {r['p_SSI_to_feat']:.4f}")
        print(f"    AR (feat_t): b = {r['b_AR_feat']:.3f}, p = {r['p_AR_feat']:.4f}")
        print(f"    n = {r['n']}")

# ── Save ───────────────────────────────────────────────────────
os.makedirs('results', exist_ok=True)
res_fwd.to_csv('results/clpm_liwc_ssi.csv', index=False)
print("\nSaved: results/clpm_liwc_ssi.csv")
