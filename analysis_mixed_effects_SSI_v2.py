"""
Mixed Effects Model Analysis (v2): F0 & LIWC Features → SSI
=============================================================
Model:
  SSI ~ feature_z + time_numeric + age + sex + C(Dx) + [AP_dose or edu_yrs] + (1|id)

Covariates:
  - Both models : time_numeric (0/2/4/8/12 months), age, sex, C(Dx)
  - F0final model: + AP_dose
  - LIWC model   : + edu_yrs

F0 features: 39 F0final_sma_* (as specified by user)
LIWC features: all liwc_* columns
"""

import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
from statsmodels.stats.multitest import multipletests
import warnings
warnings.filterwarnings('ignore')

# ─────────────────────────────────────────────
# 1. Load & Prepare Data
# ─────────────────────────────────────────────
df = pd.read_csv('/root/.claude/uploads/182da1f2-ce75-4f20-9aa1-09191b685df9/e668796d-prism_softImpute4_smile.csv')

# time_numeric: case_episode을 숫자형 시간(개월)으로 인코딩
time_map = {'baseline': 0, '2m': 2, '4m': 4, '8m': 8, '12m': 12}
df['time_numeric'] = df['case_episode'].map(time_map)

# Dx: 문자형 범주 → statsmodels formula에서 C(Dx) 처리 (reference: BPII)
df['Dx'] = df['Dx'].astype(str)

print("=== Dataset Info ===")
print(f"Rows: {df.shape[0]}, Patients: {df['id'].nunique()}")
print(f"SSI: mean={df['SSI'].mean():.2f}, std={df['SSI'].std():.2f}, range [{df['SSI'].min()}-{df['SSI'].max()}]")
print(f"Dx distribution:\n{df['Dx'].value_counts()}")
print(f"Missing in covariates:")
for c in ['age','sex','Dx','AP_dose','edu_yrs','time_numeric']:
    print(f"  {c}: {df[c].isna().sum()}")
print()

# ─────────────────────────────────────────────
# 2. Define Features
# ─────────────────────────────────────────────

# F0: 사용자 지정 39개
F0_FEATURES = [
    'F0final_sma_amean', 'F0final_sma_flatness', 'F0final_sma_posamean', 'F0final_sma_rqmean',
    'F0final_sma_range', 'F0final_sma_maxPos', 'F0final_sma_minPos',
    'F0final_sma_linregc1', 'F0final_sma_linregc2', 'F0final_sma_linregerrQ',
    'F0final_sma_qregc1', 'F0final_sma_qregc2', 'F0final_sma_qregc3', 'F0final_sma_qregerrQ',
    'F0final_sma_centroid', 'F0final_sma_quartile1', 'F0final_sma_quartile2', 'F0final_sma_quartile3',
    'F0final_sma_iqr1-2', 'F0final_sma_iqr2-3', 'F0final_sma_iqr1-3',
    'F0final_sma_percentile1.0', 'F0final_sma_percentile99.0', 'F0final_sma_pctlrange0-1',
    'F0final_sma_stddev', 'F0final_sma_skewness', 'F0final_sma_kurtosis',
    'F0final_sma_upleveltime25', 'F0final_sma_upleveltime50', 'F0final_sma_upleveltime75',
    'F0final_sma_upleveltime90', 'F0final_sma_risetime', 'F0final_sma_leftctime',
    'F0final_sma_lpgain', 'F0final_sma_lpc0', 'F0final_sma_lpc1',
    'F0final_sma_lpc2', 'F0final_sma_lpc3', 'F0final_sma_lpc4'
]

# LIWC: 전체
LIWC_FEATURES = [c for c in df.columns if c.lower().startswith('liwc_')]

print(f"F0 features: {len(F0_FEATURES)}")
print(f"LIWC features: {len(LIWC_FEATURES)}")

# ─────────────────────────────────────────────
# 3. Mixed Effects Model Function
# ─────────────────────────────────────────────

def run_lme_with_covariates(df, features, extra_covariates, label=''):
    """
    SSI ~ feat_z + time_numeric + age + sex + C(Dx) + [extra_covariates] + (1|id)

    feat_z: z-표준화한 feature (효과크기 비교 위해)
    Dx: dummy coding, reference = 'BPII'
    """
    base_covs = ['time_numeric', 'age', 'sex', 'C(Dx, Treatment("BPII"))']
    all_covs  = base_covs + extra_covariates
    cov_str   = ' + '.join(all_covs)

    results = []
    total = len(features)

    for i, feat in enumerate(features):
        if (i+1) % 20 == 0 or i == 0:
            print(f"  [{label}] {i+1}/{total}: {feat}")

        safe_name = 'feat_z'
        needed_cols = [feat, 'SSI', 'id', 'time_numeric', 'age', 'sex', 'Dx', 'AP_dose', 'edu_yrs']
        tmp = df[[c for c in needed_cols if c in df.columns]].copy().dropna()
        tmp = tmp.rename(columns={feat: safe_name})

        # z-표준화
        std = tmp[safe_name].std()
        if std == 0 or np.isnan(std):
            results.append({'feature': feat, **{k: np.nan for k in
                ['coef','SE','CI_lower','CI_upper','p_value','n_obs','n_subjects']}})
            continue

        tmp[safe_name] = (tmp[safe_name] - tmp[safe_name].mean()) / std

        try:
            formula = f'SSI ~ {safe_name} + {cov_str}'
            model  = smf.mixedlm(formula, tmp, groups=tmp['id'])
            result = model.fit(reml=True, method='lbfgs')

            results.append({
                'feature':    feat,
                'coef':       result.params[safe_name],
                'SE':         result.bse[safe_name],
                'CI_lower':   result.conf_int().loc[safe_name, 0],
                'CI_upper':   result.conf_int().loc[safe_name, 1],
                'p_value':    result.pvalues[safe_name],
                'n_obs':      len(tmp),
                'n_subjects': tmp['id'].nunique()
            })
        except Exception as e:
            results.append({'feature': feat, **{k: np.nan for k in
                ['coef','SE','CI_lower','CI_upper','p_value','n_obs','n_subjects']}})

    res_df = pd.DataFrame(results).dropna(subset=['p_value'])

    # FDR (Benjamini-Hochberg)
    _, p_fdr, _, _ = multipletests(res_df['p_value'], method='fdr_bh')
    res_df['p_fdr'] = p_fdr

    # Bonferroni
    _, p_bon, _, _ = multipletests(res_df['p_value'], method='bonferroni')
    res_df['p_bonferroni'] = p_bon

    return res_df.sort_values('p_value').reset_index(drop=True)


# ─────────────────────────────────────────────
# 4. Run Analysis
# ─────────────────────────────────────────────
print("\n" + "="*60)
print("  F0 Models: SSI ~ feat_z + time + age + sex + Dx + AP_dose + (1|id)")
print("="*60)
f0_res = run_lme_with_covariates(
    df, F0_FEATURES,
    extra_covariates=['AP_dose'],
    label='F0'
)

print("\n" + "="*60)
print("  LIWC Models: SSI ~ feat_z + time + age + sex + Dx + edu_yrs + (1|id)")
print("="*60)
liwc_res = run_lme_with_covariates(
    df, LIWC_FEATURES,
    extra_covariates=['edu_yrs'],
    label='LIWC'
)

# ─────────────────────────────────────────────
# 5. Print Results
# ─────────────────────────────────────────────
def print_results(res_df, label, top_n=25):
    sig_fdr = (res_df['p_fdr'] < 0.05).sum()
    sig_nom = (res_df['p_value'] < 0.05).sum()
    trend   = ((res_df['p_value'] >= 0.05) & (res_df['p_value'] < 0.10)).sum()

    print(f"\n{'='*70}")
    print(f"  {label}")
    print(f"{'='*70}")
    print(f"  Features tested           : {len(res_df)}")
    print(f"  FDR significant (q<0.05)  : {sig_fdr}")
    print(f"  Nominally sig (p<0.05)    : {sig_nom}")
    print(f"  Trend (p 0.05–0.10)       : {trend}")
    print()
    print(f"  {'Feature':<42} {'β':>7} {'SE':>6} {'p':>10} {'q_FDR':>10}  Mark")
    print(f"  {'-'*85}")
    for _, row in res_df.head(top_n).iterrows():
        if row['p_fdr'] < 0.001:
            mark = '★★★ FDR'
        elif row['p_fdr'] < 0.01:
            mark = '★★  FDR'
        elif row['p_fdr'] < 0.05:
            mark = '★   FDR'
        elif row['p_value'] < 0.001:
            mark = '***'
        elif row['p_value'] < 0.01:
            mark = '** '
        elif row['p_value'] < 0.05:
            mark = '*  '
        elif row['p_value'] < 0.10:
            mark = '.  '
        else:
            mark = ''
        print(f"  {row['feature']:<42} {row['coef']:>7.3f} {row['SE']:>6.3f} "
              f"{row['p_value']:>10.5f} {row['p_fdr']:>10.5f}  {mark}")

print_results(f0_res,   "F0 Features → SSI (covariate: age, sex, Dx, AP_dose, time)")
print_results(liwc_res, "LIWC Features → SSI (covariate: age, sex, Dx, edu_yrs, time)")

# ─────────────────────────────────────────────
# 6. Save Results
# ─────────────────────────────────────────────
import os
OUT = '/home/user/PRISM-V/results'
os.makedirs(OUT, exist_ok=True)

f0_res.to_csv(f'{OUT}/lme_v2_F0_SSI_results.csv', index=False)
liwc_res.to_csv(f'{OUT}/lme_v2_LIWC_SSI_results.csv', index=False)

all_res = pd.concat([f0_res.assign(feature_type='F0'), liwc_res.assign(feature_type='LIWC')])
all_res[all_res['p_value'] < 0.05].sort_values('p_value').to_csv(
    f'{OUT}/lme_v2_significant_p05.csv', index=False)
all_res[all_res['p_fdr'] < 0.05].sort_values('p_fdr').to_csv(
    f'{OUT}/lme_v2_significant_FDR.csv', index=False)

print(f"\n\n✓ Saved to {OUT}/")
print("  lme_v2_F0_SSI_results.csv")
print("  lme_v2_LIWC_SSI_results.csv")
print("  lme_v2_significant_p05.csv")
print("  lme_v2_significant_FDR.csv")
