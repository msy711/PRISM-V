"""
Mixed Effects Model Analysis: F0 & LIWC Features → SSI
=======================================================
- Outcome: SSI (Scale for Suicidal Ideation)
- Random Effect: patient ID (id) - to account for repeated measures
- Fixed Effects: each F0/LIWC feature individually + time covariate
- Multiple comparison correction: FDR (Benjamini-Hochberg)
- Data: prism_softImpute4_smile.csv (N=104 patients, up to 5 time points)
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

# Encode time as numeric ordinal (months)
time_map = {'baseline': 0, '2m': 2, '4m': 4, '8m': 8, '12m': 12}
df['time_numeric'] = df['case_episode'].map(time_map)

# Define feature groups
f0_smile_cols = [c for c in df.columns if c.startswith('F0final_sma')]
f0_basic_cols = ['mean_p', 'error_p', 'change_p', 'mean_m', 'error_m', 'change_m', 'mean_z', 'delay_p']
f0_all = f0_basic_cols + f0_smile_cols

liwc_cols = [c for c in df.columns if c.lower().startswith('liwc_')]

print(f"Dataset: {df.shape[0]} rows, {df['id'].nunique()} patients")
print(f"F0 features: {len(f0_all)} (basic: {len(f0_basic_cols)}, openSMILE: {len(f0_smile_cols)})")
print(f"LIWC features: {len(liwc_cols)}")
print(f"SSI range: {df['SSI'].min():.1f} – {df['SSI'].max():.1f}, mean={df['SSI'].mean():.2f}")
print()

# ─────────────────────────────────────────────
# 2. Univariate Mixed Effects Models
# ─────────────────────────────────────────────
def run_lme_univariate(df, features, outcome='SSI', group_var='id',
                        time_covariate='time_numeric'):
    """
    For each feature, fit:
        outcome ~ feature + time_numeric + (1 | id)
    Returns DataFrame with coefficients and p-values.
    """
    results = []

    for feat in features:
        # Sanitize feature name for formula
        safe_name = 'feat_var'
        tmp = df[[outcome, feat, time_covariate, group_var]].copy().dropna()
        tmp = tmp.rename(columns={feat: safe_name})

        # Standardize feature (z-score) for comparable effect sizes
        feat_std = tmp[safe_name].std()
        if feat_std == 0 or np.isnan(feat_std):
            continue
        tmp[safe_name] = (tmp[safe_name] - tmp[safe_name].mean()) / feat_std

        try:
            formula = f"{outcome} ~ {safe_name} + {time_covariate}"
            model = smf.mixedlm(formula, tmp, groups=tmp[group_var])
            result = model.fit(reml=True, method='lbfgs')

            coef  = result.params[safe_name]
            pval  = result.pvalues[safe_name]
            se    = result.bse[safe_name]
            ci_lo = result.conf_int().loc[safe_name, 0]
            ci_hi = result.conf_int().loc[safe_name, 1]

            results.append({
                'feature':   feat,
                'coef':      coef,
                'SE':        se,
                'CI_lower':  ci_lo,
                'CI_upper':  ci_hi,
                'p_value':   pval,
                'n_obs':     len(tmp),
                'n_subjects': tmp[group_var].nunique()
            })
        except Exception as e:
            results.append({
                'feature':   feat,
                'coef':      np.nan,
                'SE':        np.nan,
                'CI_lower':  np.nan,
                'CI_upper':  np.nan,
                'p_value':   np.nan,
                'n_obs':     np.nan,
                'n_subjects':np.nan
            })

    res_df = pd.DataFrame(results)
    res_df = res_df.dropna(subset=['p_value'])

    # FDR correction (Benjamini-Hochberg)
    _, p_fdr, _, _ = multipletests(res_df['p_value'], method='fdr_bh')
    res_df['p_fdr'] = p_fdr

    # Bonferroni correction
    _, p_bon, _, _ = multipletests(res_df['p_value'], method='bonferroni')
    res_df['p_bonferroni'] = p_bon

    res_df = res_df.sort_values('p_value')
    return res_df

# ─────────────────────────────────────────────
# 3. Run Analysis
# ─────────────────────────────────────────────
print("=" * 60)
print("Running LME for F0 features...")
print("=" * 60)
f0_res = run_lme_univariate(df, f0_all)

print("Running LME for LIWC features...")
liwc_res = run_lme_univariate(df, liwc_cols)

# ─────────────────────────────────────────────
# 4. Summarize Results
# ─────────────────────────────────────────────
def print_summary(res_df, label, alpha_nom=0.05, alpha_fdr=0.05):
    print(f"\n{'='*60}")
    print(f"  {label} — Results (sorted by p-value)")
    print(f"{'='*60}")

    sig_nom = res_df[res_df['p_value'] < alpha_nom]
    sig_fdr = res_df[res_df['p_fdr']   < alpha_fdr]

    print(f"  Total features tested : {len(res_df)}")
    print(f"  Nominally significant (p<{alpha_nom}) : {len(sig_nom)}")
    print(f"  FDR-corrected (q<{alpha_fdr})          : {len(sig_fdr)}")
    print()

    # Show top results
    top = res_df.head(20)
    print(f"{'Feature':<45} {'Coef':>8} {'SE':>8} {'p':>10} {'q(FDR)':>10}  Sig")
    print("-" * 90)
    for _, row in top.iterrows():
        sig_marker = ""
        if row['p_fdr'] < 0.05:
            sig_marker = "*** FDR"
        elif row['p_value'] < 0.05:
            sig_marker = "*   nom"
        elif row['p_value'] < 0.10:
            sig_marker = ".   trend"
        print(f"{row['feature']:<45} {row['coef']:>8.3f} {row['SE']:>8.3f} "
              f"{row['p_value']:>10.4f} {row['p_fdr']:>10.4f}  {sig_marker}")

print_summary(f0_res, "F0 FEATURES → SSI")
print_summary(liwc_res, "LIWC FEATURES → SSI")

# ─────────────────────────────────────────────
# 5. Save Full Results
# ─────────────────────────────────────────────
output_dir = '/home/user/PRISM-V/results'
import os
os.makedirs(output_dir, exist_ok=True)

f0_res.to_csv(f'{output_dir}/lme_F0_SSI_results.csv', index=False)
liwc_res.to_csv(f'{output_dir}/lme_LIWC_SSI_results.csv', index=False)

# Combined significant features
all_res = pd.concat([
    f0_res.assign(feature_type='F0'),
    liwc_res.assign(feature_type='LIWC')
])
sig_all = all_res[all_res['p_fdr'] < 0.05].copy()
sig_nom = all_res[all_res['p_value'] < 0.05].copy()

print(f"\n{'='*60}")
print("  SUMMARY: Nominally Significant Features (p < 0.05)")
print(f"{'='*60}")
print(sig_nom[['feature_type','feature','coef','SE','p_value','p_fdr']].to_string(index=False))

sig_all.to_csv(f'{output_dir}/lme_significant_features_FDR.csv', index=False)
sig_nom.to_csv(f'{output_dir}/lme_significant_features_nominal.csv', index=False)

print(f"\n✓ Results saved to {output_dir}/")
print(f"  lme_F0_SSI_results.csv")
print(f"  lme_LIWC_SSI_results.csv")
print(f"  lme_significant_features_FDR.csv")
print(f"  lme_significant_features_nominal.csv")
