"""
Multilevel Causal Mediation Analysis
=====================================
Model: X ──a──> M ──b──> Y (SSI)
       X ──────────────> Y  (direct: c')
       Indirect effect = a × b

Structure:
  Level-1 (within-person): X_ij, M_ij, Y_ij
  Level-2 (between-person): patient i

a-path: M   ~ X + time + age + sex + C(Dx) + (1|id)
b-path: SSI ~ X + M + time + age + sex + C(Dx) + (1|id)

Indirect effect CI: Monte Carlo method (5000 draws from N(a,SE_a) × N(b,SE_b))
Multiple comparison: FDR (BH) across mediators, separately per predictor
"""

import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
from statsmodels.stats.multitest import multipletests
import warnings
warnings.filterwarnings('ignore')

# ─────────────────────────────────────────────
# 1. Data Preparation
# ─────────────────────────────────────────────
df = pd.read_csv('/home/user/PRISM-V/data/prism_softImpute4_smile_with_items.csv', low_memory=False)
df['BHOL9'] = pd.to_numeric(df['BHOL9'], errors='coerce')

time_map = {'baseline': 0, '2m': 2, '4m': 4, '8m': 8, '12m': 12}
df['time_numeric'] = df['case_episode'].map(time_map)
df['Dx'] = df['Dx'].astype(str)

# 비-SSI item mediator 후보
item_cols = (
    [f'HAMD{i}' for i in range(1, 18)] +
    [f'PHQ{i}'  for i in range(1, 10)] +
    [f'BAI{i}'  for i in range(1, 22)] +
    [f'BHOL{i}' for i in range(1, 21)]
)

PREDICTORS = {
    'liwc_death':        {'extra_cov': 'edu_yrs'},
    'F0final_sma_qregc3': {'extra_cov': 'AP_dose'},
}
OUTCOME  = 'SSI'
BASE_COV = ['time_numeric', 'age', 'sex', 'C(Dx, Treatment("BPII"))']

# Complete cases for all variables
all_vars = list(PREDICTORS.keys()) + [OUTCOME] + item_cols + \
           ['id', 'time_numeric', 'age', 'sex', 'Dx', 'AP_dose', 'edu_yrs']
sub = df[[c for c in all_vars if c in df.columns]].dropna().reset_index(drop=True)
print(f"Complete cases: {len(sub)} rows | {sub['id'].nunique()} patients")
print(f"case_episode: {sub['case_episode'].value_counts().to_dict() if 'case_episode' in sub.columns else 'N/A'}")

# ─────────────────────────────────────────────
# 2. Select mediator candidates (|r| > 0.3)
# ─────────────────────────────────────────────
mediators = []
for item in item_cols:
    r_liwc = abs(sub['liwc_death'].corr(sub[item]))
    r_f0   = abs(sub['F0final_sma_qregc3'].corr(sub[item]))
    r_ssi  = abs(sub[item].corr(sub[OUTCOME]))
    if r_liwc > 0.3 or r_f0 > 0.3 or r_ssi > 0.3:
        mediators.append(item)

print(f"Mediator candidates (|r|>0.3): {len(mediators)}")

# ─────────────────────────────────────────────
# 3. Multilevel Mediation Function
# ─────────────────────────────────────────────
def multilevel_mediation(df, X, M, Y, extra_cov, n_mc=5000, seed=42):
    """
    Multilevel mediation via two LME models + Monte Carlo CI for indirect effect.

    a-path: M   ~ X_z + covariates + (1|id)
    b-path: Y   ~ X_z + M_z + covariates + (1|id)

    Returns dict with paths a, b, c', indirect=a×b, and 95% MC CI.
    """
    rng = np.random.default_rng(seed)
    cov_str = ' + '.join(BASE_COV + [extra_cov])

    tmp = df[['id', X, M, Y, 'time_numeric', 'age', 'sex', 'Dx', 'AP_dose', 'edu_yrs']].dropna().copy()

    # Z-standardize X and M for comparable effect sizes
    for col, alias in [(X, 'X_z'), (M, 'M_z'), (Y, 'Y_raw')]:
        s = tmp[col].std()
        if s > 0:
            tmp[alias] = (tmp[col] - tmp[col].mean()) / s
        else:
            return None

    try:
        # ── a-path: M_z ~ X_z + covariates + (1|id) ──
        a_model  = smf.mixedlm(f'M_z ~ X_z + {cov_str}', tmp, groups=tmp['id'])
        a_result = a_model.fit(reml=True, method='lbfgs')
        a      = a_result.params['X_z']
        a_se   = a_result.bse['X_z']
        a_p    = a_result.pvalues['X_z']

        # ── b-path: Y ~ X_z + M_z + covariates + (1|id) ──
        b_model  = smf.mixedlm(f'{Y} ~ X_z + M_z + {cov_str}', tmp, groups=tmp['id'])
        b_result = b_model.fit(reml=True, method='lbfgs')
        b      = b_result.params['M_z']
        b_se   = b_result.bse['M_z']
        b_p    = b_result.pvalues['M_z']
        c_prime      = b_result.params['X_z']
        c_prime_se   = b_result.bse['X_z']
        c_prime_p    = b_result.pvalues['X_z']

        # ── Total effect: c-path (Y ~ X_z only) ──
        c_model  = smf.mixedlm(f'{Y} ~ X_z + {cov_str}', tmp, groups=tmp['id'])
        c_result = c_model.fit(reml=True, method='lbfgs')
        c      = c_result.params['X_z']
        c_p    = c_result.pvalues['X_z']

        # ── Indirect effect = a × b ──
        indirect = a * b

        # ── Monte Carlo CI for a×b ──
        a_samp = rng.normal(a, a_se, n_mc)
        b_samp = rng.normal(b, b_se, n_mc)
        ab_dist = a_samp * b_samp
        ci_lo, ci_hi = np.percentile(ab_dist, [2.5, 97.5])

        # Proportion mediated
        prop_med = indirect / c if abs(c) > 0.001 else np.nan

        return {
            'mediator':    M,
            'a':           a,       'a_se':  a_se,   'a_p':  a_p,
            'b':           b,       'b_se':  b_se,   'b_p':  b_p,
            'c':           c,       'c_p':   c_p,
            'c_prime':     c_prime, 'c_prime_se': c_prime_se, 'c_prime_p': c_prime_p,
            'indirect':    indirect,
            'CI_lo':       ci_lo,  'CI_hi': ci_hi,
            'sig_MC':      not (ci_lo <= 0 <= ci_hi),  # CI excludes 0?
            'prop_mediated': prop_med,
            'n_obs':       len(tmp),
            'n_subjects':  tmp['id'].nunique(),
        }
    except Exception as e:
        return None

# ─────────────────────────────────────────────
# 4. Run Mediation for each X × M combination
# ─────────────────────────────────────────────
all_results = {}

for X, cfg in PREDICTORS.items():
    print(f"\n{'='*60}")
    print(f"  Predictor: {X}")
    print(f"  a-path: M ~ {X}_z + time + age + sex + Dx + {cfg['extra_cov']} + (1|id)")
    print(f"  b-path: SSI ~ {X}_z + M_z + time + age + sex + Dx + {cfg['extra_cov']} + (1|id)")
    print(f"{'='*60}")

    results = []
    for i, M in enumerate(mediators):
        if (i+1) % 15 == 0 or i == 0:
            print(f"  [{i+1}/{len(mediators)}] {M}")
        res = multilevel_mediation(sub, X, M, OUTCOME, cfg['extra_cov'])
        if res is not None:
            results.append(res)

    res_df = pd.DataFrame(results)

    # FDR correction on indirect effect (use p from both a and b paths combined)
    # Use the max(a_p, b_p) as conservative p for indirect effect
    res_df['p_indirect_proxy'] = res_df[['a_p', 'b_p']].max(axis=1)
    _, fdr_q, _, _ = multipletests(res_df['p_indirect_proxy'], method='fdr_bh')
    res_df['q_fdr'] = fdr_q

    res_df = res_df.sort_values('indirect', key=abs, ascending=False)
    all_results[X] = res_df

# ─────────────────────────────────────────────
# 5. Print Results
# ─────────────────────────────────────────────
def print_mediation(res_df, X):
    sig = res_df[res_df['sig_MC']].copy()
    print(f"\n{'='*70}")
    print(f"  X = {X}  (Monte Carlo significant indirect effects)")
    print(f"  Total tested: {len(res_df)} | Sig (MC 95%CI): {len(sig)}")
    print(f"{'='*70}")
    print(f"  {'Mediator':<12} {'a':>7} {'b':>7} {'Indirect':>10} {'95%CI':>20} {'PropMed':>9} {'q_fdr':>8}")
    print(f"  {'-'*75}")

    show = res_df.head(20)
    for _, row in show.iterrows():
        ci_str = f"[{row['CI_lo']:+.3f}, {row['CI_hi']:+.3f}]"
        flag   = " ★" if row['sig_MC'] else ""
        pm_str = f"{row['prop_mediated']:.2f}" if pd.notna(row['prop_mediated']) else "  N/A"
        print(f"  {row['mediator']:<12} {row['a']:>7.3f} {row['b']:>7.3f} "
              f"{row['indirect']:>10.4f} {ci_str:>20} {pm_str:>9} {row['q_fdr']:>8.4f}{flag}")

    if len(sig) > 0:
        print(f"\n  → Significant mediators:")
        for _, row in sig.iterrows():
            direction = "↑SSI" if row['indirect'] > 0 else "↓SSI"
            print(f"     {row['mediator']}: {X} →[a={row['a']:.3f}]→ {row['mediator']} →[b={row['b']:.3f}]→ SSI  "
                  f"(indirect={row['indirect']:.4f}, {direction}, "
                  f"PropMed={row['prop_mediated']:.1%} if pd.notna(row['prop_mediated']) else '')")

for X, res_df in all_results.items():
    print_mediation(res_df, X)

# ─────────────────────────────────────────────
# 6. Save
# ─────────────────────────────────────────────
import os
os.makedirs('/home/user/PRISM-V/results', exist_ok=True)

for X, res_df in all_results.items():
    fname = X.replace('F0final_sma_', 'F0_').replace('liwc_', 'liwc_')
    res_df.to_csv(f'/home/user/PRISM-V/results/mediation_{fname}.csv', index=False)

print("\n\n✓ Results saved.")
