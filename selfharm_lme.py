"""
Self-injury & Suicide Behavior ~ liwc_death / F0_qregc3
==========================================================
Binary outcomes → Bayesian Mixed GLMM (logistic) via BinomialBayesMixedGLM
Excluding baseline (case_episode != 'baseline')

Models (per outcome × predictor):
  outcome ~ pred_z + time_numeric + age + sex + C(Dx) + edu_yrs/AP_dose + (1|id)
"""

import pandas as pd
import numpy as np
from statsmodels.genmod.bayes_mixed_glm import BinomialBayesMixedGLM
import scipy.stats
import warnings, os
warnings.filterwarnings('ignore')

# ─────────────────────────────────────────────
# 1. Data (non-baseline only)
# ─────────────────────────────────────────────
df = pd.read_csv('data/prism_softImpute4_smile_with_items.csv', low_memory=False)
df['time_numeric'] = df['case_episode'].map({'baseline':0,'2m':2,'4m':4,'8m':8,'12m':12})

for col in ['self_inj', 'suicide_behavior', 'liwc_death', 'F0final_sma_qregc3',
            'age', 'sex', 'edu_yrs', 'AP_dose']:
    df[col] = pd.to_numeric(df[col], errors='coerce')

df = df[df['case_episode'] != 'baseline'].copy()
print(f"Non-baseline rows: {len(df)} | patients: {df['id'].nunique()}")
print(f"self_inj prevalence:   {df['self_inj'].mean():.3f}  (n={df['self_inj'].notna().sum()})")
print(f"suicide_behavior prev: {df['suicide_behavior'].mean():.3f}  (n={df['suicide_behavior'].notna().sum()})")

# Dx dummies (reference = BPI, most common)
dx_dummies = pd.get_dummies(df['Dx'].astype(str), prefix='Dx').drop(columns=['Dx_BPI'], errors='ignore')
df = pd.concat([df, dx_dummies], axis=1)
DX_COLS = [c for c in dx_dummies.columns]

# ─────────────────────────────────────────────
# 2. GLMM function (Bayesian MAP, logistic)
# ─────────────────────────────────────────────
def run_glmm(df, outcome, predictor, extra_cov):
    cols = [outcome, predictor, 'id', 'time_numeric', 'age', 'sex', extra_cov] + DX_COLS
    tmp  = df[cols].dropna().copy()
    for c in DX_COLS:
        tmp[c] = tmp[c].astype(float)

    tmp['pred_z'] = (tmp[predictor] - tmp[predictor].mean()) / tmp[predictor].std()

    fix_terms = ['pred_z', 'time_numeric', 'age', 'sex', extra_cov] + DX_COLS
    formula   = f'{outcome} ~ ' + ' + '.join(fix_terms)

    model  = BinomialBayesMixedGLM.from_formula(formula, {'id': '0 + C(id)'}, tmp)
    result = model.fit_map()

    idx  = result.model.fep_names.index('pred_z')
    coef = result.fe_mean[idx]
    sd   = result.fe_sd[idx]
    z    = coef / sd
    p    = 2 * (1 - scipy.stats.norm.cdf(abs(z)))

    return {
        'coef':      coef,
        'SD':        sd,
        'z':         z,
        'p':         p,
        'OR':        np.exp(coef),
        'OR_CI_lo':  np.exp(coef - 1.96 * sd),
        'OR_CI_hi':  np.exp(coef + 1.96 * sd),
        'n':         len(tmp),
        'n_subj':    tmp['id'].nunique(),
    }

# ─────────────────────────────────────────────
# 3. Run all 4 models
# ─────────────────────────────────────────────
MODELS = [
    ('self_inj',         'liwc_death',         'edu_yrs'),
    ('self_inj',         'F0final_sma_qregc3', 'AP_dose'),
    ('suicide_behavior', 'liwc_death',         'edu_yrs'),
    ('suicide_behavior', 'F0final_sma_qregc3', 'AP_dose'),
]

results = []
print(f"\n{'='*75}")
print(f"  {'Outcome':<20} {'Predictor':<22} {'log-OR':>8} {'SD':>6} {'p':>8}   OR [95% CI]")
print(f"  {'-'*72}")

for outcome, predictor, extra in MODELS:
    short_pred = predictor.replace('F0final_sma_', 'F0_')
    try:
        r = run_glmm(df, outcome, predictor, extra)
        sig = '***' if r['p'] < 0.001 else ('**' if r['p'] < 0.01 else ('*' if r['p'] < 0.05 else ''))
        print(f"  {outcome:<20} {short_pred:<22} {r['coef']:>8.3f} {r['SD']:>6.3f} {r['p']:>8.4f}   "
              f"OR={r['OR']:.2f} [{r['OR_CI_lo']:.2f}–{r['OR_CI_hi']:.2f}]  {sig}")
        results.append({'outcome': outcome, 'predictor': short_pred, **r})
    except Exception as e:
        import traceback
        print(f"  {outcome:<20} {short_pred:<22} ERROR: {e}")
        traceback.print_exc()

# ─────────────────────────────────────────────
# 4. Save
# ─────────────────────────────────────────────
os.makedirs('results', exist_ok=True)
res_df = pd.DataFrame(results)
res_df.to_csv('results/selfharm_glmm.csv', index=False)
print(f"\n✓ results/selfharm_glmm.csv")
