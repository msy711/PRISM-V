"""
SSI Item-level LME: liwc_death & F0_qregc3 → each SSI item (SSI1–19)
=======================================================================

Models:
  SSI_item ~ liwc_death_z + time + age + sex + C(Dx) + edu_yrs + (1|id)
  SSI_item ~ F0_qregc3_z + time + age + sex + C(Dx) + AP_dose + (1|id)

FDR correction across 19 items per predictor.
"""

import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
from statsmodels.stats.multitest import multipletests
import warnings
warnings.filterwarnings('ignore')

# ─────────────────────────────────────────────
# 1. Data
# ─────────────────────────────────────────────
df = pd.read_csv('/home/user/PRISM-V/data/prism_softImpute4_smile_with_items.csv',
                 low_memory=False)
df['BHOL9']        = pd.to_numeric(df['BHOL9'], errors='coerce')
df['time_numeric'] = df['case_episode'].map(
    {'baseline': 0, '2m': 2, '4m': 4, '8m': 8, '12m': 12})
df['Dx'] = df['Dx'].astype(str)

SSI_ITEMS = [f'SSI{i}' for i in range(1, 20)]

# coerce SSI items to numeric
for col in SSI_ITEMS:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors='coerce')

needed = ['liwc_death', 'F0final_sma_qregc3', 'id',
          'time_numeric', 'age', 'sex', 'Dx', 'AP_dose', 'edu_yrs'] + SSI_ITEMS
sub = df[[c for c in needed if c in df.columns]].dropna().reset_index(drop=True)

print(f"Complete cases: {len(sub)} rows | {sub['id'].nunique()} patients")
print(f"SSI items tested: {len(SSI_ITEMS)}")

# ─────────────────────────────────────────────
# 2. SSI item labels
# ─────────────────────────────────────────────
SSI_LABELS = {
    'SSI1':  'SSI1: Wish to be dead',
    'SSI2':  'SSI2: Wish to attempt suicide',
    'SSI3':  'SSI3: Reasons for living',
    'SSI4':  'SSI4: Deterrents to attempt',
    'SSI5':  'SSI5: Duration of ideation',
    'SSI6':  'SSI6: Frequency of ideation',
    'SSI7':  'SSI7: Attitude toward ideation',
    'SSI8':  'SSI8: Control over suicidal action',
    'SSI9':  'SSI9: Deterrents to attempt',
    'SSI10': 'SSI10: Reasons for wanting attempt',
    'SSI11': 'SSI11: Method specificity',
    'SSI12': 'SSI12: Method availability/opportunity',
    'SSI13': 'SSI13: Sense of capability',
    'SSI14': 'SSI14: Expectancy of attempt',
    'SSI15': 'SSI15: Imminence of attempt',
    'SSI16': 'SSI16: Actual preparation',
    'SSI17': 'SSI17: Suicide note',
    'SSI18': 'SSI18: Final acts',
    'SSI19': 'SSI19: Deception/concealment',
}

# ─────────────────────────────────────────────
# 3. LME function
# ─────────────────────────────────────────────
BASE = 'time_numeric + age + sex + C(Dx, Treatment("BPII"))'

def run_lme_ssi_items(df, predictor, cov_str, items):
    """Run LME with `predictor` as X and each SSI item as Y."""
    df = df.copy()
    pred_std  = df[predictor].std()
    pred_mean = df[predictor].mean()
    df['pred_z'] = (df[predictor] - pred_mean) / pred_std

    results = []
    for item in items:
        tmp = df[['id', item, 'pred_z',
                  'time_numeric', 'age', 'sex', 'Dx',
                  'AP_dose', 'edu_yrs']].dropna().copy()
        y_std  = tmp[item].std()
        y_mean = tmp[item].mean()
        if y_std == 0:
            continue
        tmp['Y'] = (tmp[item] - y_mean) / y_std   # z-score outcome

        fitted = None
        for method in ['lbfgs', 'powell', 'nm']:
            try:
                fitted = smf.mixedlm(
                    f'Y ~ pred_z + {cov_str}', tmp, groups=tmp['id']
                ).fit(reml=True, method=method)
                break
            except Exception:
                continue

        if fitted is not None:
            results.append({
                'item':   item,
                'coef':   fitted.params['pred_z'],
                'SE':     fitted.bse['pred_z'],
                'CI_lo':  fitted.conf_int().loc['pred_z', 0],
                'CI_hi':  fitted.conf_int().loc['pred_z', 1],
                'p':      fitted.pvalues['pred_z'],
                'n':      len(tmp),
                'n_subj': tmp['id'].nunique(),
            })
        else:
            print(f"    [WARN] {item}: all solvers failed")

    res_df = pd.DataFrame(results)
    if len(res_df) == 0:
        return res_df
    _, q, _, _ = multipletests(res_df['p'], method='fdr_bh')
    res_df['q_fdr'] = q
    return res_df.sort_values('p').reset_index(drop=True)


# ─────────────────────────────────────────────
# 4. Run
# ─────────────────────────────────────────────
MODELS = {
    'liwc_death':          BASE + ' + edu_yrs',
    'F0final_sma_qregc3':  BASE + ' + AP_dose',
}

all_res = {}
for predictor, cov in MODELS.items():
    short = predictor.replace('F0final_sma_', 'F0_')
    print(f"\n{'='*65}")
    print(f"  Predictor: {short}")
    print(f"{'='*65}")
    res = run_lme_ssi_items(sub, predictor, cov, SSI_ITEMS)
    all_res[predictor] = res

    print(f"  {'Item':<8} {'β':>7} {'SE':>6} {'p':>9} {'q_FDR':>9}  Sig")
    print(f"  {'-'*50}")
    for _, row in res.iterrows():
        sig = '★ FDR' if row['q_fdr'] < 0.05 else ('* nom' if row['p'] < 0.05 else '')
        lbl = SSI_LABELS.get(row['item'], row['item'])[:40]
        print(f"  {row['item']:<8} {row['coef']:>7.3f} {row['SE']:>6.3f} "
              f"{row['p']:>9.4f} {row['q_fdr']:>9.4f}  {sig}  {lbl}")

# ─────────────────────────────────────────────
# 5. Overlap: items sig in both predictors
# ─────────────────────────────────────────────
print(f"\n{'='*65}")
print("  OVERLAP (FDR q < 0.05)")
print(f"{'='*65}")

sig_liwc = set(all_res['liwc_death'][all_res['liwc_death']['q_fdr'] < 0.05]['item'])
sig_f0   = set(all_res['F0final_sma_qregc3'][all_res['F0final_sma_qregc3']['q_fdr'] < 0.05]['item'])
print(f"  liwc_death  FDR sig: {sorted(sig_liwc)}")
print(f"  F0_qregc3   FDR sig: {sorted(sig_f0)}")
print(f"  Intersection (FDR) : {sorted(sig_liwc & sig_f0)}")

nom_liwc = set(all_res['liwc_death'][all_res['liwc_death']['p'] < 0.05]['item'])
nom_f0   = set(all_res['F0final_sma_qregc3'][all_res['F0final_sma_qregc3']['p'] < 0.05]['item'])
print(f"\n  liwc_death  nominal: {sorted(nom_liwc)}")
print(f"  F0_qregc3   nominal: {sorted(nom_f0)}")
print(f"  Intersection (nom) : {sorted(nom_liwc & nom_f0)}")

# ─────────────────────────────────────────────
# 6. Save
# ─────────────────────────────────────────────
import os
os.makedirs('/home/user/PRISM-V/results', exist_ok=True)

for predictor, res in all_res.items():
    short = predictor.replace('F0final_sma_', 'F0_')
    out_path = f'/home/user/PRISM-V/results/ssi_item_lme_{short}.csv'
    res.to_csv(out_path, index=False)
    print(f"✓ {out_path}")

# wide merge
wide = all_res['liwc_death'][['item', 'coef', 'SE', 'CI_lo', 'CI_hi', 'p', 'q_fdr']].rename(
    columns={'coef': 'b_liwc', 'SE': 'SE_liwc',
             'CI_lo': 'CI_lo_liwc', 'CI_hi': 'CI_hi_liwc',
             'p': 'p_liwc', 'q_fdr': 'q_liwc'})

f0_wide = all_res['F0final_sma_qregc3'][['item', 'coef', 'SE', 'CI_lo', 'CI_hi', 'p', 'q_fdr']].rename(
    columns={'coef': 'b_F0', 'SE': 'SE_F0',
             'CI_lo': 'CI_lo_F0', 'CI_hi': 'CI_hi_F0',
             'p': 'p_F0', 'q_fdr': 'q_F0'})

wide = wide.merge(f0_wide, on='item', how='outer')
wide.to_csv('/home/user/PRISM-V/results/ssi_item_lme_wide.csv', index=False)
print("✓ results/ssi_item_lme_wide.csv")
