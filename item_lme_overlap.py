"""
Item-level LME: 공통 임상 기반 분석
=====================================
3개의 outcome 각각에 대해 univariate LME 실행:
  1. SSI       ~ item_z + time + age + sex + C(Dx) + (1|id)
  2. liwc_death ~ item_z + time + age + sex + C(Dx) + edu_yrs + (1|id)
  3. F0_qregc3  ~ item_z + time + age + sex + C(Dx) + AP_dose + (1|id)

교집합: 세 outcome 모두(또는 두 개)에서 유의한 items
→ "liwc_death와 SSI를 함께 설명하는 공통 임상 표현"
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
df = pd.read_csv('/home/user/PRISM-V/data/prism_softImpute4_smile_with_items.csv', low_memory=False)
df['BHOL9'] = pd.to_numeric(df['BHOL9'], errors='coerce')
df['time_numeric'] = df['case_episode'].map({'baseline':0,'2m':2,'4m':4,'8m':8,'12m':12})
df['Dx'] = df['Dx'].astype(str)

ITEMS = ([f'HAMD{i}' for i in range(1,18)] +
         [f'PHQ{i}'  for i in range(1,10)] +
         [f'BAI{i}'  for i in range(1,22)] +
         [f'BHOL{i}' for i in range(1,21)])

needed = ['SSI','liwc_death','F0final_sma_qregc3','id',
          'time_numeric','age','sex','Dx','AP_dose','edu_yrs'] + ITEMS
sub = df[[c for c in needed if c in df.columns]].dropna().reset_index(drop=True)

print(f"Complete cases: {len(sub)} rows | {sub['id'].nunique()} patients")
print(f"Items tested: {len(ITEMS)}")

# ─────────────────────────────────────────────
# 2. Univariate LME function
# ─────────────────────name───────────────────
BASE = 'time_numeric + age + sex + C(Dx, Treatment("BPII"))'

OUTCOMES = {
    'SSI':              {'formula_cov': BASE,              'item_std': True},
    'liwc_death':       {'formula_cov': BASE + ' + edu_yrs','item_std': True},
    'F0final_sma_qregc3': {'formula_cov': BASE + ' + AP_dose','item_std': True},
}

def run_lme_items(df, outcome, cov_str, items):
    results = []
    # outcome z-score (for comparable β across outcomes)
    y_std = df[outcome].std()
    y_mean = df[outcome].mean()
    df = df.copy()
    df['Y'] = (df[outcome] - y_mean) / y_std

    for item in items:
        tmp = df[['id','Y', item, 'time_numeric','age','sex','Dx','AP_dose','edu_yrs']].dropna().copy()
        std = tmp[item].std()
        if std == 0: continue
        tmp['item_z'] = (tmp[item] - tmp[item].mean()) / std

        try:
            formula = f'Y ~ item_z + {cov_str}'
            res = smf.mixedlm(formula, tmp, groups=tmp['id']).fit(reml=True, method='lbfgs')
            results.append({
                'item':   item,
                'coef':   res.params['item_z'],
                'SE':     res.bse['item_z'],
                'CI_lo':  res.conf_int().loc['item_z', 0],
                'CI_hi':  res.conf_int().loc['item_z', 1],
                'p':      res.pvalues['item_z'],
                'n':      len(tmp),
            })
        except:
            pass

    res_df = pd.DataFrame(results)
    _, q, _, _ = multipletests(res_df['p'], method='fdr_bh')
    res_df['q_fdr'] = q
    return res_df.sort_values('p').reset_index(drop=True)

# ─────────────────────────────────────────────
# 3. Run
# ─────────────────────────────────────────────
all_res = {}
for outcome, cfg in OUTCOMES.items():
    label = outcome.replace('F0final_sma_','F0_')
    print(f"\n[{label}] running {len(ITEMS)} LME models...")
    res = run_lme_items(sub, outcome, cfg['formula_cov'], ITEMS)
    all_res[outcome] = res
    sig_fdr = (res['q_fdr'] < 0.05).sum()
    sig_nom = (res['p'] < 0.05).sum()
    print(f"  FDR sig: {sig_fdr}  |  nominal sig: {sig_nom}")

# ─────────────────────────────────────────────
# 4. Overlap analysis
# ─────────────────────────────────────────────
# FDR < 0.05 기준 교집합
sig_sets = {}
for outcome, res in all_res.items():
    sig_sets[outcome] = set(res[res['q_fdr'] < 0.05]['item'])

# nominal p < 0.05 기준 교집합
nom_sets = {}
for outcome, res in all_res.items():
    nom_sets[outcome] = set(res[res['p'] < 0.05]['item'])

print("\n" + "="*65)
print("  OVERLAP ANALYSIS")
print("="*65)

for outcome, s in sig_sets.items():
    lbl = outcome.replace('F0final_sma_','F0_')
    print(f"  FDR sig ({lbl}): {len(s)}  → {sorted(s)}")

ssi_set   = sig_sets['SSI']
liwc_set  = sig_sets['liwc_death']
f0_set    = sig_sets['F0final_sma_qregc3']

# 교집합
ssi_liwc  = ssi_set & liwc_set
ssi_f0    = ssi_set & f0_set
liwc_f0   = liwc_set & f0_set
all_three = ssi_set & liwc_set & f0_set

print(f"\n  SSI ∩ liwc_death   : {len(ssi_liwc)}  → {sorted(ssi_liwc)}")
print(f"  SSI ∩ F0_qregc3   : {len(ssi_f0)}   → {sorted(ssi_f0)}")
print(f"  liwc ∩ F0         : {len(liwc_f0)}   → {sorted(liwc_f0)}")
print(f"  All three         : {len(all_three)} → {sorted(all_three)}")

# nominal 기준도 확인
print("\n  --- Nominal (p<0.05) 기준 ---")
ssi_liwc_nom  = nom_sets['SSI'] & nom_sets['liwc_death']
ssi_f0_nom    = nom_sets['SSI'] & nom_sets['F0final_sma_qregc3']
all_three_nom = nom_sets['SSI'] & nom_sets['liwc_death'] & nom_sets['F0final_sma_qregc3']
print(f"  SSI ∩ liwc_death (nom) : {len(ssi_liwc_nom)}  → {sorted(ssi_liwc_nom)}")
print(f"  SSI ∩ F0_qregc3  (nom) : {len(ssi_f0_nom)}   → {sorted(ssi_f0_nom)}")
print(f"  All three        (nom) : {len(all_three_nom)} → {sorted(all_three_nom)}")

# ─────────────────────────────────────────────
# 5. Build merged result table
# ─────────────────────────────────────────────
# 각 outcome의 결과를 item 기준으로 wide format으로 합치기
merged = all_res['SSI'][['item','coef','q_fdr','p']].rename(
    columns={'coef':'coef_SSI','q_fdr':'q_SSI','p':'p_SSI'})

for outcome, res in all_res.items():
    if outcome == 'SSI': continue
    short = 'liwc' if 'liwc' in outcome else 'F0'
    sub2 = res[['item','coef','q_fdr','p']].rename(
        columns={'coef':f'coef_{short}','q_fdr':f'q_{short}','p':f'p_{short}'})
    merged = merged.merge(sub2, on='item', how='outer')

# 멤버십 플래그
merged['sig_SSI']  = merged['q_SSI']  < 0.05
merged['sig_liwc'] = merged['q_liwc'] < 0.05
merged['sig_F0']   = merged['q_F0']   < 0.05
merged['n_sig']    = merged[['sig_SSI','sig_liwc','sig_F0']].sum(axis=1)

merged = merged.sort_values(['n_sig','p_SSI'], ascending=[False,True])

# ─────────────────────────────────────────────
# 6. Save
# ─────────────────────────────────────────────
import os
os.makedirs('/home/user/PRISM-V/results', exist_ok=True)

for outcome, res in all_res.items():
    short = outcome.replace('F0final_sma_','F0_').replace('liwc_','liwc_')
    res.to_csv(f'/home/user/PRISM-V/results/item_lme_{short}.csv', index=False)

merged.to_csv('/home/user/PRISM-V/results/item_lme_overlap.csv', index=False)

print(f"\n✓ Saved results to /home/user/PRISM-V/results/")

# ─────────────────────────────────────────────
# 7. Print top overlap items
# ─────────────────────────────────────────────
ITEM_LABELS = {
    'HAMD1':'HAMD1: Depressed mood',        'HAMD2':'HAMD2: Guilt',
    'HAMD3':'HAMD3: Suicidal ideation',     'HAMD4':'HAMD4: Early insomnia',
    'HAMD5':'HAMD5: Middle insomnia',       'HAMD6':'HAMD6: Late insomnia',
    'HAMD7':'HAMD7: Work & activities',     'HAMD8':'HAMD8: Retardation',
    'HAMD9':'HAMD9: Agitation',             'HAMD10':'HAMD10: Psychic anxiety',
    'HAMD11':'HAMD11: Somatic anxiety',
    'PHQ1':'PHQ1: Anhedonia',               'PHQ2':'PHQ2: Depressed mood',
    'PHQ3':'PHQ3: Sleep problems',          'PHQ4':'PHQ4: Fatigue',
    'PHQ5':'PHQ5: Appetite change',         'PHQ6':'PHQ6: Worthlessness',
    'PHQ7':'PHQ7: Concentration',           'PHQ8':'PHQ8: Psychomotor',
    'PHQ9':'PHQ9: Suicidal thoughts',
    'BAI1':'BAI1: Numbness',                'BAI4':'BAI4: Unable to relax',
    'BAI5':'BAI5: Fear of worst',           'BAI7':'BAI7: Heart pounding',
    'BAI8':'BAI8: Unsteady',                'BAI9':'BAI9: Terrified',
    'BAI10':'BAI10: Nervous',               'BAI11':'BAI11: Choking',
    'BAI14':'BAI14: Fear losing control',   'BAI15':'BAI15: Dyspnea',
    'BAI17':'BAI17: Scared',               'BAI19':'BAI19: Faint',
    'BHOL1':'BHS1: Optimism(R)',            'BHOL2':'BHS2: Giving up',
    'BHOL3':'BHS3: Things going well(R)',   'BHOL4':'BHS4: No future',
    'BHOL5':'BHS5: Time to achieve',        'BHOL6':'BHS6: Expected success(R)',
    'BHOL7':'BHS7: Bleak future',           'BHOL9':'BHS9: Cannot get what I want',
    'BHOL10':'BHS10: Problems persist',     'BHOL11':'BHS11: No interest in future',
    'BHOL12':'BHS12: Things won\'t work',   'BHOL13':'BHS13: Will succeed(R)',
    'BHOL14':'BHS14: Things go badly',      'BHOL16':'BHS16: Never get what I want',
    'BHOL17':'BHS17: Unlikely happy(R)',    'BHOL18':'BHS18: Uncertain future',
    'BHOL19':'BHS19: More sad than happy(R)','BHOL20':'BHS20: No point trying',
}

print("\n" + "="*75)
print("  Items significant in ≥2 outcomes (FDR<0.05)")
print("="*75)
print(f"  {'Item':<10} {'Label':<35} {'β_SSI':>7} {'β_liwc':>8} {'β_F0':>7}  Sig in")
print(f"  {'-'*72}")
for _, row in merged[merged['n_sig'] >= 2].iterrows():
    lbl = ITEM_LABELS.get(row['item'], row['item'])[:35]
    flags = []
    if row['sig_SSI']:  flags.append('SSI')
    if row['sig_liwc']: flags.append('liwc')
    if row['sig_F0']:   flags.append('F0')
    b_ssi  = f"{row['coef_SSI']:.3f}"  if pd.notna(row.get('coef_SSI'))  else '  N/A'
    b_liwc = f"{row['coef_liwc']:.3f}" if pd.notna(row.get('coef_liwc')) else '  N/A'
    b_f0   = f"{row['coef_F0']:.3f}"   if pd.notna(row.get('coef_F0'))   else '  N/A'
    print(f"  {row['item']:<10} {lbl:<35} {b_ssi:>7} {b_liwc:>8} {b_f0:>7}  {'+'.join(flags)}")
