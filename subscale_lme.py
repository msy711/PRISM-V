"""
Subscale-level LME: Literature-based Factor Scores → SSI / liwc_death / F0_qregc3
====================================================================================

Factor structures from key references:

HAMD-17 (Maier & Philipp 1985; Shafer 2006):
  - Core Depression : 1, 2, 3, 7, 8   (mood, guilt, suicide, work, retardation)
  - Anxiety         : 9, 10, 11        (agitation, psychic/somatic anxiety)
  - Somatic         : 12, 13, 14, 15, 16 (GI, general, genital, hypochondriasis, weight)
  - Sleep           : 4, 5, 6          (early/middle/late insomnia)
  Note: HAMD17 (insight) left out (often not summed in factor scores)

PHQ-9 (Lamers et al. 2018; Kroenke & Spitzer 2002):
  - Cognitive/Affective : 1, 2, 6, 9  (anhedonia, depressed mood, worthlessness, SI)
  - Somatic             : 3, 4, 5, 7, 8 (sleep, fatigue, appetite, concentration, psychomotor)

BAI-21 (Hewitt & Norton 1993; Beck et al. 1988):
  - Somatic/Neurophysiological : 1, 2, 3, 6, 7, 8, 12, 13, 17, 18, 19, 20, 21
  - Cognitive/Subjective       : 4, 5, 9, 10, 11, 14, 15, 16

BHS-20 (Aish & Wasserman 2001; Beck et al. 1974):
  - Hopeful (reversed direction, higher = more hopeful):
      1(R), 3(R), 5(R), 6(R), 8(R), 10(R), 13(R), 15(R), 19(R)
  - Hopeless (higher = more hopelessness):
      2, 4, 7, 9, 11, 12, 14, 16, 17, 18, 20
  → 역채점 후 전체 합산 = BHS_Total (일원적 구조; Kliem et al. 2018 지지)
  → 또는 2-factor: BHS_Hopeful / BHS_Hopeless

Total subscales: 4(HAMD) + 2(PHQ) + 2(BAI) + 2(BHS) = 10개
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

# ─────────────────────────────────────────────
# 2. Define subscales
# ─────────────────────────────────────────────

# BHS reversed items (score = 0→hopeless, 1→hopeful; or need to flip so higher=worse)
BHS_REVERSED = [1, 3, 5, 6, 8, 10, 13, 15, 19]  # item numbers

def bhs_item(n, df):
    """BHOL n-th item, reversed if necessary (so higher = more hopelessness)"""
    col = f'BHOL{n}'
    vals = pd.to_numeric(df[col], errors='coerce')
    if n in BHS_REVERSED:
        # BHS reversed items: original 0=hopeless, 1=hopeful → flip to 0=hopeful, 1=hopeless
        # Assuming binary 0/1 scoring
        max_val = vals.max()
        return max_val - vals
    return vals

SUBSCALES = {
    # ── HAMD-17 (Maier & Philipp 1985) ──────────────────────
    'HAMD_Depression': {
        'items': ['HAMD1','HAMD2','HAMD3','HAMD7','HAMD8'],
        'ref': 'Maier & Philipp 1985',
        'desc': 'Depressed mood, Guilt, Suicidal ideation, Work, Retardation'
    },
    'HAMD_Anxiety': {
        'items': ['HAMD9','HAMD10','HAMD11'],
        'ref': 'Maier & Philipp 1985',
        'desc': 'Agitation, Psychic anxiety, Somatic anxiety'
    },
    'HAMD_Somatic': {
        'items': ['HAMD12','HAMD13','HAMD14','HAMD15','HAMD16'],
        'ref': 'Maier & Philipp 1985',
        'desc': 'GI somatic, General somatic, Genital, Hypochondriasis, Weight'
    },
    'HAMD_Sleep': {
        'items': ['HAMD4','HAMD5','HAMD6'],
        'ref': 'Maier & Philipp 1985',
        'desc': 'Early/Middle/Late insomnia'
    },
    # ── PHQ-9 (Lamers et al. 2018) ───────────────────────────
    'PHQ_CogAffect': {
        'items': ['PHQ1','PHQ2','PHQ6','PHQ9'],
        'ref': 'Lamers et al. 2018',
        'desc': 'Anhedonia, Depressed mood, Worthlessness, Suicidal thoughts'
    },
    'PHQ_Somatic': {
        'items': ['PHQ3','PHQ4','PHQ5','PHQ7','PHQ8'],
        'ref': 'Lamers et al. 2018',
        'desc': 'Sleep, Fatigue, Appetite, Concentration, Psychomotor'
    },
    # ── BAI-21 (Hewitt & Norton 1993) ────────────────────────
    'BAI_Somatic': {
        'items': ['BAI1','BAI2','BAI3','BAI6','BAI7','BAI8',
                  'BAI12','BAI13','BAI17','BAI18','BAI19','BAI20','BAI21'],
        'ref': 'Hewitt & Norton 1993',
        'desc': 'Neurophysiological/somatic symptoms (numbness, hot, wobbly, dizzy, pounding, unsteady, trembling, shaky, scared, indigestion, faint, flushed, sweats)'
    },
    'BAI_Cognitive': {
        'items': ['BAI4','BAI5','BAI9','BAI10','BAI11','BAI14','BAI15','BAI16'],
        'ref': 'Hewitt & Norton 1993',
        'desc': 'Cognitive/subjective anxiety (unable to relax, fear of worst, terrified, nervous, choking, fear losing control, dyspnea, fear dying)'
    },
    # ── BHS-20 (Aish & Wasserman 2001) ───────────────────────
    'BHS_Hopeless': {
        'items': ['BHOL2','BHOL4','BHOL7','BHOL9','BHOL11',
                  'BHOL12','BHOL14','BHOL16','BHOL17','BHOL18','BHOL20'],
        'ref': 'Aish & Wasserman 2001',
        'desc': 'Negative hopelessness items (higher = more hopeless)'
    },
    'BHS_Hopeful': {
        'items': ['BHOL1','BHOL3','BHOL5','BHOL6','BHOL8',
                  'BHOL10','BHOL13','BHOL15','BHOL19'],
        'ref': 'Aish & Wasserman 2001',
        'desc': 'Positive/hopeful items — reversed scored (higher raw = more hopeful)'
    },
}

# ─────────────────────────────────────────────
# 3. Compute subscale scores
# ─────────────────────────────────────────────
print("=== Subscale Structure ===")
for name, cfg in SUBSCALES.items():
    print(f"  {name:<22}: {len(cfg['items'])} items  [{cfg['ref']}]")
    print(f"    → {cfg['desc']}")
print()

for name, cfg in SUBSCALES.items():
    items = cfg['items']
    if name == 'BHS_Hopeful':
        # reverse-score so higher = more hopelessness (consistent direction)
        # max per item assumed to be 1 (binary BHS)
        cols = []
        for itm in items:
            n = int(itm.replace('BHOL',''))
            c = f'BHOL{n}_rev'
            df[c] = pd.to_numeric(df[itm], errors='coerce')
            max_v = df[c].max()
            df[c] = max_v - df[c]
            cols.append(c)
        df[name] = df[cols].sum(axis=1)
    else:
        df[name] = df[items].apply(pd.to_numeric, errors='coerce').sum(axis=1)

SUBSCALE_NAMES = list(SUBSCALES.keys())

# Check subscale descriptives
print("=== Subscale Descriptives ===")
sub_check = df[SUBSCALE_NAMES + ['SSI']].describe().loc[['mean','std','min','max']].round(2)
print(sub_check.T.to_string())
print()

# ─────────────────────────────────────────────
# 4. Complete cases
# ─────────────────────────────────────────────
needed = ['SSI','liwc_death','F0final_sma_qregc3','id',
          'time_numeric','age','sex','Dx','AP_dose','edu_yrs'] + SUBSCALE_NAMES
sub = df[needed].dropna().reset_index(drop=True)
print(f"Complete cases: {len(sub)} rows | {sub['id'].nunique()} patients")

# ─────────────────────────────────────────────
# 5. LME function
# ─────────────────────────────────────────────
BASE = 'time_numeric + age + sex + C(Dx, Treatment("BPII"))'

OUTCOMES = {
    'SSI':                {'cov': BASE,               'label': 'SSI (Suicidal Ideation)'},
    'liwc_death':         {'cov': BASE + ' + edu_yrs', 'label': 'liwc_death (Death-related language)'},
    'F0final_sma_qregc3': {'cov': BASE + ' + AP_dose', 'label': 'F0_qregc3 (Pitch curvature)'},
}

def run_lme(df, outcome, cov_str, subscales):
    df = df.copy()
    y_s = df[outcome].std()
    df['Y'] = (df[outcome] - df[outcome].mean()) / y_s

    results = []
    for sc in subscales:
        tmp = df[['id','Y', sc, 'time_numeric','age','sex','Dx','AP_dose','edu_yrs']].dropna().copy()
        std = tmp[sc].std()
        if std == 0: continue
        tmp['sc_z'] = (tmp[sc] - tmp[sc].mean()) / std

        try:
            res = smf.mixedlm(f'Y ~ sc_z + {cov_str}', tmp, groups=tmp['id'])\
                      .fit(reml=True, method='lbfgs')
            results.append({
                'subscale': sc,
                'coef':  res.params['sc_z'],
                'SE':    res.bse['sc_z'],
                'CI_lo': res.conf_int().loc['sc_z', 0],
                'CI_hi': res.conf_int().loc['sc_z', 1],
                'p':     res.pvalues['sc_z'],
                'n': len(tmp), 'n_subj': tmp['id'].nunique(),
            })
        except:
            pass

    res_df = pd.DataFrame(results)
    _, q, _, _ = multipletests(res_df['p'], method='fdr_bh')
    res_df['q_fdr'] = q
    return res_df.sort_values('p').reset_index(drop=True)

# ─────────────────────────────────────────────
# 6. Run & print
# ─────────────────────────────────────────────
all_res = {}
for outcome, cfg in OUTCOMES.items():
    short = outcome.replace('F0final_sma_','F0_')
    print(f"\n{'='*65}")
    print(f"  Outcome: {cfg['label']}")
    print(f"{'='*65}")
    res = run_lme(sub, outcome, cfg['cov'], SUBSCALE_NAMES)
    all_res[outcome] = res

    print(f"  {'Subscale':<22} {'β':>7} {'SE':>6} {'p':>9} {'q_FDR':>9}  Sig")
    print(f"  {'-'*60}")
    for _, row in res.iterrows():
        sig = '★ FDR' if row['q_fdr'] < 0.05 else ('* nom' if row['p'] < 0.05 else '')
        print(f"  {row['subscale']:<22} {row['coef']:>7.3f} {row['SE']:>6.3f} "
              f"{row['p']:>9.4f} {row['q_fdr']:>9.4f}  {sig}")

# ─────────────────────────────────────────────
# 7. Overlap
# ─────────────────────────────────────────────
print(f"\n{'='*65}")
print("  OVERLAP (FDR q < 0.05)")
print(f"{'='*65}")

sig = {k: set(v[v['q_fdr'] < 0.05]['subscale']) for k,v in all_res.items()}
nom = {k: set(v[v['p']     < 0.05]['subscale']) for k,v in all_res.items()}

ssi_key = 'SSI'; liwc_key = 'liwc_death'; f0_key = 'F0final_sma_qregc3'
for k, s in sig.items():
    print(f"  {k.replace('F0final_sma_','F0_')}: {sorted(s)}")

print(f"\n  SSI ∩ liwc   (FDR): {sorted(sig[ssi_key] & sig[liwc_key])}")
print(f"  SSI ∩ F0     (FDR): {sorted(sig[ssi_key] & sig[f0_key])}")
print(f"  All three    (FDR): {sorted(sig[ssi_key] & sig[liwc_key] & sig[f0_key])}")
print(f"\n  SSI ∩ liwc   (nom): {sorted(nom[ssi_key] & nom[liwc_key])}")
print(f"  SSI ∩ F0     (nom): {sorted(nom[ssi_key] & nom[f0_key])}")
print(f"  All three    (nom): {sorted(nom[ssi_key] & nom[liwc_key] & nom[f0_key])}")

# ─────────────────────────────────────────────
# 8. Save
# ─────────────────────────────────────────────
import os
os.makedirs('/home/user/PRISM-V/results', exist_ok=True)
for outcome, res in all_res.items():
    short = outcome.replace('F0final_sma_','F0_')
    res.to_csv(f'/home/user/PRISM-V/results/subscale_lme_{short}.csv', index=False)

# wide table
wide = all_res[ssi_key][['subscale','coef','q_fdr','p']].rename(
    columns={'coef':'b_SSI','q_fdr':'q_SSI','p':'p_SSI'})
for k, res in all_res.items():
    if k == ssi_key: continue
    sh = 'liwc' if 'liwc' in k else 'F0'
    wide = wide.merge(res[['subscale','coef','q_fdr','p']].rename(
        columns={'coef':f'b_{sh}','q_fdr':f'q_{sh}','p':f'p_{sh}'}), on='subscale')
wide.to_csv('/home/user/PRISM-V/results/subscale_lme_wide.csv', index=False)
print(f"\n✓ Saved to /home/user/PRISM-V/results/")
