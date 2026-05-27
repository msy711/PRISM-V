"""
Figure 1 (v2_summary_significant): Arial font, 큰 제목 제거
"""

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.font_manager as fm
import warnings
warnings.filterwarnings('ignore')

# ── Arial 폰트 등록 ──────────────────────────────
fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial.ttf')
fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial_Bold.ttf')
plt.rcParams['font.family'] = 'Arial'

OUT = '/home/user/PRISM-V/results'

f0_res   = pd.read_csv(f'{OUT}/lme_v2_F0_SSI_results.csv')
liwc_res = pd.read_csv(f'{OUT}/lme_v2_LIWC_SSI_results.csv')

def sig_marker(row):
    if row['p_fdr'] < 0.001: return '★★★ FDR', '#C0392B'
    if row['p_fdr'] < 0.01:  return '★★ FDR',  '#C0392B'
    if row['p_fdr'] < 0.05:  return '★ FDR',   '#C0392B'
    if row['p_value'] < 0.001: return '***', '#E67E22'
    if row['p_value'] < 0.01:  return '**',  '#E67E22'
    if row['p_value'] < 0.05:  return '*',   '#E67E22'
    if row['p_value'] < 0.10:  return '.',   '#F39C12'
    return '', '#BDC3C7'

def bar_color(row):
    if row['p_fdr'] < 0.05:   return '#922B21'
    if row['p_value'] < 0.05: return '#E67E22' if row['coef'] > 0 else '#2980B9'
    if row['p_value'] < 0.10: return '#F39C12' if row['coef'] > 0 else '#5DADE2'
    return '#BDC3C7'

legend_patches = [
    mpatches.Patch(color='#922B21', alpha=0.85, label='FDR significant (q < 0.05)'),
    mpatches.Patch(color='#E67E22', alpha=0.85, label='p < 0.05 (positive β)'),
    mpatches.Patch(color='#2980B9', alpha=0.85, label='p < 0.05 (negative β)'),
    mpatches.Patch(color='#F39C12', alpha=0.85, label='Trend p < 0.10'),
]

fig, axes = plt.subplots(1, 2, figsize=(16, 9))

panels = [
    (liwc_res[liwc_res['p_value'] < 0.05].sort_values('coef'), 'LIWC',
     'SSI ~ feat_z + time + age + sex + C(Dx) + edu_yrs + (1|pt)'),
    (f0_res[f0_res['p_value'] < 0.10].sort_values('coef'), 'F0',
     'SSI ~ feat_z + time + age + sex + C(Dx) + AP_dose + (1|pt)')
]

for ax, (res_sorted, ftype, subtitle) in zip(axes, panels):
    if len(res_sorted) == 0:
        ax.text(0.5, 0.5, 'No significant features', ha='center', va='center', fontsize=12)
        ax.set_title(ftype)
        continue

    y = np.arange(len(res_sorted))
    clrs = [bar_color(r) for _, r in res_sorted.iterrows()]

    ax.barh(y, res_sorted['coef'], color=clrs, alpha=0.85, edgecolor='white', height=0.72)
    ax.errorbar(res_sorted['coef'], y,
                xerr=[res_sorted['coef'] - res_sorted['CI_lower'],
                      res_sorted['CI_upper'] - res_sorted['coef']],
                fmt='none', ecolor='#2C3E50', elinewidth=1.2, capsize=3, alpha=0.75)
    ax.axvline(0, color='#2C3E50', lw=1, ls='--', alpha=0.35)

    raw_labels = list(res_sorted['feature'])
    cleaned = [r.replace('liwc_','').replace('F0final_sma_','').replace('_',' ').title()
               for r in raw_labels]
    ax.set_yticks(y)
    ax.set_yticklabels(cleaned, fontsize=9.5)

    for i, (_, row) in enumerate(res_sorted.iterrows()):
        m, c = sig_marker(row)
        if m:
            ax.text(row['CI_upper'] + 0.04, i, m, va='center', fontsize=8,
                    color=c, fontweight='bold')

    ax.set_xlabel('Standardized β (z-scored feature)', fontsize=10.5)
    ax.set_title(f'{ftype} Features → SSI\n{subtitle}', fontsize=10.5, fontweight='bold')
    ax.legend(handles=legend_patches, fontsize=8.5, loc='lower right', framealpha=0.85)
    ax.grid(axis='x', alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

# 큰 제목 없음
plt.tight_layout()
plt.savefig(f'{OUT}/v2_summary_significant.png', dpi=150, bbox_inches='tight')
plt.close()
print("✓ v2_summary_significant.png (Arial, no suptitle)")
