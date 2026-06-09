"""
v2_summary_significant (invnorm version) + results tables
"""

import pandas as pd, numpy as np
import matplotlib, matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.font_manager as fm
matplotlib.use('Agg')

fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial.ttf')
fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial_Bold.ttf')
plt.rcParams['font.family'] = 'Arial'

OUT = '/home/user/PRISM-V/results'

f0_res   = pd.read_csv(f'{OUT}/lme_invnorm_F0_SSI_results.csv')
liwc_res = pd.read_csv(f'{OUT}/lme_invnorm_LIWC_SSI_results.csv')

def sig_marker(row):
    if row['p_fdr'] < 0.001:   return '[FDR***]', '#C0392B'
    if row['p_fdr'] < 0.01:    return '[FDR**]',  '#C0392B'
    if row['p_fdr'] < 0.05:    return '[FDR]',    '#C0392B'
    if row['p_value'] < 0.001: return '***', '#E67E22'
    if row['p_value'] < 0.01:  return '**',  '#E67E22'
    if row['p_value'] < 0.05:  return '*',   '#E67E22'
    if row['p_value'] < 0.10:  return '.',   '#F39C12'
    return '', '#BDC3C7'

def bar_color(row):
    if row['p_fdr'] < 0.05:    return '#922B21'
    if row['p_value'] < 0.05:  return '#E67E22' if row['coef'] > 0 else '#2980B9'
    if row['p_value'] < 0.10:  return '#F39C12' if row['coef'] > 0 else '#5DADE2'
    return '#BDC3C7'

legend_patches = [
    mpatches.Patch(color='#922B21', alpha=0.85, label='[FDR] = q < 0.05'),
    mpatches.Patch(color='#E67E22', alpha=0.85, label='p < 0.05 (positive beta)'),
    mpatches.Patch(color='#2980B9', alpha=0.85, label='p < 0.05 (negative beta)'),
    mpatches.Patch(color='#F39C12', alpha=0.85, label='Trend p < 0.10'),
]

# ═══════════════════════════════════════════════════════════════
# FIGURE 1: v2_summary_significant (invnorm)
# ═══════════════════════════════════════════════════════════════
panels = [
    (liwc_res[liwc_res['p_value'] < 0.05].sort_values('coef'), 'LIWC',
     'SSI ~ feat_invnorm + time + age + sex + C(Dx) + edu_yrs + (1|pt)'),
    (f0_res[f0_res['p_value'] < 0.10].sort_values('coef'),   'F0',
     'SSI ~ feat_invnorm + time + age + sex + C(Dx) + AP_dose + (1|pt)'),
]

fig, axes = plt.subplots(1, 2, figsize=(16, 9))

for ax, (res_s, ftype, subtitle) in zip(axes, panels):
    if len(res_s) == 0:
        ax.text(0.5, 0.5, 'No significant features',
                ha='center', va='center', fontsize=12)
        ax.set_title(ftype); continue

    y    = np.arange(len(res_s))
    clrs = [bar_color(r) for _, r in res_s.iterrows()]

    ax.barh(y, res_s['coef'], color=clrs, alpha=0.85,
            edgecolor='white', height=0.72)
    ax.errorbar(res_s['coef'], y,
                xerr=[res_s['coef'] - res_s['CI_lower'],
                      res_s['CI_upper'] - res_s['coef']],
                fmt='none', ecolor='#2C3E50', elinewidth=1.2,
                capsize=3, alpha=0.75)
    ax.axvline(0, color='#2C3E50', lw=1, ls='--', alpha=0.35)

    cleaned = [r.replace('liwc_', '').replace('F0final_sma_', '')
                .replace('_', ' ').title() for r in res_s['feature']]
    ax.set_yticks(y)
    ax.set_yticklabels(cleaned, fontsize=9.5)

    for i, (_, row) in enumerate(res_s.iterrows()):
        m, c = sig_marker(row)
        if m:
            ax.text(row['CI_upper'] + 0.04, i, m,
                    va='center', fontsize=8, color=c, fontweight='bold')

    ax.set_xlabel('Standardized beta (invnorm feature → z-scored SSI)', fontsize=10.5)
    ax.set_title(f'{ftype} Features → SSI\n{subtitle}',
                 fontsize=10.5, fontweight='bold')
    ax.legend(handles=legend_patches, fontsize=8.5, loc='lower right', framealpha=0.85)
    ax.grid(axis='x', alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

plt.tight_layout()
plt.savefig(f'{OUT}/v2_summary_significant_invnorm.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ v2_summary_significant_invnorm.png')


# ═══════════════════════════════════════════════════════════════
# FIGURE 2: Results table — LIWC FDR-sig features → SSI
# ═══════════════════════════════════════════════════════════════
def make_table_fig(res, title, fname, p_thresh=0.05):
    show = res[res['p_value'] < p_thresh].copy().sort_values('coef', ascending=False)
    if show.empty:
        print(f"  (no rows for {fname})")
        return

    rows_data = []
    for _, row in show.iterrows():
        m, _ = sig_marker(row)
        rows_data.append([
            row['feature'].replace('liwc_','').replace('F0final_sma_','')
                          .replace('_',' ').title(),
            f"{row['coef']:+.3f}",
            f"{row['SE']:.3f}",
            f"[{row['CI_lower']:+.3f}, {row['CI_upper']:+.3f}]",
            f"{row['p_value']:.4f}",
            f"{row['p_fdr']:.4f}",
            m,
        ])

    col_labels = ['Feature', 'beta', 'SE', '95% CI', 'p', 'q (FDR)', 'Sig']

    row_colors = []
    for r in rows_data:
        sig = r[-1]
        if '[FDR]' in sig:
            bg = '#FADBD8'
        elif r[4] and float(r[4]) < 0.05:
            bg = '#FDEBD0'
        else:
            bg = '#F8F9FA'
        row_colors.append([bg] * len(col_labels))

    fig_h = max(3.5, len(rows_data) * 0.42 + 1.8)
    fig, ax = plt.subplots(figsize=(13, fig_h))
    ax.axis('off')

    tbl = ax.table(cellText=rows_data, colLabels=col_labels,
                   cellLoc='center', loc='center',
                   cellColours=row_colors)
    tbl.auto_set_font_size(False)
    tbl.set_fontsize(9.5)
    tbl.scale(1, 1.65)
    for (i, j), cell in tbl.get_celld().items():
        cell.set_edgecolor('#cccccc')
        if i == 0:
            cell.set_facecolor('#2C3E50')
            cell.set_text_props(color='white', fontweight='bold')
        if i > 0 and j == 1:   # beta column
            val = rows_data[i-1][1]
            cell.set_text_props(color='#C0392B' if val.startswith('+') else '#2980B9',
                                fontweight='bold')

    ax.set_title(title, fontsize=11, fontweight='bold', pad=12)
    plt.tight_layout()
    plt.savefig(fname, dpi=150, bbox_inches='tight')
    plt.close()
    print(f'✓ {fname.split("/")[-1]}')

make_table_fig(
    liwc_res,
    'LIWC Features → SSI  (invnorm, p < 0.05)\nLME: SSI ~ feat_invnorm + time + age + sex + C(Dx) + edu_yrs + (1|id)',
    f'{OUT}/table_liwc_ssi_invnorm.png',
    p_thresh=0.05
)
make_table_fig(
    f0_res,
    'F0 Features → SSI  (invnorm, p < 0.10)\nLME: SSI ~ feat_invnorm + time + age + sex + C(Dx) + AP_dose + (1|id)',
    f'{OUT}/table_f0_ssi_invnorm.png',
    p_thresh=0.10
)

# ═══════════════════════════════════════════════════════════════
# FIGURE 3: Full results table for feature × scale (invnorm)
# FDR-sig rows only, all 4 outcomes combined
# ═══════════════════════════════════════════════════════════════
res_scale = pd.read_csv(f'{OUT}/feature_scale_lme_invnorm.csv')
fdr_rows  = res_scale[res_scale['q_fdr'] < 0.05].copy()
fdr_rows  = fdr_rows.sort_values(['outcome', 'group', 'coef'], ascending=[True, True, False])

rows_data = []
for _, row in fdr_rows.iterrows():
    feat_clean = (row['feature'].replace('liwc_','').replace('F0final_sma_','')
                                .replace('_',' ').title())
    sig = '[FDR***]' if row['p'] < 0.001 else ('[FDR**]' if row['p'] < 0.01 else '[FDR]')
    rows_data.append([
        row['outcome'],
        row['group'],
        feat_clean,
        f"{row['coef']:+.3f}",
        f"{row['SE']:.3f}",
        f"[{row['CI_lo']:+.3f}, {row['CI_hi']:+.3f}]",
        f"{row['p']:.4f}",
        f"{row['q_fdr']:.4f}",
        sig,
    ])

col_labels = ['Outcome', 'Group', 'Feature', 'beta', 'SE', '95% CI', 'p', 'q (FDR)', 'Sig']
outcome_bg = {'HAMD': '#D6EAF8', 'PHQ': '#D5F5E3', 'BAI': '#FDEBD0', 'BHOL': '#E8DAEF'}

row_colors = [[outcome_bg.get(r[0], '#F8F9FA')] * len(col_labels) for r in rows_data]

fig_h = max(6, len(rows_data) * 0.38 + 2.5)
fig, ax = plt.subplots(figsize=(15, fig_h))
ax.axis('off')

tbl = ax.table(cellText=rows_data, colLabels=col_labels,
               cellLoc='center', loc='center',
               cellColours=row_colors)
tbl.auto_set_font_size(False)
tbl.set_fontsize(9)
tbl.scale(1, 1.55)
for (i, j), cell in tbl.get_celld().items():
    cell.set_edgecolor('#cccccc')
    if i == 0:
        cell.set_facecolor('#2C3E50')
        cell.set_text_props(color='white', fontweight='bold')
    if i > 0 and j == 3:
        val = rows_data[i-1][3]
        cell.set_text_props(color='#C0392B' if val.startswith('+') else '#2980B9',
                            fontweight='bold')

ax.set_title(
    'FDR-Significant Associations: Speech/Language Features (invnorm) × Clinical Scales\n'
    'LME with time, age, sex, Dx covariates (+AP_dose for F0; +edu_yrs for LIWC)',
    fontsize=11, fontweight='bold', pad=12)
plt.tight_layout()
plt.savefig(f'{OUT}/table_feature_scale_invnorm_fdr.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ table_feature_scale_invnorm_fdr.png')
