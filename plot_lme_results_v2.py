"""
Visualization v2: Mixed Effects Model Results (with full covariates)
"""

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import warnings
warnings.filterwarnings('ignore')

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

# ─────────────────────────────────────────────
# FIGURE 1: LIWC Forest Plot (p < 0.10)
# ─────────────────────────────────────────────
liwc_top = liwc_res[liwc_res['p_value'] < 0.10].copy().sort_values('coef').reset_index(drop=True)

fig, ax = plt.subplots(figsize=(11, max(7, len(liwc_top) * 0.48)))
y = np.arange(len(liwc_top))

colors = [bar_color(r) for _, r in liwc_top.iterrows()]
ax.barh(y, liwc_top['coef'], color=colors, alpha=0.85, edgecolor='white', height=0.72)
ax.errorbar(liwc_top['coef'], y,
            xerr=[liwc_top['coef'] - liwc_top['CI_lower'],
                  liwc_top['CI_upper'] - liwc_top['coef']],
            fmt='none', ecolor='#2C3E50', elinewidth=1.3, capsize=3.5, alpha=0.75)
ax.axvline(0, color='#2C3E50', lw=1.2, ls='--', alpha=0.45)

labels = [r.replace('liwc_','').replace('_',' ').title() for r in liwc_top['feature']]
ax.set_yticks(y)
ax.set_yticklabels(labels, fontsize=10.5)

for i, (_, row) in enumerate(liwc_top.iterrows()):
    m, c = sig_marker(row)
    if m:
        ax.text(row['CI_upper'] + 0.04, i, m, va='center', fontsize=8.5,
                color=c, fontweight='bold')

legend_patches = [
    mpatches.Patch(color='#922B21', alpha=0.85, label='FDR significant (q < 0.05)'),
    mpatches.Patch(color='#E67E22', alpha=0.85, label='p < 0.05 (positive β)'),
    mpatches.Patch(color='#2980B9', alpha=0.85, label='p < 0.05 (negative β)'),
    mpatches.Patch(color='#F39C12', alpha=0.85, label='Trend p < 0.10'),
]
ax.legend(handles=legend_patches, fontsize=9, loc='lower right', framealpha=0.85)
ax.set_xlabel('Standardized Coefficient (β)', fontsize=12)
ax.set_title('LIWC Features Associated with SSI\n'
             'LME: SSI ~ feat_z + time + age + sex + C(Dx) + edu_yrs + (1|patient)',
             fontsize=12, fontweight='bold', pad=10)
ax.grid(axis='x', alpha=0.25, ls=':')
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
plt.tight_layout()
plt.savefig(f'{OUT}/v2_forest_LIWC_SSI.png', dpi=150, bbox_inches='tight')
plt.close()
print("✓ v2_forest_LIWC_SSI.png")

# ─────────────────────────────────────────────
# FIGURE 2: F0 Forest Plot (p < 0.15)
# ─────────────────────────────────────────────
f0_top = f0_res[f0_res['p_value'] < 0.15].copy().sort_values('coef').reset_index(drop=True)

fig, ax = plt.subplots(figsize=(11, max(6, len(f0_top) * 0.48)))
y = np.arange(len(f0_top))
colors = [bar_color(r) for _, r in f0_top.iterrows()]

ax.barh(y, f0_top['coef'], color=colors, alpha=0.85, edgecolor='white', height=0.72)
ax.errorbar(f0_top['coef'], y,
            xerr=[f0_top['coef'] - f0_top['CI_lower'],
                  f0_top['CI_upper'] - f0_top['coef']],
            fmt='none', ecolor='#2C3E50', elinewidth=1.3, capsize=3.5, alpha=0.75)
ax.axvline(0, color='#2C3E50', lw=1.2, ls='--', alpha=0.45)

f0_labels = [r.replace('F0final_sma_','').replace('_',' ') for r in f0_top['feature']]
ax.set_yticks(y)
ax.set_yticklabels(f0_labels, fontsize=10.5)

for i, (_, row) in enumerate(f0_top.iterrows()):
    m, c = sig_marker(row)
    if m:
        ax.text(row['CI_upper'] + 0.04, i, m, va='center', fontsize=8.5,
                color=c, fontweight='bold')

ax.legend(handles=legend_patches, fontsize=9, loc='lower right', framealpha=0.85)
ax.set_xlabel('Standardized Coefficient (β)', fontsize=12)
ax.set_title('F0 Features Associated with SSI\n'
             'LME: SSI ~ feat_z + time + age + sex + C(Dx) + AP_dose + (1|patient)',
             fontsize=12, fontweight='bold', pad=10)
ax.grid(axis='x', alpha=0.25, ls=':')
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
plt.tight_layout()
plt.savefig(f'{OUT}/v2_forest_F0_SSI.png', dpi=150, bbox_inches='tight')
plt.close()
print("✓ v2_forest_F0_SSI.png")

# ─────────────────────────────────────────────
# FIGURE 3: Combined Summary (significant only)
# ─────────────────────────────────────────────
import matplotlib.font_manager as fm

# Arial 폰트 설정
plt.rcParams['font.family'] = 'Arial'

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

    # Annotation
    for i, (_, row) in enumerate(res_sorted.iterrows()):
        m, c = sig_marker(row)
        if m:
            ax.text(row['CI_upper'] + 0.04, i, m, va='center', fontsize=8, color=c, fontweight='bold')

    ax.set_xlabel('Standardized β (z-scored feature)', fontsize=10.5)
    ax.set_title(f'{ftype} Features → SSI\n{subtitle}', fontsize=10.5, fontweight='bold')
    ax.legend(handles=legend_patches, fontsize=8.5, loc='lower right', framealpha=0.85)
    ax.grid(axis='x', alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

# 큰 제목 제거
plt.tight_layout()
plt.savefig(f'{OUT}/v2_summary_significant.png', dpi=150, bbox_inches='tight')
plt.close()

# rcParams 초기화 (이후 플롯에 영향 없도록)
plt.rcParams['font.family'] = 'sans-serif'
print("✓ v2_summary_significant.png")

# ─────────────────────────────────────────────
# FIGURE 4: Volcano plots
# ─────────────────────────────────────────────
all_res = pd.concat([
    f0_res.assign(feature_type='F0'),
    liwc_res.assign(feature_type='LIWC')
]).dropna(subset=['p_value'])

all_res['neg_log10_p'] = -np.log10(all_res['p_value'].clip(1e-15))

fig, axes = plt.subplots(1, 2, figsize=(16, 7))

for ax, (ftype, mclr) in zip(axes, [('F0','#2980B9'), ('LIWC','#8E44AD')]):
    sub = all_res[all_res['feature_type'] == ftype].copy()

    pt_colors = []
    pt_sizes  = []
    for _, r in sub.iterrows():
        if r['p_fdr'] < 0.05:
            pt_colors.append('#C0392B'); pt_sizes.append(90)
        elif r['p_value'] < 0.05:
            pt_colors.append('#E67E22'); pt_sizes.append(60)
        elif r['p_value'] < 0.10:
            pt_colors.append('#F39C12'); pt_sizes.append(40)
        else:
            pt_colors.append('#BDC3C7'); pt_sizes.append(22)

    ax.scatter(sub['coef'], sub['neg_log10_p'],
               c=pt_colors, s=pt_sizes, alpha=0.85, edgecolors='white', linewidths=0.4, zorder=3)

    ax.axhline(-np.log10(0.05), color='#E67E22', ls='--', lw=1.1, alpha=0.6, label='p=0.05')

    fdr_rows = sub[sub['p_fdr'] < 0.05]
    if len(fdr_rows) > 0:
        fdr_thr = fdr_rows['p_value'].max()
        ax.axhline(-np.log10(fdr_thr), color='#C0392B', ls='--', lw=1.5, alpha=0.65, label='FDR threshold')

    ax.axvline(0, color='gray', ls=':', alpha=0.35)

    # Label significant points
    for _, row in sub[sub['p_value'] < 0.05].iterrows():
        lbl = row['feature'].replace('F0final_sma_','').replace('liwc_','')
        ax.annotate(lbl, xy=(row['coef'], row['neg_log10_p']),
                    xytext=(4, 2), textcoords='offset points',
                    fontsize=7, color='#2C3E50', alpha=0.9)

    ax.set_xlabel('Standardized Coefficient (β)', fontsize=11)
    ax.set_ylabel('-log₁₀(p)', fontsize=11)
    cov_note = 'AP_dose' if ftype == 'F0' else 'edu_yrs'
    ax.set_title(f'{ftype}: Volcano Plot\n(covariate: age, sex, Dx, {cov_note}, time)', fontsize=11, fontweight='bold')
    ax.legend(fontsize=9)
    ax.grid(alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

plt.tight_layout()
plt.savefig(f'{OUT}/v2_volcano.png', dpi=150, bbox_inches='tight')
plt.close()
print("✓ v2_volcano.png")

print(f"\n✓ All plots saved to {OUT}/")
