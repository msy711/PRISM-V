"""
Visualization of Mixed Effects Model Results: F0 & LIWC → SSI
"""

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.lines import Line2D
import warnings
warnings.filterwarnings('ignore')

# ─────────────────────────────────────────────
# Load results
# ─────────────────────────────────────────────
f0_res   = pd.read_csv('/home/user/PRISM-V/results/lme_F0_SSI_results.csv')
liwc_res = pd.read_csv('/home/user/PRISM-V/results/lme_LIWC_SSI_results.csv')

# ─────────────────────────────────────────────
# Color scheme
# ─────────────────────────────────────────────
def get_color(row):
    if row['p_fdr'] < 0.05:
        return '#C0392B'   # FDR significant – red
    elif row['p_value'] < 0.05:
        return '#E67E22'   # Nominally significant – orange
    elif row['p_value'] < 0.10:
        return '#F1C40F'   # Trend – yellow
    else:
        return '#BDC3C7'   # Not significant – gray

# ─────────────────────────────────────────────
# FIGURE 1: LIWC Forest Plot
# ─────────────────────────────────────────────
liwc_top = liwc_res[liwc_res['p_value'] < 0.10].copy().sort_values('coef')
liwc_top['color'] = liwc_top.apply(get_color, axis=1)

# Clean feature names
def clean_name(name):
    return name.replace('liwc_', '').replace('_', ' ').title()

liwc_top['label'] = liwc_top['feature'].apply(clean_name)

fig, ax = plt.subplots(figsize=(10, max(6, len(liwc_top) * 0.45)))
y_pos = np.arange(len(liwc_top))

ax.barh(y_pos, liwc_top['coef'], color=liwc_top['color'], alpha=0.85,
        edgecolor='white', height=0.7)

# Error bars (95% CI)
ax.errorbar(liwc_top['coef'], y_pos,
            xerr=[liwc_top['coef'] - liwc_top['CI_lower'],
                  liwc_top['CI_upper'] - liwc_top['coef']],
            fmt='none', ecolor='#2C3E50', elinewidth=1.2, capsize=3, alpha=0.7)

# Vertical line at 0
ax.axvline(0, color='#2C3E50', linewidth=1.2, linestyle='--', alpha=0.5)

# Labels
ax.set_yticks(y_pos)
ax.set_yticklabels(liwc_top['label'], fontsize=10)

# P-value annotations
for i, (_, row) in enumerate(liwc_top.iterrows()):
    marker = ''
    if row['p_fdr'] < 0.001:
        marker = '★★★ FDR'
    elif row['p_fdr'] < 0.01:
        marker = '★★ FDR'
    elif row['p_fdr'] < 0.05:
        marker = '★ FDR'
    elif row['p_value'] < 0.001:
        marker = '***'
    elif row['p_value'] < 0.01:
        marker = '**'
    elif row['p_value'] < 0.05:
        marker = '*'
    elif row['p_value'] < 0.10:
        marker = '.'

    if marker:
        x = row['CI_upper'] + 0.05
        ax.text(x, i, marker, va='center', fontsize=8,
                color='#C0392B' if 'FDR' in marker else '#2C3E50', fontweight='bold')

# Legend
patches = [
    mpatches.Patch(color='#C0392B', alpha=0.85, label='FDR significant (q<0.05)'),
    mpatches.Patch(color='#E67E22', alpha=0.85, label='Nominally significant (p<0.05)'),
    mpatches.Patch(color='#F1C40F', alpha=0.85, label='Trend (p<0.10)'),
]
ax.legend(handles=patches, loc='lower right', fontsize=9, framealpha=0.8)

ax.set_xlabel('Standardized Coefficient (β)', fontsize=12)
ax.set_title('LIWC Features Associated with SSI\n(Mixed Effects Model, Random Effect = Patient ID)',
             fontsize=13, fontweight='bold', pad=12)
ax.grid(axis='x', alpha=0.3, linestyle=':')
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

plt.tight_layout()
plt.savefig('/home/user/PRISM-V/results/forest_plot_LIWC_SSI.png', dpi=150, bbox_inches='tight')
plt.close()
print("✓ Saved: forest_plot_LIWC_SSI.png")

# ─────────────────────────────────────────────
# FIGURE 2: F0 Forest Plot
# ─────────────────────────────────────────────
f0_top = f0_res[f0_res['p_value'] < 0.10].copy().sort_values('coef')
f0_top['color'] = f0_top.apply(get_color, axis=1)

def clean_f0_name(name):
    return (name.replace('F0final_sma_', 'F0_')
                .replace('_', ' '))

f0_top['label'] = f0_top['feature'].apply(clean_f0_name)

fig, ax = plt.subplots(figsize=(10, max(6, len(f0_top) * 0.45)))
y_pos = np.arange(len(f0_top))

ax.barh(y_pos, f0_top['coef'], color=f0_top['color'], alpha=0.85,
        edgecolor='white', height=0.7)

ax.errorbar(f0_top['coef'], y_pos,
            xerr=[f0_top['coef'] - f0_top['CI_lower'],
                  f0_top['CI_upper'] - f0_top['coef']],
            fmt='none', ecolor='#2C3E50', elinewidth=1.2, capsize=3, alpha=0.7)

ax.axvline(0, color='#2C3E50', linewidth=1.2, linestyle='--', alpha=0.5)

ax.set_yticks(y_pos)
ax.set_yticklabels(f0_top['label'], fontsize=10)

for i, (_, row) in enumerate(f0_top.iterrows()):
    marker = ''
    if row['p_fdr'] < 0.05:
        marker = '★ FDR'
    elif row['p_value'] < 0.001:
        marker = '***'
    elif row['p_value'] < 0.01:
        marker = '**'
    elif row['p_value'] < 0.05:
        marker = '*'
    elif row['p_value'] < 0.10:
        marker = '.'
    if marker:
        ax.text(row['CI_upper'] + 0.05, i, marker, va='center',
                fontsize=8, color='#C0392B' if 'FDR' in marker else '#2C3E50',
                fontweight='bold')

ax.legend(handles=patches, loc='lower right', fontsize=9, framealpha=0.8)
ax.set_xlabel('Standardized Coefficient (β)', fontsize=12)
ax.set_title('F0 Features Associated with SSI\n(Mixed Effects Model, Random Effect = Patient ID)',
             fontsize=13, fontweight='bold', pad=12)
ax.grid(axis='x', alpha=0.3, linestyle=':')
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

plt.tight_layout()
plt.savefig('/home/user/PRISM-V/results/forest_plot_F0_SSI.png', dpi=150, bbox_inches='tight')
plt.close()
print("✓ Saved: forest_plot_F0_SSI.png")

# ─────────────────────────────────────────────
# FIGURE 3: Volcano Plot (both F0 + LIWC)
# ─────────────────────────────────────────────
all_res = pd.concat([
    f0_res.assign(feature_type='F0'),
    liwc_res.assign(feature_type='LIWC')
]).dropna(subset=['p_value'])

all_res['neg_log10_p'] = -np.log10(all_res['p_value'].clip(lower=1e-15))
all_res['neg_log10_fdr'] = -np.log10(all_res['p_fdr'].clip(lower=1e-15))

fig, axes = plt.subplots(1, 2, figsize=(16, 7))

for ax, (ftype, color_main) in zip(axes, [('F0', '#2980B9'), ('LIWC', '#8E44AD')]):
    sub = all_res[all_res['feature_type'] == ftype].copy()

    # Color points
    colors = np.where(sub['p_fdr'] < 0.05, '#C0392B',
              np.where(sub['p_value'] < 0.05, '#E67E22',
              np.where(sub['p_value'] < 0.10, '#F1C40F', '#BDC3C7')))

    sizes = np.where(sub['p_fdr'] < 0.05, 80,
             np.where(sub['p_value'] < 0.05, 55, 25))

    ax.scatter(sub['coef'], sub['neg_log10_p'],
               c=colors, s=sizes, alpha=0.8, edgecolors='white', linewidths=0.4, zorder=3)

    # Threshold lines
    ax.axhline(-np.log10(0.05), color='#E67E22', linestyle='--', alpha=0.6,
               linewidth=1, label='p=0.05')

    fdr_thr = sub[sub['p_fdr'] < 0.05]['p_value'].max() if (sub['p_fdr'] < 0.05).any() else None
    if fdr_thr is not None:
        ax.axhline(-np.log10(fdr_thr), color='#C0392B', linestyle='--', alpha=0.6,
                   linewidth=1.5, label=f'FDR threshold')

    ax.axvline(0, color='gray', linestyle=':', alpha=0.4)

    # Label significant features
    sig_pts = sub[sub['p_value'] < 0.05].copy()
    for _, row in sig_pts.iterrows():
        lbl = row['feature'].replace('F0final_sma_','').replace('liwc_','')
        ax.annotate(lbl,
                    xy=(row['coef'], row['neg_log10_p']),
                    xytext=(4, 2), textcoords='offset points',
                    fontsize=7, color='#2C3E50', alpha=0.9)

    ax.set_xlabel('Standardized Coefficient (β)', fontsize=11)
    ax.set_ylabel('-log₁₀(p-value)', fontsize=11)
    ax.set_title(f'{ftype} Features: Volcano Plot\n(SSI outcome)', fontsize=12, fontweight='bold')
    ax.legend(fontsize=9)
    ax.grid(alpha=0.2, linestyle=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

plt.tight_layout()
plt.savefig('/home/user/PRISM-V/results/volcano_plot_SSI.png', dpi=150, bbox_inches='tight')
plt.close()
print("✓ Saved: volcano_plot_SSI.png")

# ─────────────────────────────────────────────
# FIGURE 4: Top features summary heatmap
# ─────────────────────────────────────────────
top_liwc = liwc_res[liwc_res['p_value'] < 0.05].sort_values('p_value').head(20)
top_f0   = f0_res[f0_res['p_value'] < 0.05].sort_values('p_value').head(10)

fig, axes = plt.subplots(1, 2, figsize=(14, 8))

for ax, res_df, ftype, color in zip(
    axes,
    [top_liwc, top_f0],
    ['LIWC', 'F0'],
    ['#8E44AD', '#2980B9']
):
    if len(res_df) == 0:
        ax.text(0.5, 0.5, 'No significant features', ha='center', va='center', fontsize=12)
        ax.set_title(ftype)
        continue

    res_sorted = res_df.sort_values('coef')
    labels = [r.replace('liwc_','').replace('F0final_sma_','F0_').replace('_',' ')
              for r in res_sorted['feature']]
    bar_colors = ['#C0392B' if c > 0 else '#2980B9' for c in res_sorted['coef']]

    bars = ax.barh(range(len(res_sorted)), res_sorted['coef'],
                   color=bar_colors, alpha=0.8, edgecolor='white', height=0.7)
    ax.errorbar(res_sorted['coef'], range(len(res_sorted)),
                xerr=[res_sorted['coef'] - res_sorted['CI_lower'],
                      res_sorted['CI_upper'] - res_sorted['coef']],
                fmt='none', ecolor='#555', elinewidth=1, capsize=3)
    ax.axvline(0, color='black', linewidth=1, linestyle='--', alpha=0.3)
    ax.set_yticks(range(len(res_sorted)))
    ax.set_yticklabels(labels, fontsize=9)
    ax.set_xlabel('β (standardized)', fontsize=10)
    ax.set_title(f'Significant {ftype} Features → SSI\n(p < 0.05)', fontsize=11, fontweight='bold')

    # p-value stars
    for i, (_, row) in enumerate(res_sorted.iterrows()):
        stars = '***' if row['p_value'] < 0.001 else ('**' if row['p_value'] < 0.01 else '*')
        fdr_note = ' q<.05' if row['p_fdr'] < 0.05 else ''
        ax.text(row['CI_upper'] + 0.03, i, stars + fdr_note,
                va='center', fontsize=7.5, color='#C0392B' if fdr_note else '#555')

    red_patch   = mpatches.Patch(color='#C0392B', alpha=0.8, label='↑ Higher SSI')
    blue_patch  = mpatches.Patch(color='#2980B9', alpha=0.8, label='↓ Lower SSI')
    ax.legend(handles=[red_patch, blue_patch], fontsize=9, loc='lower right')
    ax.grid(axis='x', alpha=0.2, linestyle=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

plt.suptitle('Mixed Effects Model: Features Significantly Associated with SSI\n(Random Effect = Patient, Covariate = Time)',
             fontsize=12, fontweight='bold', y=1.01)
plt.tight_layout()
plt.savefig('/home/user/PRISM-V/results/significant_features_summary.png', dpi=150, bbox_inches='tight')
plt.close()
print("✓ Saved: significant_features_summary.png")

print("\n✓ All visualizations saved to /home/user/PRISM-V/results/")
