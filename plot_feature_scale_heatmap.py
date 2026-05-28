"""
Heatmap: liwc / F0 features × HAMD / PHQ / BAI / BHOL
Color = standardized β; markers = significance level
"""

import pandas as pd, numpy as np
import matplotlib, matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.font_manager as fm
from matplotlib.colors import TwoSlopeNorm
matplotlib.use('Agg')

fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial.ttf')
fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial_Bold.ttf')
plt.rcParams['font.family'] = 'Arial'

OUT = '/home/user/PRISM-V/results'
res = pd.read_csv(f'{OUT}/feature_scale_lme.csv')

OUTCOMES = ['HAMD', 'PHQ', 'BAI', 'BHOL']

# ── Feature labels: strip prefix for display ──────────────────
def feat_label(f):
    return (f.replace('liwc_', '')
             .replace('F0final_sma_', 'F0: ')
             .replace('_', ' '))

# ── Pivot to wide (feature × outcome) ────────────────────────
def make_wide(sub, col='coef'):
    return sub.pivot(index='feature', columns='outcome', values=col)[OUTCOMES]

# ── Filter: nominally sig in >= 1 outcome, sorted by mean |β| ─
def filter_features(sub):
    nom = sub[sub['p'] < 0.05]['feature'].unique()
    filtered = sub[sub['feature'].isin(nom)]
    mean_abs_b = (filtered.groupby('feature')['coef']
                          .apply(lambda x: x.abs().mean())
                          .sort_values(ascending=False))
    return mean_abs_b.index.tolist()

liwc_sub = res[res['group'] == 'LIWC']
f0_sub   = res[res['group'] == 'F0']

liwc_feats = filter_features(liwc_sub)
f0_feats   = filter_features(f0_sub)

print(f"LIWC features shown: {len(liwc_feats)}")
print(f"F0   features shown: {len(f0_feats)}")

# ═══════════════════════════════════════════════════════════════
# FIGURE: stacked heatmap — LIWC (top) + F0 (bottom)
# ═══════════════════════════════════════════════════════════════
n_liwc = len(liwc_feats)
n_f0   = len(f0_feats)
n_gap  = 1   # gap rows between sections

fig_h = max(12, (n_liwc + n_f0 + n_gap) * 0.40 + 3)
fig, axes = plt.subplots(
    3, 1,
    figsize=(8, fig_h),
    gridspec_kw={'height_ratios': [n_liwc, n_gap * 0.3, n_f0],
                 'hspace': 0.05}
)
ax_liwc, ax_gap, ax_f0 = axes
ax_gap.axis('off')

# ── shared color scale ────────────────────────────────────────
all_b = res.loc[res['feature'].isin(liwc_feats + f0_feats), 'coef']
vmax  = max(abs(all_b.min()), all_b.max(), 0.15)
norm  = TwoSlopeNorm(vmin=-vmax, vcenter=0, vmax=vmax)
cmap  = 'RdBu_r'

def draw_heatmap(ax, feats, sub, title_prefix):
    if not feats:
        ax.axis('off')
        return

    b_wide = make_wide(sub[sub['feature'].isin(feats)].copy())
    p_wide = make_wide(sub[sub['feature'].isin(feats)].copy(), col='p')
    q_wide = make_wide(sub[sub['feature'].isin(feats)].copy(), col='q_fdr')

    b_wide = b_wide.reindex(feats)
    p_wide = p_wide.reindex(feats)
    q_wide = q_wide.reindex(feats)

    mat = b_wide.values
    im  = ax.imshow(mat, aspect='auto', cmap=cmap, norm=norm,
                    interpolation='nearest')

    # cell annotations: significance
    for i in range(mat.shape[0]):
        for j in range(mat.shape[1]):
            q = q_wide.iloc[i, j]
            p = p_wide.iloc[i, j]
            b = mat[i, j]
            if np.isnan(b):
                continue
            if q < 0.05:
                txt, fs, fw = '[FDR]', 6.5, 'bold'
                col = 'white' if abs(b) > vmax * 0.55 else '#111'
            elif p < 0.05:
                txt, fs, fw = '*', 9, 'bold'
                col = 'white' if abs(b) > vmax * 0.55 else '#444'
            else:
                continue
            ax.text(j, i, txt, ha='center', va='center',
                    fontsize=fs, fontweight=fw, color=col)

    # axes labels
    ax.set_xticks(range(len(OUTCOMES)))
    ax.set_xticklabels(OUTCOMES, fontsize=11, fontweight='bold')
    ax.xaxis.set_ticks_position('top')
    ax.xaxis.set_label_position('top')

    ylbls = [feat_label(f) for f in feats]
    ax.set_yticks(range(len(feats)))
    ax.set_yticklabels(ylbls, fontsize=8.5)

    # section label on left spine
    ax.set_ylabel(title_prefix, fontsize=10, fontweight='bold', labelpad=8)

    # grid lines
    for x in np.arange(-0.5, len(OUTCOMES), 1):
        ax.axvline(x, color='white', lw=0.8)
    for y in np.arange(-0.5, len(feats), 1):
        ax.axhline(y, color='white', lw=0.5)

    return im

im = draw_heatmap(ax_liwc, liwc_feats, liwc_sub, 'LIWC')
draw_heatmap(ax_f0,   f0_feats,   f0_sub,   'F0')

# shared colorbar
cbar = fig.colorbar(im, ax=axes.ravel().tolist(),
                    orientation='horizontal', pad=0.03,
                    fraction=0.025, aspect=40)
cbar.set_label('Standardized β  (LME coefficient)', fontsize=10)

# legend
patches = [
    mpatches.Patch(facecolor='#888', label='[FDR] = q < 0.05'),
    mpatches.Patch(facecolor='#aaa', alpha=0.6, label='*  = p < 0.05 (nominal)'),
    mpatches.Patch(facecolor='#C0392B', alpha=0.7, label='Red = positive β'),
    mpatches.Patch(facecolor='#2980B9', alpha=0.7, label='Blue = negative β'),
]
fig.legend(handles=patches, fontsize=8.5, loc='lower center',
           ncol=4, bbox_to_anchor=(0.5, -0.04), framealpha=0.9)

fig.suptitle(
    'Speech/Language Features × Clinical Scale Scores\n'
    '(LME, N=104 patients; features nominally significant in ≥1 outcome)',
    fontsize=11, fontweight='bold', y=1.01)

plt.savefig(f'{OUT}/feature_scale_heatmap.png',
            dpi=150, bbox_inches='tight')
plt.close()
print('✓ feature_scale_heatmap.png')


# ═══════════════════════════════════════════════════════════════
# FIGURE 2: FDR-significant LIWC features only — detailed forest
# ═══════════════════════════════════════════════════════════════
fdr_feats = sorted(
    res[(res['group'] == 'LIWC') & (res['q_fdr'] < 0.05)]['feature'].unique()
)
if fdr_feats:
    sub_fdr = liwc_sub[liwc_sub['feature'].isin(fdr_feats)].copy()

    fig, axes = plt.subplots(1, len(OUTCOMES), figsize=(15, 5), sharey=True)
    for ax, outcome in zip(axes, OUTCOMES):
        sub_o = sub_fdr[sub_fdr['outcome'] == outcome].set_index('feature').reindex(fdr_feats)
        y     = np.arange(len(fdr_feats))
        ylbls = [feat_label(f) for f in fdr_feats]

        for i, feat in enumerate(fdr_feats):
            row = sub_o.loc[feat]
            if pd.isna(row['coef']):
                continue
            alpha = 0.90 if row['q_fdr'] < 0.05 else (0.55 if row['p'] < 0.05 else 0.25)
            clr   = '#C0392B' if row['coef'] > 0 else '#2980B9'
            ax.barh(i, row['coef'], color=clr, alpha=alpha,
                    edgecolor='white', height=0.65)
            ax.errorbar(row['coef'], i,
                        xerr=[[row['coef'] - row['CI_lo']],
                               [row['CI_hi'] - row['coef']]],
                        fmt='none', ecolor='#333', elinewidth=1.1,
                        capsize=3, alpha=0.75)
            if row['q_fdr'] < 0.05:
                ax.text(row['CI_hi'] + 0.005, i, '[FDR]', va='center',
                        fontsize=7, color='#7D3C98', fontweight='bold')
            elif row['p'] < 0.05:
                ax.text(row['CI_hi'] + 0.005, i, '*', va='center',
                        fontsize=9, color='#E67E22')

        ax.axvline(0, color='#2C3E50', lw=1, ls='--', alpha=0.4)
        ax.set_xlabel('Standardized β', fontsize=10)
        ax.set_title(outcome, fontsize=12, fontweight='bold', pad=6)
        ax.grid(axis='x', alpha=0.2, ls=':')
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)

    axes[0].set_yticks(y)
    axes[0].set_yticklabels(ylbls, fontsize=10)

    plt.suptitle(
        'LIWC Features with ≥1 FDR-Significant Association\nwith Clinical Scale Scores',
        fontsize=11, fontweight='bold', y=1.03)
    plt.tight_layout()
    plt.savefig(f'{OUT}/feature_scale_fdr_forest.png', dpi=150, bbox_inches='tight')
    plt.close()
    print('✓ feature_scale_fdr_forest.png')
