"""
Heatmap: liwc / F0 features (invnorm) × HAMD / PHQ / BAI / BHOL
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
res = pd.read_csv(f'{OUT}/feature_scale_lme_invnorm.csv')

OUTCOMES = ['HAMD', 'PHQ', 'BAI', 'BHOL']

def feat_label(f):
    return (f.replace('liwc_', '')
             .replace('F0final_sma_', '')
             .replace('_', ' '))

def make_wide(sub, col='coef'):
    return sub.pivot(index='feature', columns='outcome', values=col)[OUTCOMES]

# ─────────────────────────────────────────────
# Filter: FDR-sig in >=1 outcome; sort by n_sig then mean |β|
# ─────────────────────────────────────────────
def select_features(sub):
    fdr_feats = sub[sub['q_fdr'] < 0.05]['feature'].unique()
    nom_feats = sub[sub['p'] < 0.05]['feature'].unique()
    show = list(set(fdr_feats) | set(nom_feats))
    if not show:
        return []
    s = sub[sub['feature'].isin(show)].copy()
    # sort: FDR count desc, then mean |β| desc
    n_fdr = s[s['q_fdr'] < 0.05].groupby('feature').size().reindex(show, fill_value=0)
    mean_b = s.groupby('feature')['coef'].apply(lambda x: x.abs().mean()).reindex(show)
    order  = pd.DataFrame({'n_fdr': n_fdr, 'mean_b': mean_b})
    order  = order.sort_values(['n_fdr', 'mean_b'], ascending=False)
    return order.index.tolist()

liwc_sub   = res[res['group'] == 'LIWC']
f0_sub     = res[res['group'] == 'F0']
liwc_feats = select_features(liwc_sub)
f0_feats   = select_features(f0_sub)

print(f"LIWC features shown: {len(liwc_feats)}")
print(f"F0   features shown: {len(f0_feats)}")

# ═══════════════════════════════════════════════════════════════
# FIGURE 1: Stacked heatmap  (LIWC top / F0 bottom)
# ═══════════════════════════════════════════════════════════════
n_liwc = len(liwc_feats)
n_f0   = len(f0_feats)

fig_h = max(14, (n_liwc + n_f0) * 0.38 + 4)
fig, axes = plt.subplots(
    3, 1, figsize=(8, fig_h),
    gridspec_kw={'height_ratios': [n_liwc, 0.4, n_f0], 'hspace': 0.04}
)
ax_liwc, ax_gap, ax_f0 = axes
ax_gap.axis('off')

all_b = res.loc[res['feature'].isin(liwc_feats + f0_feats), 'coef']
vmax  = max(abs(all_b.min()), all_b.max(), 0.15)
norm  = TwoSlopeNorm(vmin=-vmax, vcenter=0, vmax=vmax)
cmap  = 'RdBu_r'

def draw_heatmap(ax, feats, sub, ylabel):
    if not feats:
        ax.axis('off'); return None

    b_wide = make_wide(sub[sub['feature'].isin(feats)]).reindex(feats)
    p_wide = make_wide(sub[sub['feature'].isin(feats)], 'p').reindex(feats)
    q_wide = make_wide(sub[sub['feature'].isin(feats)], 'q_fdr').reindex(feats)

    mat = b_wide.values
    im  = ax.imshow(mat, aspect='auto', cmap=cmap, norm=norm, interpolation='nearest')

    for i in range(mat.shape[0]):
        for j in range(mat.shape[1]):
            q = q_wide.iloc[i, j]
            p = p_wide.iloc[i, j]
            b = mat[i, j]
            if np.isnan(b): continue
            bright = abs(b) > vmax * 0.55
            if q < 0.05:
                ax.text(j, i, '[FDR]', ha='center', va='center',
                        fontsize=6.5, fontweight='bold',
                        color='white' if bright else '#111')
            elif p < 0.05:
                ax.text(j, i, '*', ha='center', va='center',
                        fontsize=9, fontweight='bold',
                        color='white' if bright else '#444')

    ax.set_xticks(range(len(OUTCOMES)))
    ax.set_xticklabels(OUTCOMES, fontsize=11, fontweight='bold')
    ax.xaxis.set_ticks_position('top')
    ax.xaxis.set_label_position('top')
    ax.set_yticks(range(len(feats)))
    ax.set_yticklabels([feat_label(f) for f in feats], fontsize=8)
    ax.set_ylabel(ylabel, fontsize=10, fontweight='bold', labelpad=8)

    for x in np.arange(-0.5, len(OUTCOMES), 1):
        ax.axvline(x, color='white', lw=0.8)
    for y in np.arange(-0.5, len(feats), 1):
        ax.axhline(y, color='white', lw=0.5)
    return im

im = draw_heatmap(ax_liwc, liwc_feats, liwc_sub, 'LIWC')
draw_heatmap(ax_f0,   f0_feats,   f0_sub,   'F0')

cbar = fig.colorbar(im, ax=axes.ravel().tolist(),
                    orientation='horizontal', pad=0.03,
                    fraction=0.022, aspect=45)
cbar.set_label('Standardized beta  (invnorm feature → z-scored outcome)', fontsize=10)

patches = [
    mpatches.Patch(facecolor='#888', label='[FDR] = q < 0.05'),
    mpatches.Patch(facecolor='#aaa', alpha=0.6, label='* = p < 0.05 (nominal)'),
    mpatches.Patch(facecolor='#C0392B', alpha=0.7, label='Red = positive beta'),
    mpatches.Patch(facecolor='#2980B9', alpha=0.7, label='Blue = negative beta'),
]
fig.legend(handles=patches, fontsize=9, loc='lower center',
           ncol=4, bbox_to_anchor=(0.5, -0.04), framealpha=0.9)

fig.suptitle(
    'Speech/Language Features (invnorm) x Clinical Scale Scores\n'
    'LME, N=104 patients; features with p<0.05 in >=1 outcome shown',
    fontsize=11, fontweight='bold', y=1.01)

plt.savefig(f'{OUT}/feature_scale_heatmap_invnorm.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ feature_scale_heatmap_invnorm.png')


# ═══════════════════════════════════════════════════════════════
# FIGURE 2: FDR-sig features only — side-by-side forest per outcome
# F0 and LIWC separately
# ═══════════════════════════════════════════════════════════════
for grp, sub, grp_feats in [('F0', f0_sub, f0_feats), ('LIWC', liwc_sub, liwc_feats)]:
    fdr_feats = sorted(sub[sub['q_fdr'] < 0.05]['feature'].unique(),
                       key=lambda f: sub[sub['feature']==f]['coef'].mean(), reverse=True)
    if not fdr_feats:
        print(f"  No FDR-sig features for {grp}")
        continue

    fig, axes = plt.subplots(1, len(OUTCOMES), figsize=(15, max(5, len(fdr_feats)*0.45+2)),
                             sharey=True)
    for ax, outcome in zip(axes, OUTCOMES):
        sub_o = sub[sub['outcome'] == outcome].set_index('feature').reindex(fdr_feats)
        y     = np.arange(len(fdr_feats))

        for i, feat in enumerate(fdr_feats):
            row = sub_o.loc[feat]
            if pd.isna(row['coef']): continue
            clr   = '#C0392B' if row['coef'] > 0 else '#2980B9'
            alpha = 0.90 if row['q_fdr'] < 0.05 else (0.55 if row['p'] < 0.05 else 0.22)
            ax.barh(i, row['coef'], color=clr, alpha=alpha, edgecolor='white', height=0.65)
            ax.errorbar(row['coef'], i,
                        xerr=[[row['coef'] - row['CI_lo']],
                               [row['CI_hi'] - row['coef']]],
                        fmt='none', ecolor='#333', elinewidth=1.1, capsize=3, alpha=0.75)
            if row['q_fdr'] < 0.05:
                ax.text(row['CI_hi'] + 0.01, i, '[FDR]', va='center',
                        fontsize=7, color='#7D3C98', fontweight='bold')
            elif row['p'] < 0.05:
                ax.text(row['CI_hi'] + 0.01, i, '*', va='center',
                        fontsize=9, color='#E67E22')

        ax.axvline(0, color='#2C3E50', lw=1, ls='--', alpha=0.4)
        ax.set_xlabel('Standardized beta', fontsize=10)
        ax.set_title(outcome, fontsize=12, fontweight='bold', pad=6)
        ax.grid(axis='x', alpha=0.2, ls=':')
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)

    axes[0].set_yticks(y)
    axes[0].set_yticklabels([feat_label(f) for f in fdr_feats], fontsize=9.5)

    plt.suptitle(f'{grp} Features (invnorm) with FDR-sig Association\n'
                 f'with Clinical Scale Scores',
                 fontsize=11, fontweight='bold', y=1.03)
    plt.tight_layout()
    fname = f'{OUT}/feature_scale_fdr_forest_invnorm_{grp}.png'
    plt.savefig(fname, dpi=150, bbox_inches='tight')
    plt.close()
    print(f'✓ {fname.split("/")[-1]}')


# ═══════════════════════════════════════════════════════════════
# FIGURE 3: Before vs After invnorm — FDR sig count comparison
# ═══════════════════════════════════════════════════════════════
res_orig = pd.read_csv(f'{OUT}/feature_scale_lme.csv')

fig, axes = plt.subplots(1, 2, figsize=(12, 5))
width = 0.35

for ax, grp in zip(axes, ['F0', 'LIWC']):
    orig = res_orig[res_orig['group'] == grp]
    inv  = res[res['group'] == grp]

    orig_fdr = [((orig['outcome']==o)&(orig['q_fdr']<0.05)).sum() for o in OUTCOMES]
    inv_fdr  = [((inv['outcome']==o) &(inv['q_fdr'] <0.05)).sum() for o in OUTCOMES]
    orig_nom = [((orig['outcome']==o)&(orig['p']<0.05)).sum() for o in OUTCOMES]
    inv_nom  = [((inv['outcome']==o) &(inv['p'] <0.05)).sum() for o in OUTCOMES]

    x = np.arange(len(OUTCOMES))
    ax.bar(x - width/2, orig_fdr, width, label='FDR (original)',  color='#2980B9', alpha=0.85)
    ax.bar(x + width/2, inv_fdr,  width, label='FDR (invnorm)',   color='#C0392B', alpha=0.85)
    ax.bar(x - width/2, orig_nom, width, label='nom (original)',  color='#2980B9', alpha=0.35,
           bottom=0, linestyle='--', edgecolor='#2980B9')
    ax.bar(x + width/2, inv_nom,  width, label='nom (invnorm)',   color='#C0392B', alpha=0.35,
           bottom=0, linestyle='--', edgecolor='#C0392B')

    ax.set_xticks(x)
    ax.set_xticklabels(OUTCOMES, fontsize=11)
    ax.set_ylabel('Number of significant features', fontsize=10)
    ax.set_title(f'{grp} features', fontsize=11, fontweight='bold')
    ax.legend(fontsize=8.5, loc='upper right')
    ax.grid(axis='y', alpha=0.25, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

plt.suptitle('Impact of Inverse Normal Transformation on Significance\n'
             '(FDR q<0.05 and nominal p<0.05 feature counts)',
             fontsize=11, fontweight='bold', y=1.03)
plt.tight_layout()
plt.savefig(f'{OUT}/invnorm_comparison.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ invnorm_comparison.png')
