"""SSI Item-level LME results visualization
Side-by-side forest plot: liwc_death and F0_qregc3 → each SSI item
"""

import pandas as pd, numpy as np
import matplotlib, matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.font_manager as fm
from matplotlib.lines import Line2D
matplotlib.use('Agg')

fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial.ttf')
fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial_Bold.ttf')
plt.rcParams['font.family'] = 'Arial'

OUT = '/home/user/PRISM-V/results'

res_liwc = pd.read_csv(f'{OUT}/ssi_item_lme_liwc_death.csv')
res_f0   = pd.read_csv(f'{OUT}/ssi_item_lme_F0_qregc3.csv')

SSI_LABELS = {
    'SSI1':  'SSI1: Loss of will to live',
    'SSI2':  'SSI2: Wish to die',
    'SSI3':  'SSI3: Life/death attitude',
    'SSI4':  'SSI4: Active suicidal desire',
    'SSI5':  'SSI5: Passive suicidal desire',
    'SSI6':  'SSI6: Duration of ideation',
    'SSI7':  'SSI7: Frequency of ideation',
    'SSI8':  'SSI8: Acceptance of ideation',
    'SSI9':  'SSI9: Control over impulse (inv)',
    'SSI10': 'SSI10: Environmental deterrents (inv)',
    'SSI11': 'SSI11: Reason for ideation',
    'SSI12': 'SSI12: Method specificity',
    'SSI13': 'SSI13: Method availability',
    'SSI14': 'SSI14: Sense of capability',
    'SSI15': 'SSI15: Expectancy of attempt',
    'SSI16': 'SSI16: Actual preparation',
    'SSI17': 'SSI17: Suicide note',
    'SSI18': 'SSI18: Final acts',
    'SSI19': 'SSI19: Concealment of ideation',
}

# SSI domain groupings (Beck SSI subscales)
# Part I: Ideation/Attitude (1–5) — wish to live/die, active/passive desire
# Part II: Characteristics (6–11) — duration, frequency, acceptance, control, deterrents, reason
# Part III: Plan (12–15) — method specificity, availability, capability, expectancy
# Part IV: Behavior (16–19) — preparation, note, final acts, concealment
DOMAIN_COLORS = {
    'SSI1': '#2980B9', 'SSI2': '#2980B9', 'SSI3': '#2980B9',
    'SSI4': '#2980B9', 'SSI5': '#2980B9',
    'SSI6': '#27AE60', 'SSI7': '#27AE60', 'SSI8': '#27AE60',
    'SSI9': '#27AE60', 'SSI10': '#27AE60', 'SSI11': '#27AE60',
    'SSI12': '#E67E22', 'SSI13': '#E67E22',
    'SSI14': '#E67E22', 'SSI15': '#E67E22',
    'SSI16': '#8E44AD', 'SSI17': '#8E44AD',
    'SSI18': '#8E44AD', 'SSI19': '#8E44AD',
}

# ─────────────────────────────────────────────────────────────
# Order by liwc_death β (ascending for bottom=weakest)
# ─────────────────────────────────────────────────────────────
order    = res_liwc.sort_values('coef')['item'].tolist()
y_pos    = {sc: i for i, sc in enumerate(order)}
y        = np.arange(len(order))
ylabels  = [SSI_LABELS.get(sc, sc) for sc in order]

# ─────────────────────────────────────────────────────────────
# FIGURE 1: Side-by-side forest plot
# ─────────────────────────────────────────────────────────────
fig, axes = plt.subplots(1, 2, figsize=(16, 8), sharey=True)
panels = [
    (res_liwc, 'liwc_death',
     'SSI item ~ liwc_death_z + time + age + sex + Dx + edu_yrs + (1|id)'),
    (res_f0,   'F0_qregc3',
     'SSI item ~ F0_qregc3_z + time + age + sex + Dx + AP_dose + (1|id)'),
]

for ax, (res, title, subtitle) in zip(axes, panels):
    res_idx = res.set_index('item')

    # background bands for domains (0-indexed y positions)
    domain_bounds = [(0, 5, '#EBF5FB'), (5, 11, '#EAFAF1'),
                     (11, 15, '#FEF9E7'), (15, 19, '#F5EEF8')]
    for ylo, yhi, col in domain_bounds:
        ax.axhspan(ylo - 0.5, yhi - 0.5, facecolor=col, alpha=0.35, zorder=0)

    for sc in order:
        if sc not in res_idx.index:
            continue
        row = res_idx.loc[sc]
        yi  = y_pos[sc]
        clr = DOMAIN_COLORS.get(sc, '#7F8C8D')

        # opacity by significance
        if row['q_fdr'] < 0.05:
            alpha = 0.90
        elif row['p'] < 0.05:
            alpha = 0.60
        else:
            alpha = 0.30

        ax.barh(yi, row['coef'], color=clr, alpha=alpha,
                edgecolor='white', height=0.70)
        ax.errorbar(row['coef'], yi,
                    xerr=[[row['coef'] - row['CI_lo']],
                           [row['CI_hi'] - row['coef']]],
                    fmt='none', ecolor='#2C3E50', elinewidth=1.2,
                    capsize=3, alpha=0.75)

        # significance markers
        if row['q_fdr'] < 0.05:
            ax.text(row['CI_hi'] + 0.005, yi, '[FDR]', va='center',
                    fontsize=7.5, color='#C0392B', fontweight='bold')
        elif row['p'] < 0.05:
            ax.text(row['CI_hi'] + 0.005, yi, '*', va='center',
                    fontsize=10, color='#E67E22', fontweight='bold')

    ax.axvline(0, color='#2C3E50', lw=1, ls='--', alpha=0.4)
    ax.set_xlabel('Standardized β  (95% CI)', fontsize=10.5)
    ax.set_title(f'{title}\n{subtitle}', fontsize=9.5, fontweight='bold', pad=8)
    ax.grid(axis='x', alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

axes[0].set_yticks(y)
axes[0].set_yticklabels(ylabels, fontsize=9.5)

# Domain legend
domain_patches = [
    mpatches.Patch(facecolor='#EBF5FB', edgecolor='#aaa', alpha=0.8, label='Ideation/Attitude (1–5)'),
    mpatches.Patch(facecolor='#EAFAF1', edgecolor='#aaa', alpha=0.8, label='Characteristics (6–11)'),
    mpatches.Patch(facecolor='#FEF9E7', edgecolor='#aaa', alpha=0.8, label='Plan & capability (12–15)'),
    mpatches.Patch(facecolor='#F5EEF8', edgecolor='#aaa', alpha=0.8, label='Behavioral acts (16–19)'),
    mpatches.Patch(facecolor='gray', alpha=0.90, label='q < 0.05 FDR (opaque)'),
    mpatches.Patch(facecolor='gray', alpha=0.30, label='p ≥ 0.05 (faded)'),
    mpatches.Patch(facecolor='#C0392B', alpha=0.40, label='[FDR] = q < 0.05'),
]

fig.legend(handles=domain_patches, fontsize=9, loc='lower center', ncol=4,
           bbox_to_anchor=(0.5, -0.06), framealpha=0.88)

plt.suptitle(
    'Associations of Speech/Language Features with Each SSI Item\n'
    '(Multilevel LME, N=103 patients, 323 observations; Y = SSI item z-score)',
    fontsize=11, fontweight='bold', y=1.02)
plt.tight_layout()
plt.savefig(f'{OUT}/ssi_item_forest.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ ssi_item_forest.png')


# ─────────────────────────────────────────────────────────────
# FIGURE 2: Scatter / dot-plot comparing β across two predictors
# β_liwc (x) vs β_F0 (y), with SSI item labels
# ─────────────────────────────────────────────────────────────
wide = pd.read_csv(f'{OUT}/ssi_item_lme_wide.csv')
wide['label'] = wide['item'].map(SSI_LABELS).fillna(wide['item'])
wide['color'] = wide['item'].map(DOMAIN_COLORS).fillna('#7F8C8D')

fig, ax = plt.subplots(figsize=(9, 8))

ax.scatter(wide['b_liwc'], wide['b_F0'],
           c=wide['color'], s=80, alpha=0.85, edgecolors='white', linewidths=0.7)

for _, row in wide.iterrows():
    ax.annotate(row['item'],
                xy=(row['b_liwc'], row['b_F0']),
                xytext=(4, 3), textcoords='offset points',
                fontsize=8, color='#333')

ax.axhline(0, color='#2C3E50', lw=0.8, ls='--', alpha=0.4)
ax.axvline(0, color='#2C3E50', lw=0.8, ls='--', alpha=0.4)

ax.set_xlabel('β  liwc_death → SSI item  (standardized)', fontsize=11)
ax.set_ylabel('β  F0_qregc3 → SSI item  (standardized)', fontsize=11)
ax.set_title('Comparing Speech Feature Associations Across SSI Items\n'
             '(each dot = 1 SSI item; colors = SSI domain)',
             fontsize=11, fontweight='bold')

domain_patches = [
    mpatches.Patch(color='#2980B9', alpha=0.85, label='Ideation/Attitude (1–5)'),
    mpatches.Patch(color='#27AE60', alpha=0.85, label='Characteristics (6–11)'),
    mpatches.Patch(color='#E67E22', alpha=0.85, label='Plan & capability (12–15)'),
    mpatches.Patch(color='#8E44AD', alpha=0.85, label='Behavioral acts (16–19)'),
]
ax.legend(handles=domain_patches, fontsize=9, loc='upper left', framealpha=0.88)

ax.grid(alpha=0.2, ls=':')
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

plt.tight_layout()
plt.savefig(f'{OUT}/ssi_item_scatter.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ ssi_item_scatter.png')


# ─────────────────────────────────────────────────────────────
# FIGURE 3: Heatmap-style summary table (effect size + sig)
# ─────────────────────────────────────────────────────────────
fig, ax = plt.subplots(figsize=(11, 7))
ax.axis('off')

# Build table: item | label | β_liwc | q_liwc | sig_liwc | β_F0 | q_F0 | sig_F0
rows_data = []
wide_sorted = wide.merge(
    res_liwc[['item', 'coef']].rename(columns={'coef': 'coef_liwc_sort'}),
    on='item').sort_values('coef_liwc_sort', ascending=False)

for _, row in wide_sorted.iterrows():
    sig_l = '[FDR]' if row['q_liwc'] < 0.05 else ('*' if row['p_liwc'] < 0.05 else '')
    sig_f = '[FDR]' if row['q_F0'] < 0.05 else ('*' if row['p_F0'] < 0.05 else '')
    rows_data.append([
        row['item'],
        SSI_LABELS.get(row['item'], row['item']),
        f"{row['b_liwc']:.3f}",
        f"{row['q_liwc']:.4f}",
        sig_l,
        f"{row['b_F0']:.3f}",
        f"{row['q_F0']:.4f}",
        sig_f,
    ])

col_labels = ['Item', 'Description',
              'β liwc_death', 'q liwc', 'Sig',
              'β F0_qregc3', 'q F0', 'Sig']

# Row colors
row_colors = []
for r in rows_data:
    itm = r[0]
    bg = DOMAIN_COLORS.get(itm, '#7F8C8D')
    # make it light
    light = {'#2980B9': '#D6EAF8', '#27AE60': '#D5F5E3',
              '#E67E22': '#FDEBD0', '#8E44AD': '#E8DAEF'}
    row_colors.append([light.get(bg, '#F2F3F4')] * 8)

tbl = ax.table(cellText=rows_data, colLabels=col_labels,
               cellLoc='left', loc='center',
               cellColours=row_colors)
tbl.auto_set_font_size(False)
tbl.set_fontsize(9)
tbl.scale(1, 1.55)
for (i, j), cell in tbl.get_celld().items():
    cell.set_edgecolor('#cccccc')
    if i == 0:
        cell.set_facecolor('#2C3E50')
        cell.set_text_props(color='white', fontweight='bold')
    # highlight FDR sig β cells
    if i > 0 and j in [2, 5]:  # β columns
        val_str = cell.get_text().get_text()
        try:
            val = float(val_str)
            sig_col = 4 if j == 2 else 7
        except:
            pass

ax.set_title('SSI Item LME: Effects of liwc_death and F0_qregc3\n'
             '(sorted by β liwc_death, descending)', fontsize=11, fontweight='bold', pad=10)
plt.tight_layout()
plt.savefig(f'{OUT}/ssi_item_table.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ ssi_item_table.png')
