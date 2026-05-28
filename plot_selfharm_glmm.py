"""Visualization: self_inj & suicide_behavior ~ liwc_death / F0_qregc3 (GLMM)"""

import pandas as pd, numpy as np
import matplotlib, matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.font_manager as fm
matplotlib.use('Agg')

fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial.ttf')
fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial_Bold.ttf')
plt.rcParams['font.family'] = 'Arial'

OUT = '/home/user/PRISM-V/results'
res = pd.read_csv(f'{OUT}/selfharm_glmm.csv')

# ── Outcome and predictor display names ──────────────────────
OUTCOME_LABELS = {
    'self_inj':          'Self-injury\n(non-suicidal)',
    'suicide_behavior':  'Suicide behavior\n(attempt/act)',
}
PRED_LABELS = {
    'liwc_death': 'liwc_death\n(death-related language)',
    'F0_qregc3':  'F0_qregc3\n(pitch contour curvature)',
}
PRED_COLORS = {
    'liwc_death': '#2980B9',
    'F0_qregc3':  '#E67E22',
}

# ═══════════════════════════════════════════════════════════════
# FIGURE 1: Forest plot — OR with 95% CI
# 2×2 grid: rows = outcomes, cols = predictors
# ═══════════════════════════════════════════════════════════════
fig, axes = plt.subplots(1, 2, figsize=(13, 5))

outcomes   = ['self_inj', 'suicide_behavior']
predictors = ['liwc_death', 'F0_qregc3']

for ax, pred in zip(axes, predictors):
    clr = PRED_COLORS[pred]
    sub = res[res['predictor'] == pred].copy()
    sub = sub.set_index('outcome').reindex(outcomes)

    y = np.arange(len(outcomes))
    ylabels = [OUTCOME_LABELS[o] for o in outcomes]

    ax.axvline(1, color='#2C3E50', lw=1.2, ls='--', alpha=0.45)

    for i, outcome in enumerate(outcomes):
        row = sub.loc[outcome]
        alpha = 0.90 if row['p'] < 0.05 else 0.40

        ax.barh(i, row['OR'] - 1, left=1, color=clr, alpha=alpha,
                edgecolor='white', height=0.55)
        ax.errorbar(row['OR'], i,
                    xerr=[[row['OR'] - row['OR_CI_lo']],
                           [row['OR_CI_hi'] - row['OR']]],
                    fmt='none', ecolor='#2C3E50', elinewidth=1.5,
                    capsize=5, alpha=0.85)

        # p-value annotation
        sig = ('***' if row['p'] < 0.001 else
               ('**'  if row['p'] < 0.01  else
                ('*'   if row['p'] < 0.05  else 'ns')))
        col_sig = '#C0392B' if row['p'] < 0.05 else '#888'
        ax.text(row['OR_CI_hi'] + 0.05, i,
                f"OR={row['OR']:.2f}  p={row['p']:.3f}  {sig}",
                va='center', fontsize=9, color=col_sig,
                fontweight='bold' if row['p'] < 0.05 else 'normal')

    ax.set_yticks(y)
    ax.set_yticklabels(ylabels, fontsize=11)
    ax.set_xlabel('Odds Ratio  (95% CI)', fontsize=10.5)
    ax.set_title(PRED_LABELS[pred], fontsize=11, fontweight='bold', pad=8)
    ax.set_xlim(0, ax.get_xlim()[1] * 1.35)
    ax.grid(axis='x', alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

plt.suptitle(
    'liwc_death & F0_qregc3 → Self-injury / Suicide Behavior\n'
    '(Bayesian Mixed GLMM, logistic; non-baseline visits only, N=89 patients)',
    fontsize=11, fontweight='bold', y=1.04)

sig_patch = mpatches.Patch(facecolor='gray', alpha=0.90, label='p < 0.05 (opaque)')
ns_patch  = mpatches.Patch(facecolor='gray', alpha=0.40, label='p ≥ 0.05 (faded)')
fig.legend(handles=[sig_patch, ns_patch], fontsize=9, loc='lower center',
           ncol=2, bbox_to_anchor=(0.5, -0.08), framealpha=0.88)

plt.tight_layout()
plt.savefig(f'{OUT}/selfharm_glmm_forest.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ selfharm_glmm_forest.png')

# ═══════════════════════════════════════════════════════════════
# FIGURE 2: Summary table
# ═══════════════════════════════════════════════════════════════
fig, ax = plt.subplots(figsize=(12, 3.5))
ax.axis('off')

rows_data = []
for _, row in res.iterrows():
    sig = ('***' if row['p'] < 0.001 else
           ('**'  if row['p'] < 0.01  else
            ('*'   if row['p'] < 0.05  else '')))
    rows_data.append([
        OUTCOME_LABELS[row['outcome']].replace('\n', ' '),
        PRED_LABELS[row['predictor']].replace('\n', ' '),
        f"{row['coef']:.3f}",
        f"{row['SD']:.3f}",
        f"{row['p']:.4f}",
        f"{row['OR']:.2f}",
        f"[{row['OR_CI_lo']:.2f} – {row['OR_CI_hi']:.2f}]",
        sig,
        f"{int(row['n'])} ({int(row['n_subj'])} pts)",
    ])

col_labels = ['Outcome', 'Predictor', 'log-OR', 'SD', 'p', 'OR', '95% CI', 'Sig', 'N (patients)']

row_colors = [
    ['#D6EAF8'] * 9,  # self_inj liwc
    ['#FDEBD0'] * 9,  # self_inj F0
    ['#D5F5E3'] * 9,  # suicide_behavior liwc
    ['#F5EEF8'] * 9,  # suicide_behavior F0
]

tbl = ax.table(cellText=rows_data, colLabels=col_labels,
               cellLoc='center', loc='center',
               cellColours=row_colors)
tbl.auto_set_font_size(False)
tbl.set_fontsize(9.5)
tbl.scale(1, 1.8)
for (i, j), cell in tbl.get_celld().items():
    cell.set_edgecolor('#cccccc')
    if i == 0:
        cell.set_facecolor('#2C3E50')
        cell.set_text_props(color='white', fontweight='bold')

ax.set_title('GLMM Results: Speech/Language → Self-harm (non-baseline visits)',
             fontsize=11, fontweight='bold', pad=12)
plt.tight_layout()
plt.savefig(f'{OUT}/selfharm_glmm_table.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ selfharm_glmm_table.png')
