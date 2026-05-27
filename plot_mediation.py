"""
Multilevel Mediation Results Visualization
- Forest plots of indirect effects (a×b) per predictor
- Path diagram for top mediators
- Font: Arial
"""

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.font_manager as fm
import matplotlib.gridspec as gridspec
from matplotlib.patches import FancyArrowPatch
import warnings
warnings.filterwarnings('ignore')

# Arial 폰트
fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial.ttf')
fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial_Bold.ttf')
plt.rcParams['font.family'] = 'Arial'

OUT = '/home/user/PRISM-V/results'

# ── Item labels ─────────────────────────────────────────────
ITEM_LABELS = {
    # HAMD-17
    'HAMD1':  'HAMD1: Depressed mood',
    'HAMD2':  'HAMD2: Guilt feelings',
    'HAMD3':  'HAMD3: Suicidal ideation',
    'HAMD4':  'HAMD4: Early insomnia',
    'HAMD5':  'HAMD5: Middle insomnia',
    'HAMD6':  'HAMD6: Late insomnia',
    'HAMD7':  'HAMD7: Work & activities',
    'HAMD8':  'HAMD8: Psychomotor retardation',
    'HAMD9':  'HAMD9: Agitation',
    'HAMD10': 'HAMD10: Psychic anxiety',
    'HAMD11': 'HAMD11: Somatic anxiety',
    'HAMD12': 'HAMD12: GI somatic symptoms',
    'HAMD13': 'HAMD13: General somatic',
    'HAMD14': 'HAMD14: Genital symptoms',
    'HAMD15': 'HAMD15: Hypochondriasis',
    'HAMD16': 'HAMD16: Weight loss',
    'HAMD17': 'HAMD17: Insight',
    # PHQ-9
    'PHQ1': 'PHQ1: Little interest/pleasure',
    'PHQ2': 'PHQ2: Feeling down/depressed',
    'PHQ3': 'PHQ3: Sleep problems',
    'PHQ4': 'PHQ4: Fatigue/low energy',
    'PHQ5': 'PHQ5: Poor appetite/overeating',
    'PHQ6': 'PHQ6: Feeling bad about self',
    'PHQ7': 'PHQ7: Trouble concentrating',
    'PHQ8': 'PHQ8: Psychomotor changes',
    'PHQ9': 'PHQ9: Thoughts of death/SI',
    # BAI-21
    'BAI1':  'BAI1: Numbness/tingling',
    'BAI2':  'BAI2: Feeling hot',
    'BAI3':  'BAI3: Wobbly legs',
    'BAI4':  'BAI4: Unable to relax',
    'BAI5':  'BAI5: Fear of worst happening',
    'BAI6':  'BAI6: Dizzy/lightheaded',
    'BAI7':  'BAI7: Heart pounding',
    'BAI8':  'BAI8: Unsteady',
    'BAI9':  'BAI9: Terrified',
    'BAI10': 'BAI10: Nervous',
    'BAI11': 'BAI11: Feeling of choking',
    'BAI12': 'BAI12: Hands trembling',
    'BAI13': 'BAI13: Shaky/unsteady',
    'BAI14': 'BAI14: Fear of losing control',
    'BAI15': 'BAI15: Difficulty breathing',
    'BAI16': 'BAI16: Fear of dying',
    'BAI17': 'BAI17: Scared',
    'BAI18': 'BAI18: Indigestion',
    'BAI19': 'BAI19: Faint/lightheaded',
    'BAI20': 'BAI20: Face flushed',
    'BAI21': 'BAI21: Hot/cold sweats',
    # BHS-20 (Beck Hopelessness Scale)
    'BHOL1':  'BHS1: Future optimism (R)',
    'BHOL2':  'BHS2: Giving up',
    'BHOL3':  'BHS3: Things going well (R)',
    'BHOL4':  'BHS4: Cannot imagine future',
    'BHOL5':  'BHS5: Time enough to achieve',
    'BHOL6':  'BHS6: Expected success (R)',
    'BHOL7':  'BHS7: Bleak future',
    'BHOL8':  'BHS8: More good than bad (R)',
    'BHOL9':  'BHS9: Cannot get what I want',
    'BHOL10': 'BHS10: Past problems persist',
    'BHOL11': 'BHS11: No interest in future',
    'BHOL12': 'BHS12: Things will not work out',
    'BHOL13': 'BHS13: Will succeed (R)',
    'BHOL14': 'BHS14: Things will not go well',
    'BHOL15': 'BHS15: Long future (R)',
    'BHOL16': 'BHS16: Never get what I want',
    'BHOL17': 'BHS17: Unlikely to be happy (R)',
    'BHOL18': 'BHS18: Vague/uncertain future',
    'BHOL19': 'BHS19: More happy than sad (R)',
    'BHOL20': 'BHS20: No point in trying',
}

# ── Scale colors ─────────────────────────────────────────────
SCALE_COLOR = {
    'HAMD': '#2980B9',
    'PHQ':  '#27AE60',
    'BAI':  '#E67E22',
    'BHOL': '#8E44AD',
}
def item_color(item):
    for prefix, color in SCALE_COLOR.items():
        if item.startswith(prefix):
            return color
    return '#7F8C8D'

# ── Load results ─────────────────────────────────────────────
liwc_res = pd.read_csv(f'{OUT}/mediation_liwc_death.csv')
f0_res   = pd.read_csv(f'{OUT}/mediation_F0_qregc3.csv')

# ─────────────────────────────────────────────
# FIGURE 1 & 2: Forest plots (one per predictor)
# ─────────────────────────────────────────────
def forest_plot(res_df, predictor_label, fname, top_n=25):
    # indirect effect 절대값 순 상위 top_n
    show = res_df.head(top_n).copy()
    show['label'] = show['mediator'].map(ITEM_LABELS).fillna(show['mediator'])
    show = show.sort_values('indirect')

    fig, ax = plt.subplots(figsize=(12, max(7, len(show) * 0.52)))
    y = np.arange(len(show))

    colors  = [item_color(m) for m in show['mediator']]
    alphas  = [0.90 if s else 0.45 for s in show['sig_MC']]
    edge    = ['white' if s else '#999' for s in show['sig_MC']]

    bars = ax.barh(y, show['indirect'], color=colors, alpha=0.85,
                   edgecolor='white', height=0.70)

    # 95% CI
    ax.errorbar(show['indirect'], y,
                xerr=[show['indirect'] - show['CI_lo'],
                      show['CI_hi'] - show['indirect']],
                fmt='none', ecolor='#2C3E50', elinewidth=1.3,
                capsize=3.5, alpha=0.75)

    ax.axvline(0, color='#2C3E50', lw=1.2, ls='--', alpha=0.4)

    ax.set_yticks(y)
    ax.set_yticklabels(show['label'], fontsize=9.5)

    # significance marker
    for i, (_, row) in enumerate(show.iterrows()):
        if row['sig_MC']:
            x = row['CI_hi'] + 0.02
            ax.text(x, i, '★', va='center', fontsize=9, color='#C0392B', fontweight='bold')

    # scale legend
    scale_patches = [mpatches.Patch(color=c, alpha=0.85, label=s)
                     for s, c in SCALE_COLOR.items()]
    sig_patch = Line2D([0], [0], marker='*', color='#C0392B', markersize=9,
                       linestyle='None', label='MC 95% CI excludes 0')
    ax.legend(handles=scale_patches + [sig_patch],
              fontsize=9, loc='lower right', framealpha=0.85)

    ax.set_xlabel('Indirect Effect (a × b, standardized)', fontsize=11)
    ax.set_title(f'Mediation: {predictor_label} → [Mediator] → SSI\n'
                 f'Multilevel mediation (1-1-1 design), Monte Carlo 95% CI (n=5000)',
                 fontsize=11, fontweight='bold', pad=10)
    ax.grid(axis='x', alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

    plt.tight_layout()
    plt.savefig(fname, dpi=150, bbox_inches='tight')
    plt.close()
    print(f'✓ {fname}')

# Need Line2D import at top level
from matplotlib.lines import Line2D

forest_plot(liwc_res, 'liwc_death (death-related language)',
            f'{OUT}/mediation_forest_liwc_death.png', top_n=25)
forest_plot(f0_res, 'F0_qregc3 (pitch contour curvature)',
            f'{OUT}/mediation_forest_F0_qregc3.png', top_n=20)

# ─────────────────────────────────────────────
# FIGURE 3: Side-by-side summary (sig mediators only)
# ─────────────────────────────────────────────
fig, axes = plt.subplots(1, 2, figsize=(18, 10))

panels = [
    (liwc_res, 'liwc_death\n(death-related language)'),
    (f0_res,   'F0_qregc3\n(pitch contour curvature)'),
]

for ax, (res_df, xlabel) in zip(axes, panels):
    sig = res_df[res_df['sig_MC']].copy()
    if len(sig) == 0:
        ax.text(0.5, 0.5, 'No significant\nmediators', ha='center', va='center', fontsize=13)
        ax.set_title(xlabel, fontsize=11, fontweight='bold')
        continue

    sig = sig.sort_values('indirect')
    sig['label'] = sig['mediator'].map(ITEM_LABELS).fillna(sig['mediator'])
    y = np.arange(len(sig))
    colors = [item_color(m) for m in sig['mediator']]

    ax.barh(y, sig['indirect'], color=colors, alpha=0.85, edgecolor='white', height=0.72)
    ax.errorbar(sig['indirect'], y,
                xerr=[sig['indirect'] - sig['CI_lo'],
                      sig['CI_hi']    - sig['indirect']],
                fmt='none', ecolor='#2C3E50', elinewidth=1.3,
                capsize=3.5, alpha=0.75)
    ax.axvline(0, color='#2C3E50', lw=1, ls='--', alpha=0.35)

    # Proportion mediated annotation
    for i, (_, row) in enumerate(sig.iterrows()):
        pm = row['prop_mediated']
        if pd.notna(pm):
            x = row['CI_hi'] + 0.01
            ax.text(x, i, f'{pm:.0%}', va='center', fontsize=8,
                    color='#555', style='italic')

    ax.set_yticks(y)
    ax.set_yticklabels(sig['label'], fontsize=9)
    ax.set_xlabel('Indirect Effect a×b (standardized)', fontsize=10.5)
    ax.set_title(f'{xlabel}\n→ [Mediator] → SSI\n(★ MC 95% CI excludes 0)',
                 fontsize=10.5, fontweight='bold', pad=8)

    scale_patches = [mpatches.Patch(color=c, alpha=0.85, label=s)
                     for s, c in SCALE_COLOR.items()
                     if any(row['mediator'].startswith(s) for _, row in sig.iterrows())]
    ax.legend(handles=scale_patches, fontsize=9, loc='lower right', framealpha=0.85)
    ax.grid(axis='x', alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

plt.tight_layout()
plt.savefig(f'{OUT}/mediation_summary.png', dpi=150, bbox_inches='tight')
plt.close()
print(f'✓ {OUT}/mediation_summary.png')

# ─────────────────────────────────────────────
# FIGURE 4: a-path & b-path decomposition (top 10, liwc_death)
# ─────────────────────────────────────────────
fig, axes = plt.subplots(1, 3, figsize=(18, 8))
sig_liwc = liwc_res[liwc_res['sig_MC']].sort_values('indirect', ascending=False).head(12)
sig_liwc['label'] = sig_liwc['mediator'].map(ITEM_LABELS).fillna(sig_liwc['mediator'])
sig_liwc = sig_liwc.sort_values('indirect')
y = np.arange(len(sig_liwc))
colors = [item_color(m) for m in sig_liwc['mediator']]

# a-path
axes[0].barh(y, sig_liwc['a'], color=colors, alpha=0.85, edgecolor='white', height=0.7)
axes[0].axvline(0, color='black', lw=0.8, ls='--', alpha=0.4)
axes[0].set_yticks(y); axes[0].set_yticklabels(sig_liwc['label'], fontsize=9)
axes[0].set_xlabel('a coefficient', fontsize=10.5)
axes[0].set_title('a-path\nliwc_death → Mediator', fontsize=10.5, fontweight='bold')

# b-path
axes[1].barh(y, sig_liwc['b'], color=colors, alpha=0.85, edgecolor='white', height=0.7)
axes[1].axvline(0, color='black', lw=0.8, ls='--', alpha=0.4)
axes[1].set_yticks(y); axes[1].set_yticklabels([], fontsize=9)
axes[1].set_xlabel('b coefficient', fontsize=10.5)
axes[1].set_title('b-path\nMediator → SSI\n(controlling for liwc_death)', fontsize=10.5, fontweight='bold')

# indirect a×b
axes[2].barh(y, sig_liwc['indirect'], color=colors, alpha=0.85, edgecolor='white', height=0.7)
axes[2].errorbar(sig_liwc['indirect'], y,
                 xerr=[sig_liwc['indirect'] - sig_liwc['CI_lo'],
                       sig_liwc['CI_hi']    - sig_liwc['indirect']],
                 fmt='none', ecolor='#2C3E50', elinewidth=1.3, capsize=3.5, alpha=0.8)
axes[2].axvline(0, color='black', lw=0.8, ls='--', alpha=0.4)
axes[2].set_yticks(y); axes[2].set_yticklabels([], fontsize=9)
axes[2].set_xlabel('Indirect effect (a × b)', fontsize=10.5)
axes[2].set_title('Indirect effect\na × b (MC 95% CI)', fontsize=10.5, fontweight='bold')

# prop mediated on right
for i, (_, row) in enumerate(sig_liwc.iterrows()):
    pm = row['prop_mediated']
    if pd.notna(pm):
        axes[2].text(row['CI_hi'] + 0.01, i, f'{pm:.0%}', va='center',
                     fontsize=8, color='#444', style='italic')

for ax in axes:
    ax.grid(axis='x', alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

scale_patches = [mpatches.Patch(color=c, alpha=0.85, label=s)
                 for s, c in SCALE_COLOR.items()]
fig.legend(handles=scale_patches, fontsize=9, loc='lower center',
           ncol=4, bbox_to_anchor=(0.5, -0.02), framealpha=0.85)
fig.suptitle('Decomposition of Mediation Paths: liwc_death → [Items] → SSI',
             fontsize=12, fontweight='bold', y=1.01)
plt.tight_layout()
plt.savefig(f'{OUT}/mediation_pathdecomp_liwc.png', dpi=150, bbox_inches='tight')
plt.close()
print(f'✓ {OUT}/mediation_pathdecomp_liwc.png')
