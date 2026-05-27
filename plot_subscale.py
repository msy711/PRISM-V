"""Subscale LME results visualization"""

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

res_ssi  = pd.read_csv(f'{OUT}/subscale_lme_SSI.csv')
res_liwc = pd.read_csv(f'{OUT}/subscale_lme_liwc_death.csv')
res_f0   = pd.read_csv(f'{OUT}/subscale_lme_F0_qregc3.csv')
wide     = pd.read_csv(f'{OUT}/subscale_lme_wide.csv')

SCALE_COLOR = {
    'HAMD': '#2980B9', 'PHQ': '#27AE60',
    'BAI':  '#E67E22', 'BHS': '#8E44AD',
}
def sc_color(name):
    for p,c in SCALE_COLOR.items():
        if name.startswith(p) or name.replace('BHS','BHOL').startswith('BH'):
            if 'HAMD' in name: return SCALE_COLOR['HAMD']
            if 'PHQ'  in name: return SCALE_COLOR['PHQ']
            if 'BAI'  in name: return SCALE_COLOR['BAI']
            if 'BHS'  in name: return SCALE_COLOR['BHS']
    return '#7F8C8D'

SC_LABELS = {
    'HAMD_Depression': 'HAMD: Core Depression',
    'HAMD_Anxiety':    'HAMD: Anxiety',
    'HAMD_Somatic':    'HAMD: Somatic',
    'HAMD_Sleep':      'HAMD: Sleep',
    'PHQ_CogAffect':   'PHQ: Cognitive/Affective',
    'PHQ_Somatic':     'PHQ: Somatic',
    'BAI_Somatic':     'BAI: Somatic Anxiety',
    'BAI_Cognitive':   'BAI: Cognitive Anxiety',
    'BHS_Hopeless':    'BHS: Hopelessness',
    'BHS_Hopeful':     'BHS: Hopefulness (R)',
}

REF_LABELS = {
    'HAMD_Depression': 'Shafer 2006\n(items 1,2,3,7,8)',
    'HAMD_Anxiety':    'Shafer 2006\n(items 9,10,11,15,17)',
    'HAMD_Somatic':    'Shafer 2006\n(items 12,13,14,16)',
    'HAMD_Sleep':      'Shafer 2006\n(items 4,5,6)',
    'PHQ_CogAffect':   'Boothroyd et al. 2019\n(items 1,2,6,9)',
    'PHQ_Somatic':     'Boothroyd et al. 2019\n(items 3,4,5,7,8)',
    'BAI_Somatic':     'Hewitt & Norton 1993\n(items 1–3,6–8,12,13,17–21)',
    'BAI_Cognitive':   'Hewitt & Norton 1993\n(items 4,5,9–11,14–16)',
    'BHS_Hopeless':    'Aish & Wasserman 2001\n(items 2,4,7,9,11,12,14,16–18,20)',
    'BHS_Hopeful':     'Aish & Wasserman 2001\n(items 1,3,5,6,8,10,13,15,19; R)',
}

# ═══════════════════════════════════════════════════════
# FIGURE: 3-panel side-by-side forest plot
# ═══════════════════════════════════════════════════════
fig, axes = plt.subplots(1, 3, figsize=(18, 7), sharey=True)

panels = [
    (res_ssi,  'SSI',        'SSI ~ subscale_z + time + age + sex + Dx + (1|id)'),
    (res_liwc, 'liwc_death', 'liwc_death ~ subscale_z + ... + edu_yrs + (1|id)'),
    (res_f0,   'F0_qregc3',  'F0_qregc3 ~ subscale_z + ... + AP_dose + (1|id)'),
]

# y order: sort by SSI β
order = res_ssi.sort_values('coef')['subscale'].tolist()
y_pos = {sc: i for i, sc in enumerate(order)}
y = np.arange(len(order))
ylabels = [SC_LABELS.get(sc, sc) for sc in order]

for ax, (res, title, subtitle) in zip(axes, panels):
    res_idx = res.set_index('subscale')

    for sc in order:
        if sc not in res_idx.index:
            continue
        row = res_idx.loc[sc]
        yi  = y_pos[sc]
        clr = sc_color(sc)

        # bar opacity by significance
        alpha = 0.90 if row['q_fdr'] < 0.05 else (0.55 if row['p'] < 0.05 else 0.30)

        ax.barh(yi, row['coef'], color=clr, alpha=alpha,
                edgecolor='white', height=0.65)
        ax.errorbar(row['coef'], yi,
                    xerr=[[row['coef'] - row['CI_lo']],
                           [row['CI_hi'] - row['coef']]],
                    fmt='none', ecolor='#333', elinewidth=1.2,
                    capsize=3, alpha=0.8)

        # significance marker
        if row['q_fdr'] < 0.05:
            ax.text(row['CI_hi'] + 0.01, yi, '[FDR]', va='center',
                    fontsize=7.5, color='#C0392B', fontweight='bold')
        elif row['p'] < 0.05:
            ax.text(row['CI_hi'] + 0.01, yi, '*', va='center',
                    fontsize=10, color='#E67E22', fontweight='bold')

    ax.axvline(0, color='#2C3E50', lw=1, ls='--', alpha=0.4)
    ax.set_xlabel('Standardized β', fontsize=10.5)
    ax.set_title(f'{title}\n{subtitle}', fontsize=9.5, fontweight='bold', pad=8)
    ax.grid(axis='x', alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

axes[0].set_yticks(y)
axes[0].set_yticklabels(ylabels, fontsize=10)

# Legend
scale_patches = [mpatches.Patch(color=c, alpha=0.85, label=s)
                 for s, c in SCALE_COLOR.items()]
fdr_star  = mpatches.Patch(facecolor='#C0392B', alpha=0.3,
                           label='[FDR] = q < 0.05')
nom_star  = Line2D([0],[0], marker='$*$', color='#E67E22', markersize=9,
                   linestyle='None', label='p < 0.05')
opaque    = mpatches.Patch(facecolor='gray', alpha=0.90, label='q < 0.05 (opaque)')
faded     = mpatches.Patch(facecolor='gray', alpha=0.30, label='p ≥ 0.05 (faded)')

fig.legend(handles=scale_patches + [fdr_star, nom_star, opaque, faded],
           fontsize=9, loc='lower center', ncol=4,
           bbox_to_anchor=(0.5, -0.06), framealpha=0.85)

plt.suptitle(
    'Subscale Scores Associated with SSI, liwc_death, F0_qregc3\n'
    '(Literature-based factor structures; Multilevel LME, N=104 patients)',
    fontsize=11, fontweight='bold', y=1.02)
plt.tight_layout()
plt.savefig(f'{OUT}/subscale_forest.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ subscale_forest.png')

# ═══════════════════════════════════════════════════════
# TABLE figure: subscale definitions
# ═══════════════════════════════════════════════════════
fig, ax = plt.subplots(figsize=(14, 5))
ax.axis('off')

col_labels = ['Subscale', 'Scale', 'Items (n)', 'Item numbers', 'Reference']
rows = [
    ['HAMD: Core Depression', 'HAMD-17', '5',  '1,2,3,7,8',              'Shafer 2006'],
    ['HAMD: Anxiety',         'HAMD-17', '5',  '9,10,11,15,17',          'Shafer 2006'],
    ['HAMD: Somatic',         'HAMD-17', '4',  '12,13,14,16',            'Shafer 2006'],
    ['HAMD: Sleep',           'HAMD-17', '3',  '4,5,6',                  'Shafer 2006'],
    ['PHQ: Cognitive/Affective','PHQ-9', '4',  '1,2,6,9',                'Boothroyd et al. 2019'],
    ['PHQ: Somatic',          'PHQ-9',   '5',  '3,4,5,7,8',              'Boothroyd et al. 2019'],
    ['BAI: Somatic Anxiety',  'BAI-21',  '13', '1,2,3,6,7,8,12,13,17,18,19,20,21','Hewitt & Norton 1993'],
    ['BAI: Cognitive Anxiety','BAI-21',  '8',  '4,5,9,10,11,14,15,16',   'Hewitt & Norton 1993'],
    ['BHS: Hopelessness',     'BHS-20',  '11', '2,4,7,9,11,12,14,16,17,18,20','Aish & Wasserman 2001'],
    ['BHS: Hopefulness (R)',  'BHS-20',  '9',  '1,3,5,6,8,10,13,15,19 (reversed)','Aish & Wasserman 2001'],
]

colors_row = []
scale_bg = {'HAMD-17':'#D6EAF8','PHQ-9':'#D5F5E3','BAI-21':'#FDEBD0','BHS-20':'#E8DAEF'}
for r in rows:
    bg = scale_bg.get(r[1], '#FFFFFF')
    colors_row.append([bg]*5)

tbl = ax.table(cellText=rows, colLabels=col_labels,
               cellLoc='left', loc='center',
               cellColours=colors_row)
tbl.auto_set_font_size(False)
tbl.set_fontsize(9)
tbl.scale(1, 1.6)
for (i,j), cell in tbl.get_celld().items():
    cell.set_edgecolor('#cccccc')
    if i == 0:
        cell.set_facecolor('#2C3E50')
        cell.set_text_props(color='white', fontweight='bold')

ax.set_title('Literature-based Subscale Definitions', fontsize=12, fontweight='bold', pad=10)
plt.tight_layout()
plt.savefig(f'{OUT}/subscale_definitions.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ subscale_definitions.png')
