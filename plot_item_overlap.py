"""
Item-level LME Overlap Visualization
- Venn diagram (SSI / liwc_death / F0_qregc3)
- β coefficient comparison scatter (β_SSI vs β_liwc)
- Forest plot of overlapping items
"""

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.font_manager as fm
from matplotlib.lines import Line2D
import warnings
warnings.filterwarnings('ignore')

# Arial
fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial.ttf')
fm.fontManager.addfont('/usr/share/fonts/truetype/msttcorefonts/Arial_Bold.ttf')
plt.rcParams['font.family'] = 'Arial'

OUT = '/home/user/PRISM-V/results'

# ── Item labels ──────────────────────────────
ITEM_LABELS = {
    'HAMD1':'Depressed mood','HAMD2':'Guilt','HAMD3':'Suicidal ideation',
    'HAMD4':'Early insomnia','HAMD5':'Middle insomnia','HAMD6':'Late insomnia',
    'HAMD7':'Work & activities','HAMD8':'Retardation','HAMD9':'Agitation',
    'HAMD10':'Psychic anxiety','HAMD11':'Somatic anxiety',
    'PHQ1':'Anhedonia','PHQ2':'Depressed mood','PHQ3':'Sleep problems',
    'PHQ4':'Fatigue','PHQ5':'Appetite change','PHQ6':'Worthlessness',
    'PHQ7':'Concentration','PHQ8':'Psychomotor change','PHQ9':'Suicidal thoughts',
    'BAI1':'Numbness','BAI2':'Feeling hot','BAI3':'Wobbly legs',
    'BAI4':'Unable to relax','BAI5':'Fear of worst','BAI6':'Dizzy',
    'BAI7':'Heart pounding','BAI8':'Unsteady','BAI9':'Terrified',
    'BAI10':'Nervous','BAI11':'Choking feeling','BAI12':'Hands trembling',
    'BAI13':'Shaky','BAI14':'Fear losing control','BAI15':'Dyspnea',
    'BAI16':'Fear of dying','BAI17':'Scared','BAI18':'Indigestion',
    'BAI19':'Faint/lightheaded','BAI20':'Face flushed','BAI21':'Hot/cold sweats',
    'BHOL1':'Optimism (R)','BHOL2':'Giving up','BHOL3':'Things going well (R)',
    'BHOL4':'No future','BHOL5':'Time to achieve','BHOL6':'Expected success (R)',
    'BHOL7':'Bleak future','BHOL8':'More good than bad (R)','BHOL9':'Cannot get what I want',
    'BHOL10':'Problems persist','BHOL11':'No interest in future',
    'BHOL12':'Things will not work','BHOL13':'Will succeed (R)',
    'BHOL14':'Things will go badly','BHOL15':'Long future (R)',
    'BHOL16':'Never get what I want','BHOL17':'Unlikely to be happy (R)',
    'BHOL18':'Uncertain future','BHOL19':'More sad than happy (R)',
    'BHOL20':'No point in trying',
}

SCALE_COLOR = {'HAMD':'#2980B9','PHQ':'#27AE60','BAI':'#E67E22','BHOL':'#8E44AD'}
def item_color(item):
    for p,c in SCALE_COLOR.items():
        if item.startswith(p): return c
    return '#7F8C8D'

def item_scale(item):
    for p in SCALE_COLOR:
        if item.startswith(p): return p
    return 'Other'

# ── Load results ─────────────────────────────
res_ssi  = pd.read_csv(f'{OUT}/item_lme_SSI.csv')
res_liwc = pd.read_csv(f'{OUT}/item_lme_liwc_death.csv')
res_f0   = pd.read_csv(f'{OUT}/item_lme_F0_qregc3.csv')
overlap  = pd.read_csv(f'{OUT}/item_lme_overlap.csv')

# FDR 기준 집합
sig_ssi  = set(res_ssi[res_ssi['q_fdr']  < 0.05]['item'])
sig_liwc = set(res_liwc[res_liwc['q_fdr'] < 0.05]['item'])
sig_f0   = set(res_f0[res_f0['q_fdr']   < 0.05]['item'])

only_ssi   = sig_ssi - sig_liwc - sig_f0
only_liwc  = sig_liwc - sig_ssi - sig_f0
only_f0    = sig_f0 - sig_ssi - sig_liwc
ssi_liwc   = (sig_ssi & sig_liwc) - sig_f0
ssi_f0     = (sig_ssi & sig_f0) - sig_liwc
liwc_f0    = (sig_liwc & sig_f0) - sig_ssi
all_three  = sig_ssi & sig_liwc & sig_f0

# ═══════════════════════════════════════════════════════════
# FIGURE 1: Venn diagram
# ═══════════════════════════════════════════════════════════
fig, ax = plt.subplots(figsize=(8, 6))
ax.set_xlim(0, 10); ax.set_ylim(0, 8); ax.axis('off')

# 3개 원
circles = [
    plt.Circle((3.5, 4.5), 2.8, color='#3498DB', alpha=0.25, linewidth=2, ec='#2980B9'),
    plt.Circle((6.5, 4.5), 2.8, color='#E74C3C', alpha=0.25, linewidth=2, ec='#C0392B'),
    plt.Circle((5.0, 2.3), 2.8, color='#9B59B6', alpha=0.25, linewidth=2, ec='#8E44AD'),
]
for c in circles: ax.add_patch(c)

# 라벨 (원 중심)
ax.text(2.2, 6.0, 'SSI',        fontsize=13, fontweight='bold', color='#2980B9', ha='center')
ax.text(7.8, 6.0, 'liwc_death', fontsize=13, fontweight='bold', color='#C0392B', ha='center')
ax.text(5.0, 0.4, 'F0_qregc3',  fontsize=13, fontweight='bold', color='#8E44AD', ha='center')

# 숫자
ax.text(2.5, 5.0, str(len(only_ssi)),  fontsize=18, fontweight='bold', ha='center', va='center', color='#2980B9')
ax.text(7.5, 5.0, str(len(only_liwc)), fontsize=18, fontweight='bold', ha='center', va='center', color='#C0392B')
ax.text(5.0, 1.5, str(len(only_f0)),   fontsize=18, fontweight='bold', ha='center', va='center', color='#8E44AD')

ax.text(4.8, 5.5, str(len(ssi_liwc)),  fontsize=16, fontweight='bold', ha='center', va='center', color='#555')
ax.text(3.5, 2.8, str(len(ssi_f0)),    fontsize=16, fontweight='bold', ha='center', va='center', color='#555')
ax.text(6.5, 2.8, str(len(liwc_f0)),   fontsize=16, fontweight='bold', ha='center', va='center', color='#555')
ax.text(5.0, 3.8, str(len(all_three)), fontsize=16, fontweight='bold', ha='center', va='center', color='#333')

# 교집합 item 이름 표시
if ssi_liwc:
    names = '\n'.join([f'{i}: {ITEM_LABELS.get(i,i)[:18]}' for i in sorted(ssi_liwc)])
    ax.text(5.0, 7.4, f'SSI ∩ liwc ({len(ssi_liwc)} items)\n{names}',
            fontsize=6.5, ha='center', va='top', color='#333',
            bbox=dict(boxstyle='round,pad=0.3', fc='white', ec='#ccc', alpha=0.85))

if ssi_f0:
    names = '\n'.join([f'{i}: {ITEM_LABELS.get(i,i)[:18]}' for i in sorted(ssi_f0)])
    ax.text(2.8, 0.7, f'SSI ∩ F0 ({len(ssi_f0)})\n{names}',
            fontsize=6.5, ha='center', va='bottom', color='#333',
            bbox=dict(boxstyle='round,pad=0.3', fc='white', ec='#ccc', alpha=0.85))

ax.set_title('Items Significantly Associated with Each Outcome\n(FDR q < 0.05, Multilevel LME)',
             fontsize=12, fontweight='bold', pad=10)
plt.tight_layout()
plt.savefig(f'{OUT}/item_venn.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ item_venn.png')

# ═══════════════════════════════════════════════════════════
# FIGURE 2: Scatter β_SSI vs β_liwc_death (all 67 items)
# ═══════════════════════════════════════════════════════════
merged_ssi_liwc = res_ssi[['item','coef','q_fdr']].rename(
    columns={'coef':'b_ssi','q_fdr':'q_ssi'}).merge(
    res_liwc[['item','coef','q_fdr']].rename(
    columns={'coef':'b_liwc','q_fdr':'q_liwc'}), on='item')

fig, ax = plt.subplots(figsize=(9, 7))

for _, row in merged_ssi_liwc.iterrows():
    sig_s = row['q_ssi']  < 0.05
    sig_l = row['q_liwc'] < 0.05
    if sig_s and sig_l:
        color, size, alpha, zorder = item_color(row['item']), 120, 0.95, 5
    elif sig_s:
        color, size, alpha, zorder = item_color(row['item']), 60,  0.55, 3
    elif sig_l:
        color, size, alpha, zorder = item_color(row['item']), 80,  0.75, 4
    else:
        color, size, alpha, zorder = '#BDC3C7', 30, 0.35, 2

    ax.scatter(row['b_ssi'], row['b_liwc'],
               c=color, s=size, alpha=alpha, zorder=zorder,
               edgecolors='white' if size > 50 else 'none', linewidths=0.5)

# 교집합 items에 라벨
for _, row in merged_ssi_liwc[
        (merged_ssi_liwc['q_ssi'] < 0.05) & (merged_ssi_liwc['q_liwc'] < 0.05)
    ].iterrows():
    lbl = f"{row['item']}\n({ITEM_LABELS.get(row['item'], '')[:14]})"
    ax.annotate(lbl, xy=(row['b_ssi'], row['b_liwc']),
                xytext=(8, 4), textcoords='offset points',
                fontsize=7.5, color='#222',
                arrowprops=dict(arrowstyle='-', color='#aaa', lw=0.7))

ax.axhline(0, color='gray', lw=0.8, ls='--', alpha=0.4)
ax.axvline(0, color='gray', lw=0.8, ls='--', alpha=0.4)

# 사분면 shading
ax.fill_between([0, ax.get_xlim()[1] if ax.get_xlim()[1] > 0 else 1],
                0, 0.5, color='#FADBD8', alpha=0.15, zorder=1)
ax.text(0.38, 0.3, 'Both ↑\n(overlap zone)',
        fontsize=8, color='#C0392B', alpha=0.7, style='italic', ha='center')

scale_patches = [mpatches.Patch(color=c, alpha=0.85, label=s) for s,c in SCALE_COLOR.items()]
both_marker = Line2D([0],[0], marker='o', color='w', markerfacecolor='#555',
                     markersize=11, label='Sig in BOTH (q<0.05)')
ssi_only    = Line2D([0],[0], marker='o', color='w', markerfacecolor='#aaa',
                     markersize=8, label='Sig in SSI only')
ax.legend(handles=scale_patches + [both_marker, ssi_only],
          fontsize=9, loc='upper left', framealpha=0.85)

ax.set_xlabel('β for SSI (standardized)', fontsize=11)
ax.set_ylabel('β for liwc_death (standardized)', fontsize=11)
ax.set_title('Item Association: SSI vs liwc_death\n'
             'Large dots = significant in BOTH outcomes (FDR q<0.05)',
             fontsize=11, fontweight='bold')
ax.grid(alpha=0.2, ls=':')
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
plt.tight_layout()
plt.savefig(f'{OUT}/item_scatter_ssi_vs_liwc.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ item_scatter_ssi_vs_liwc.png')

# ═══════════════════════════════════════════════════════════
# FIGURE 3: Forest plot — SSI∩liwc overlap items
# side-by-side β_SSI and β_liwc
# ═══════════════════════════════════════════════════════════
overlap_items = sorted(sig_ssi & sig_liwc)

df_liwc_sub = res_liwc[res_liwc['item'].isin(overlap_items)].set_index('item')
df_ssi_sub  = res_ssi[res_ssi['item'].isin(overlap_items)].set_index('item')

# BHS7 (SSI∩F0) 도 추가로 포함
extra_f0 = sorted(sig_ssi & sig_f0)
df_f0_sub = res_f0[res_f0['item'].isin(extra_f0)].set_index('item')
df_ssi_f0 = res_ssi[res_ssi['item'].isin(extra_f0)].set_index('item')

fig, axes = plt.subplots(1, 3, figsize=(17, 6))

# ── panel 1: β_SSI for overlap items ──
items_sorted = sorted(overlap_items, key=lambda x: df_ssi_sub.loc[x,'coef'])
y = np.arange(len(items_sorted))
colors = [item_color(i) for i in items_sorted]

axes[0].barh(y, [df_ssi_sub.loc[i,'coef'] for i in items_sorted],
             color=colors, alpha=0.85, edgecolor='white', height=0.7)
axes[0].errorbar([df_ssi_sub.loc[i,'coef'] for i in items_sorted], y,
                 xerr=[[df_ssi_sub.loc[i,'coef']-df_ssi_sub.loc[i,'CI_lo'] for i in items_sorted],
                        [df_ssi_sub.loc[i,'CI_hi']-df_ssi_sub.loc[i,'coef'] for i in items_sorted]],
                 fmt='none', ecolor='#444', elinewidth=1.2, capsize=3, alpha=0.8)
axes[0].axvline(0, color='black', lw=0.8, ls='--', alpha=0.3)
labels_0 = [f"{i}: {ITEM_LABELS.get(i,i)}" for i in items_sorted]
axes[0].set_yticks(y); axes[0].set_yticklabels(labels_0, fontsize=9)
axes[0].set_xlabel('β (standardized)', fontsize=10)
axes[0].set_title('β for SSI\n(items shared with liwc_death)', fontsize=10, fontweight='bold')

# ── panel 2: β_liwc for overlap items ──
axes[1].barh(y, [df_liwc_sub.loc[i,'coef'] for i in items_sorted],
             color=colors, alpha=0.85, edgecolor='white', height=0.7)
axes[1].errorbar([df_liwc_sub.loc[i,'coef'] for i in items_sorted], y,
                 xerr=[[df_liwc_sub.loc[i,'coef']-df_liwc_sub.loc[i,'CI_lo'] for i in items_sorted],
                        [df_liwc_sub.loc[i,'CI_hi']-df_liwc_sub.loc[i,'coef'] for i in items_sorted]],
                 fmt='none', ecolor='#444', elinewidth=1.2, capsize=3, alpha=0.8)
axes[1].axvline(0, color='black', lw=0.8, ls='--', alpha=0.3)
axes[1].set_yticks(y); axes[1].set_yticklabels([], fontsize=9)
axes[1].set_xlabel('β (standardized)', fontsize=10)
axes[1].set_title('β for liwc_death\n(same items)', fontsize=10, fontweight='bold')

# ── panel 3: SSI∩F0 items ──
if extra_f0:
    yf = np.arange(len(extra_f0))
    colorsf = [item_color(i) for i in extra_f0]
    b_ssi_f0  = [df_ssi_f0.loc[i,'coef'] for i in extra_f0]
    b_f0_vals = [df_f0_sub.loc[i,'coef'] for i in extra_f0]
    width = 0.3
    axes[2].barh(yf - width/2, b_ssi_f0,  height=width, color='#2980B9', alpha=0.8, label='β SSI')
    axes[2].barh(yf + width/2, b_f0_vals, height=width, color='#8E44AD', alpha=0.8, label='β F0_qregc3')
    axes[2].axvline(0, color='black', lw=0.8, ls='--', alpha=0.3)
    labels_f0 = [f"{i}: {ITEM_LABELS.get(i,i)}" for i in extra_f0]
    axes[2].set_yticks(yf); axes[2].set_yticklabels(labels_f0, fontsize=9)
    axes[2].set_xlabel('β (standardized)', fontsize=10)
    axes[2].set_title('SSI ∩ F0_qregc3\n(shared items)', fontsize=10, fontweight='bold')
    axes[2].legend(fontsize=9, loc='lower right')
else:
    axes[2].text(0.5,0.5,'No overlap\n(FDR)', ha='center', va='center', fontsize=12)
    axes[2].set_title('SSI ∩ F0_qregc3', fontsize=10, fontweight='bold')

for ax in axes:
    ax.grid(axis='x', alpha=0.2, ls=':')
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

scale_patches = [mpatches.Patch(color=c, alpha=0.85, label=s) for s,c in SCALE_COLOR.items()]
fig.legend(handles=scale_patches, fontsize=9, loc='lower center',
           ncol=4, bbox_to_anchor=(0.35, -0.04), framealpha=0.85)
fig.suptitle('Clinical Items Associated with Both Speech Features and SSI',
             fontsize=12, fontweight='bold', y=1.01)
plt.tight_layout()
plt.savefig(f'{OUT}/item_overlap_forest.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ item_overlap_forest.png')
