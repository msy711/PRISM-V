import pandas as pd
import numpy as np
import networkx as nx
import pickle, os, sys, textwrap

sys.path.insert(0, os.path.dirname(__file__))
from svg_charts import bar_chart, grouped_bar_chart, forest_plot, km_curves, network_svg

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
FIG = "/home/user/PRISM-V/suicide_analysis/figures"

df = pd.read_pickle(os.path.join(OUT, "subject_level_with_factors.pkl"))
cases = df[df.case == 1]

# ============================================================ CHART 1: lifetime cascade
c1 = bar_chart([
    ("평생 자살사고(Suicidal ideation)", 65.7),
    ("구체적 자살계획(Plan)", 40.9),
    ("자살시도(Attempt)", 26.7),
], color_var="--series-2", title="lifetime suicidality cascade")

# ============================================================ CHART 2: current SCL SI, group compare
c2 = grouped_bar_chart(
    ["최근 1주 자살사고\n(SCL 문항 ≥ 2점)"],
    [("건강대조군", [11.0]), ("재발성 우울장애군", [55.6])],
    colors=("--series-1", "--series-2"), h=260,
)

# ============================================================ CHART 3: method / lethality
c3 = bar_chart([
    ("과량복용(Overdose)", 46.6),
    ("기타 방법", 28.0),
    ("목맴(Hanging)", 15.2),
    ("투신(Jumping)", 9.4),
], color_var="--series-4", title="method")

c4 = bar_chart([
    ("경미하거나 없음", 43.5),
    ("경도 손상", 18.4),
    ("중등도 손상", 18.1),
    ("중등도-중증 손상", 13.7),
    ("중증 손상", 6.4),
], color_var="--status-critical", title="damage severity")

# ============================================================ CHART 5: Model 1 forest plot
m1 = pd.read_csv(os.path.join(OUT, "model1_attempt_multivariable.csv"), index_col=0)
m1 = m1[~m1.index.str.startswith("marital_")]
label_map = {
    "dsm_symptom_count": "우울 증상 개수 (DSM 기준)",
    "SLEscore_": "생애 스트레스 생활사건 총점",
    "edu_level": "교육 수준",
    "age": "현재 연령",
    "Nscore_": "신경증 성향 (Neuroticism)",
    "panic_bin": "공황장애 동반",
    "age_onset": "발병 연령",
    "fh_man_ratio": "조증(양극성) 가족력 비율",
    "n_episodes": "우울 삽화 횟수",
    "fh_dep_ratio": "우울증 가족력 비율",
    "csa_any": "아동기 성적 학대 경험",
    "melancholia_bin": "멜랑콜리아 아형",
    "pbi_mother_care": "모 양육태도 - 돌봄(Care)",
    "pbi_mother_overprotect": "모 양육태도 - 과보호",
    "manic_bin": "조증 증상 동반",
    "social_support": "사회적 지지",
    "gad_bin": "범불안장애 동반",
    "pbi_father_overprotect": "부 양육태도 - 과보호",
    "atypical_bin": "비정형 증상 동반",
    "social_conflict": "사회적 갈등/마찰",
    "pbi_father_care": "부 양육태도 - 돌봄(Care)",
}
rows = []
for idx, r in m1.iterrows():
    sig = not (r.CI_low < 1 < r.CI_high)
    direction = "risk" if (sig and r.OR > 1) else ("protect" if (sig and r.OR < 1) else "ns")
    rows.append(dict(label=label_map.get(idx, idx), or_=r.OR, lo=r.CI_low, hi=r.CI_high, sig=sig, direction=direction))
rows = sorted(rows, key=lambda x: -x['or_'])
c5 = forest_plot(rows, h=len(rows)*26+90, xmax=3.2)

# ============================================================ CHART 6: factor scores forest
fac = pd.read_csv(os.path.join(OUT, "factor_scores_vs_attempt.csv"))
flabel = {
    "pbi_father_care": "부 양육태도 - 돌봄",
    "pbi_father_overprotect": "부 양육태도 - 과보호",
    "pbi_mother_care": "모 양육태도 - 돌봄",
    "pbi_mother_overprotect": "모 양육태도 - 과보호",
    "social_conflict": "사회적 갈등/마찰",
    "social_support": "사회적 지지",
}
frows = []
for _, r in fac.iterrows():
    sig = not (r.CI_low < 1 < r.CI_high)
    direction = "risk" if (sig and r.OR_per_SD > 1) else ("protect" if (sig and r.OR_per_SD < 1) else "ns")
    frows.append(dict(label=flabel[r['var']], or_=r.OR_per_SD, lo=r.CI_low, hi=r.CI_high, sig=sig, direction=direction))
c6 = forest_plot(frows, h=len(frows)*26+90, xmax=1.6, margin=(30,140,40,170))

# ============================================================ CHART 7/8: SCL networks (MDD vs Control)
with open(os.path.join(OUT, "networks.pkl"), "rb") as f:
    nets = pickle.load(f)

kor_label = {
    'scl.no.pleasure':'즐거움 상실','scl.no.interest':'흥미 상실','scl.hopeless':'절망감',
    'scl.effortful':'매사 힘겨움','scl.worthless':'무가치감','scl.poor.appetite':'식욕부진',
    'scl.fall.asleep':'입면곤란','scl.restless.sleep':'수면중 뒤척임','scl.low.energy':'기력저하',
    'scl.suicide.thought':'자살사고','scl.cry':'울음','scl.trapped':'덫에 갇힌 느낌',
    'scl.blame':'자기비난','scl.lonely':'외로움','scl.blue':'우울/침울','scl.worry':'걱정'
}

def make_network_chart(label, seed=7):
    G = nets[label]['graph']
    pos = nx.spring_layout(G, weight='weight', seed=seed, k=0.9, iterations=300)
    return network_svg(G, pos, 'scl.suicide.thought', kor_label, w=520, h=440,
                        node_color="--series-1" if label == "Control" else "--series-2",
                        highlight_color="--status-critical")

c7 = make_network_chart("MDD")
c8 = make_network_chart("Control")

# ============================================================ CHART 9: risk-factor network
with open(os.path.join(OUT, "riskfactor_network.pkl"), "rb") as f:
    rfn = pickle.load(f)
rf_kor = {
    "suicidality_severity": "자살행동 심각도",
    "dsm_symptom_count": "우울증상 개수",
    "n_episodes": "삽화 횟수",
    "age_onset": "발병 연령",
    "melancholia_bin": "멜랑콜리아",
    "gad_bin": "범불안장애",
    "panic_bin": "공황장애",
    "psy_inpatient": "입원력",
    "Nscore_": "신경증 성향",
    "csa_any": "아동기 성적학대",
    "fh_dep_ratio": "우울 가족력",
    "fh_man_ratio": "조증 가족력",
    "pbi_mother_care": "모 돌봄",
    "pbi_mother_overprotect": "모 과보호",
    "social_support": "사회적 지지",
    "social_conflict": "사회적 갈등",
    "SLEscore_": "생애 스트레스 사건",
}
Grf = rfn['graph']
posrf = nx.spring_layout(Grf, weight='weight', seed=11, k=1.0, iterations=300)
c9 = network_svg(Grf, posrf, 'suicidality_severity', rf_kor, w=620, h=520,
                  node_color="--series-3", highlight_color="--status-critical")

# ============================================================ CHART 10-12: KM curves
def load_km(fname):
    d = pd.read_csv(os.path.join(OUT, fname))
    curves = []
    for g, gd in d.groupby("group", sort=False):
        curves.append(dict(name=g, times=gd.time.tolist(), surv=gd.survival.tolist()))
    return curves

c10 = km_curves(load_km("km_CSA_any_vs_none.csv"), title="CSA")
c11 = km_curves(load_km("km_Melancholic_subtype.csv"), title="melancholia")
c12 = km_curves(load_km("km_Early_<=25_vs_later_onset.csv"), title="onset")

# ============================================================ CHART 13: kindling
kind = pd.read_csv(os.path.join(OUT, "lhc_kindling_by_episode_number.csv"))
kind = kind[kind.episode_number <= 6]
c13 = bar_chart([(f"{int(r.episode_number)}번째 삽화", r['mean']*100) for _, r in kind.iterrows()],
                 color_var="--series-3", h=300)

# ============================================================ CHART 14: episode severity attempters vs non
c14 = grouped_bar_chart(
    ["삽화당 평균 생활사건\n심각도 지수"],
    [("평생 자살시도 없음", [0.217]), ("평생 자살시도 있음", [0.500])],
    colors=("--series-1", "--series-2"), unit="", value_fmt=lambda v: f"{v:.2f}", h=260,
)

charts = dict(c1=c1, c2=c2, c3=c3, c4=c4, c5=c5, c6=c6, c7=c7, c8=c8, c9=c9,
              c10=c10, c11=c11, c12=c12, c13=c13, c14=c14)
with open(os.path.join(OUT, "charts.pkl"), "wb") as f:
    pickle.dump(charts, f)

print("Charts generated:", list(charts.keys()))
for k, v in charts.items():
    print(k, len(v), "chars")
