import pickle, os
import pandas as pd

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
with open(os.path.join(OUT, "charts.pkl"), "rb") as f:
    C = pickle.load(f)

REPORT_PATH = "/home/user/PRISM-V/suicide_analysis/report.html"

CSS = """
<style>
:root{
  --page:#f6f5f1; --surface-1:#ffffff; --surface-2:#f0eee7;
  --text-primary:#17150f; --text-secondary:#55534a; --text-muted:#8c897c;
  --gridline:#e4e1d6; --axis:#c7c3b3; --border:rgba(23,21,15,0.11);
  --series-1:#2a78d6; --series-2:#eb6834; --series-3:#1baf7a; --series-4:#eda100;
  --status-good:#0ca30c; --status-critical:#d03b3b;
  --accent:#1c5cab; --accent-soft:#e8eefa;
  color-scheme:light;
}
@media (prefers-color-scheme: dark){
  :root:not([data-theme="light"]){
    --page:#121210; --surface-1:#1b1a17; --surface-2:#211f1b;
    --text-primary:#f5f4ef; --text-secondary:#c3c2b7;
    --text-muted:#938f83; --gridline:#2c2c28; --axis:#3a3833; --border:rgba(255,255,255,0.11);
    --series-1:#3987e5; --series-2:#d95926; --series-3:#199e70; --series-4:#c98500;
    --status-good:#0ca30c; --status-critical:#e66767;
    --accent:#6ba3ea; --accent-soft:#1e2a3d;
    color-scheme:dark;
  }
}
:root[data-theme="dark"]{
  --page:#121210; --surface-1:#1b1a17; --surface-2:#211f1b;
  --text-primary:#f5f4ef; --text-secondary:#c3c2b7; --text-muted:#938f83;
  --gridline:#2c2c28; --axis:#3a3833; --border:rgba(255,255,255,0.11);
  --series-1:#3987e5; --series-2:#d95926; --series-3:#199e70; --series-4:#c98500;
  --status-good:#0ca30c; --status-critical:#e66767;
  --accent:#6ba3ea; --accent-soft:#1e2a3d;
  color-scheme:dark;
}
*{box-sizing:border-box;}
body{
  margin:0; background:var(--page); color:var(--text-primary);
  font-family:-apple-system,'Apple SD Gothic Neo','Malgun Gothic',system-ui,sans-serif;
  line-height:1.72; font-size:16px; -webkit-font-smoothing:antialiased;
}
.wrap{max-width:920px; margin:0 auto; padding:0 24px 96px;}
header.hero{
  background:linear-gradient(180deg,var(--surface-1),var(--page));
  border-bottom:1px solid var(--border); padding:52px 0 30px;
}
header.hero .wrap{padding-top:0;}
.eyebrow{font-size:12.5px; letter-spacing:.09em; text-transform:uppercase; color:var(--accent); font-weight:700;}
h1.title{font-size:clamp(26px,4vw,38px); line-height:1.28; margin:10px 0 12px; text-wrap:balance; letter-spacing:-0.01em;}
.subtitle{color:var(--text-secondary); font-size:16.5px; max-width:70ch; margin:0 0 26px;}
.stat-strip{display:grid; grid-template-columns:repeat(auto-fit,minmax(140px,1fr)); gap:1px; background:var(--border); border:1px solid var(--border); border-radius:12px; overflow:hidden; margin-top:10px;}
.stat{background:var(--surface-1); padding:16px 18px;}
.stat .n{font-variant-numeric:tabular-nums; font-size:23px; font-weight:750; letter-spacing:-0.02em;}
.stat .l{font-size:12.5px; color:var(--text-muted); margin-top:3px;}
nav.toc{display:flex; flex-wrap:wrap; gap:8px; margin:26px 0 8px;}
nav.toc a{font-size:13px; color:var(--text-secondary); text-decoration:none; background:var(--surface-2); border:1px solid var(--border); padding:6px 12px; border-radius:99px; white-space:nowrap;}
nav.toc a:hover{color:var(--accent); border-color:var(--accent);}
section{padding:44px 0; border-bottom:1px solid var(--border);}
section:last-of-type{border-bottom:none;}
.sec-num{color:var(--accent); font-weight:700; font-size:13.5px; letter-spacing:.04em;}
h2{font-size:25px; margin:6px 0 6px; letter-spacing:-0.01em; text-wrap:balance;}
h3{font-size:18.5px; margin:30px 0 10px; letter-spacing:-0.005em;}
h4{font-size:15.5px; margin:22px 0 8px; color:var(--text-primary);}
p{color:var(--text-primary); max-width:74ch;}
p.lede{color:var(--text-secondary); font-size:16.5px; max-width:72ch;}
.card{background:var(--surface-1); border:1px solid var(--border); border-radius:14px; padding:22px 24px; margin:18px 0;}
.grid2{display:grid; grid-template-columns:1fr 1fr; gap:20px;}
.grid3{display:grid; grid-template-columns:repeat(3,1fr); gap:16px;}
@media (max-width:760px){.grid2,.grid3{grid-template-columns:1fr;}}
figure{margin:18px 0;}
figcaption{font-size:13px; color:var(--text-muted); margin-top:10px; max-width:70ch; line-height:1.55;}
figcaption b{color:var(--text-secondary);}
.tablewrap{overflow-x:auto; border:1px solid var(--border); border-radius:12px; margin:18px 0;}
table{border-collapse:collapse; width:100%; font-size:13.6px; min-width:520px;}
th,td{padding:9px 13px; text-align:left; border-bottom:1px solid var(--border); white-space:nowrap;}
th{background:var(--surface-2); color:var(--text-secondary); font-weight:650; font-size:12px; text-transform:uppercase; letter-spacing:.03em;}
td{font-variant-numeric:tabular-nums; color:var(--text-primary);}
tr:last-child td{border-bottom:none;}
.tag{display:inline-flex; align-items:center; gap:6px; font-size:12.5px; font-weight:650; padding:3px 10px; border-radius:99px;}
.tag-mdd{background:color-mix(in srgb, var(--series-2) 16%, transparent); color:var(--series-2);}
.tag-ctrl{background:color-mix(in srgb, var(--series-1) 16%, transparent); color:var(--series-1);}
.tag-risk{background:color-mix(in srgb, var(--status-critical) 15%, transparent); color:var(--status-critical);}
.tag-protect{background:color-mix(in srgb, var(--status-good) 16%, transparent); color:var(--status-good);}
.callout{border-left:3px solid var(--accent); background:var(--accent-soft); border-radius:0 10px 10px 0; padding:14px 18px; margin:18px 0; font-size:14.6px; color:var(--text-secondary);}
.callout b{color:var(--text-primary);}
.finding{border:1px solid var(--border); border-radius:12px; padding:16px 18px; margin:14px 0; background:var(--surface-1);}
.finding .h{font-weight:700; font-size:14.8px; margin-bottom:4px;}
.finding .d{font-size:14px; color:var(--text-secondary);}
.reflist{list-style:none; padding:0; margin:14px 0; display:flex; flex-direction:column; gap:10px;}
.reflist li{font-size:13.8px; color:var(--text-secondary); padding-left:18px; position:relative;}
.reflist li::before{content:"—"; position:absolute; left:0; color:var(--text-muted);}
.reflist a{color:var(--accent); text-decoration:none;}
.reflist a:hover{text-decoration:underline;}
code{background:var(--surface-2); padding:1px 6px; border-radius:5px; font-size:.9em;}
.limits li{margin:6px 0; color:var(--text-secondary); font-size:14.6px;}
footer{padding:36px 0 10px; color:var(--text-muted); font-size:12.5px;}
.small{font-size:12.5px; color:var(--text-muted);}
::selection{background:var(--accent-soft);}
a{color:var(--accent);}
</style>
"""

HEAD = "<title>자살 위험요인 아틀라스</title>\n" + CSS
with open(os.path.join(OUT, "_head.html"), "w") as f:
    f.write(HEAD)

B = []  # body accumulator

# ================================================================ HERO
B.append("""
<header class="hero"><div class="wrap">
<div class="eyebrow">KOMOGEN 여성 표본 · 통합 통계분석 보고서</div>
<h1 class="title">재발성 우울장애 한국인 여성의 자살 위험요인 통합분석</h1>
<p class="subtitle">건강대조군을 포함한 6,049명(재발성 주요우울장애 3,000명, 건강대조군 3,049명)의 구조화 면담 자료와,
환자군 2,998명·8,889개 우울 삽화의 생애사 달력(Life History Calendar) 자료를 결합하여, 자살사고·계획·시도의 유병률,
위험요인, 증상 네트워크 구조, 발병 이후 시간경과에 따른 위험을 검토했습니다.</p>
<div class="stat-strip">
  <div class="stat"><div class="n">6,049</div><div class="l">전체 대상자 (여성)</div></div>
  <div class="stat"><div class="n">3,000</div><div class="l">재발성 우울장애군</div></div>
  <div class="stat"><div class="n">3,049</div><div class="l">건강대조군</div></div>
  <div class="stat"><div class="n">26.7%</div><div class="l">평생 자살시도율 (환자군)</div></div>
  <div class="stat"><div class="n">65.7%</div><div class="l">평생 자살사고율 (환자군)</div></div>
  <div class="stat"><div class="n">8,889</div><div class="l">분석된 우울 삽화 수</div></div>
</div>
<nav class="toc">
  <a href="#intro">1. 배경</a><a href="#methods">2. 자료·방법</a><a href="#prevalence">3. 유병률</a>
  <a href="#riskfactors">4. 위험요인</a><a href="#repeat">5. 반복시도</a><a href="#fullsample">6. 전체표본 SI</a>
  <a href="#factor">7. 요인분석</a><a href="#network">8. 증상 네트워크</a><a href="#rfnetwork">9. 위험요인 네트워크</a>
  <a href="#survival">10. 발병-시도 시간</a><a href="#lhc">11. 삽화 분석</a><a href="#lethality">12. 치명도 위험요인</a>
  <a href="#discussion">13. 종합논의</a><a href="#limits">14. 제한점</a><a href="#refs">참고문헌</a>
</nav>
</div></header>
""")

B.append('<div class="wrap">')

# ================================================================ 1. INTRO
B.append("""
<section id="intro">
<div class="sec-num">01</div>
<h2>배경 및 목적</h2>
<p class="lede">최근 3년(2023–2026) JAMA Psychiatry, Molecular Psychiatry, Translational Psychiatry, British Journal of
Psychiatry, Scientific Reports 등에 발표된 자살 관련 연구들을 검토한 결과, 아래 다섯 갈래의 분석 흐름이 반복적으로
확인되었습니다. 본 보고서는 이를 우리 표본에서 재현 가능한지 검토한 뒤, 가능한 범위에서 모두 수행했습니다.</p>
<div class="grid2">
<div class="finding"><div class="h">① 다변량 위험요인 · 예측모형</div><div class="d">임상·인구학적 지표를 결합해 자살시도를
예측하는 노모그램/위험모형 연구 (Frontiers in Psychiatry 2025 노모그램; Molecular Psychiatry 2026 자해 예측모형, n=102,863).</div></div>
<div class="finding"><div class="h">② 증상 네트워크 분석</div><div class="d">우울·불안 증상망에서 자살사고 노드의 중심성과
가교증상(bridge symptom)을 규명하는 연구 (Scientific Reports 2025; General Psychiatry 2024; Frontiers Psychiatry 2025).</div></div>
<div class="finding"><div class="h">③ 아동기 성적 학대와 자살</div><div class="d">아동기 성적 학대(CSA)가 우울·PTSD를 매개로
자살사고·시도 위험을 높인다는 반복 확인 (PMC 2025 리뷰; Frontiers Psychiatry 2025 예측인자 연구).</div></div>
<div class="finding"><div class="h">④ 가족력 · 재발과 자살</div><div class="d">기분장애 가족력과 삽화 재발이 자살위험의
세대간 전달 및 반복시도와 연관된다는 메타분석 (BJP 가족적 전달 모형; 청소년 대상 12M+ 메타분석 2025).</div></div>
</div>
<p>그 외에 자살사고의 잠재구조(수동적/능동적 사고의 요인 분리, <i>Suicide &amp; Life-Threatening Behavior</i> 2023),
Kendler식 생애 스트레스 사건의 맥락적 위협도와 근접 자살위험의 연관성(<i>Psychiatric Quarterly</i> 2023), 부모
양육태도(PBI)·신경증 성향·사회적 지지가 자살사고에 미치는 영향(2023–2025 다수) 문헌을 참고하여 분석 항목을 구성했습니다.
한국인 여성 대상 최신 국내 연구는 경제적 스트레스, 노동시장 차별, 미혼 여성의 심리적 고통 등을 위험요인으로 보고하고
있어(BMC Women's Health 2026; PMC 2024), 결과 해석 시 문화적 맥락으로 함께 고려했습니다.</p>
</section>
""")

# ================================================================ 2. METHODS
B.append("""
<section id="methods">
<div class="sec-num">02</div>
<h2>자료 및 분석 방법</h2>
<div class="grid2">
<div class="card">
<h4 style="margin-top:0">표본 1 · 개인 수준 (n=6,049)</h4>
<p style="font-size:14.5px">한 행 = 한 명. DSM 진단기준, 평생 자살사고/계획/시도 및 치명도, 현재 증상체크리스트(SCL,
16문항, <b>양 군 모두 포함</b>), 아동기 성적학대(CSA), 부모 양육태도(PBI, 부/모 각 16문항), 사회적 관계망, 신경증
성향(EPQ-N 23문항), 생애 스트레스 사건(SLE), 기분장애 가족력, 공존 불안/공황, 임상 아형(멜랑콜리아/비정형/정신병적/
조증 특징) 등 330개 변수.</p>
</div>
<div class="card">
<h4 style="margin-top:0">표본 2 · 삽화 수준 LHC (17,239행 → 8,889행 연결)</h4>
<p style="font-size:14.5px">환자군만 포함, 한 행 = 한 우울 삽화. 삽화 발생 연령·기간, 선행 생애 스트레스 사건의 유형·개수,
Kendler식 독립/의존 사건 구분, 사건 심각도 합성지표(severity_composite) 포함. Study ID로 개인 수준 자료와 연결하여
2,998명(환자군의 99.9%)의 삽화 이력을 자살시도 이력과 결합 분석했습니다.</p>
</div>
</div>
<p style="font-size:14.5px" class="small">통계분석: 이변량 비교(t-검정·카이제곱), 다변량 로지스틱 회귀(표준화 OR, 95%
CI, 5-fold 교차검증 AUC), 탐색적 요인분석(minres 추출·oblimin 회전, KMO/Bartlett 적합도, Cronbach α), 그래프 라쏘
기반 가우스 그래프 모형 네트워크 분석(EBIC 정규화강도 선택, 편상관 기반 강도·매개·근접 중심성), 자체 구현
Kaplan–Meier 생존분석 및 로그순위검정, 삽화-수준 대응표본 Wilcoxon 검정을 사용했습니다. Python
(pandas/statsmodels/factor_analyzer/networkx/scikit-learn)으로 수행했으며, 재현 가능한 전체 스크립트는 저장소의
<code>suicide_analysis/scripts/</code>에 포함되어 있습니다.</p>
</section>
""")

# ================================================================ 3. PREVALENCE
B.append(f"""
<section id="prevalence">
<div class="sec-num">03</div>
<h2>자살 관련 표현형의 유병률</h2>
<p class="lede">재발성 우울장애군에서는 생애면접을 통해 확인한 <b>평생</b> 자살사고·계획·시도를, 건강대조군을 포함한 전체
표본에서는 SCL 문항으로 측정한 <b>최근 1주 이내</b> 자살사고를 각각 확인했습니다.</p>
<div class="grid2">
<figure>{C['c1']}<figcaption><b>그림 1.</b> 재발성 우울장애군(n=3,000)의 평생 자살사고 단계적 이환율. 사고에서 계획,
시도로 이어지는 단계마다 감소하지만 시도까지 진행하는 비율(26.7%)이 상당히 높습니다.</figcaption></figure>
<figure>{C['c2']}<figcaption><b>그림 2.</b> 최근 1주 이내 자살사고(SCL 문항 2점 이상)를 보고한 비율. 건강대조군에서도
11.0%가 경도 이상의 자살사고를 보고해, '건강대조군'이라도 자살사고로부터 완전히 자유롭지 않음을 보여줍니다.</figcaption></figure>
</div>
<div class="callout"><b>주목할 점 —</b> 평생 자살시도 경험자 802명(전체 환자군의 26.7%) 중 <b>65.1%(522명)가 2회 이상
반복 시도</b>를 보고했습니다. 또한 평가 시점 기준으로도 환자군의 27.1%(814/3,002)가 여전히 자살사고 또는 구체적 계획을
가지고 있다고 응답했습니다 (사고만 23.2%, 구체적 계획 동반 3.9%).</div>
<div class="grid2">
<figure>{C['c3']}<figcaption><b>그림 3.</b> 자살시도 방법 (n=801, 무응답 7명 제외). 과량복용이 가장 흔한 방법입니다.</figcaption></figure>
<figure>{C['c4']}<figcaption><b>그림 4.</b> 자살시도로 인한 신체적 손상 정도 (n=796). 43.5%는 경미하거나 손상이 없었으나,
20.1%는 중등도-중증 이상의 손상을 입었습니다.</figcaption></figure>
</div>
</section>
""")

with open(os.path.join(OUT, "_part_a.pkl"), "wb") as f:
    pickle.dump(B, f)
print("Part A done, sections:", len(B))
