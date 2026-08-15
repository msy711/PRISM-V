import pickle, os
import pandas as pd

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
REPORT_PATH = "/home/user/PRISM-V/suicide_analysis/report.html"
with open(os.path.join(OUT, "charts.pkl"), "rb") as f:
    C = pickle.load(f)
with open(os.path.join(OUT, "_part_c.pkl"), "rb") as f:
    B = pickle.load(f)

# ============================================================ 8. Symptom network
su_mdd = pd.read_csv(os.path.join(OUT, "network_suicide_edges_MDD.csv")).head(6)
su_ctrl = pd.read_csv(os.path.join(OUT, "network_suicide_edges_Control.csv")).head(6)
mdd_edges = " · ".join(f"{r.neighbor}({r.partial_cor:.2f})" for _, r in su_mdd.iterrows())
ctrl_edges = " · ".join(f"{r.neighbor}({r.partial_cor:.2f})" for _, r in su_ctrl.iterrows())

B.append(f"""
<section id="network">
<div class="sec-num">08</div>
<h2>증상 네트워크 분석 — 자살사고는 어떤 증상과 직접 연결되는가</h2>
<p class="lede">현재 증상체크리스트(SCL) 16문항은 환자군·대조군 모두에서 측정되어, 두 집단의 증상 네트워크를
그래프 라쏘(EBIC 정규화) 기반 가우스 그래프 모형으로 각각 추정하고 비교했습니다. 최근 고영향력 저널의 네트워크
연구들(Scientific Reports 2025; General Psychiatry 2024)과 동일한 접근입니다.</p>
<div class="grid2">
<figure>{C['c7']}<figcaption><b>그림 7.</b> 재발성 우울장애군 증상 네트워크 (n=2,997). 선의 굵기·명도는 편상관
강도를 나타내며, <span style="color:var(--status-critical)">굵은 빨간 선</span>은 자살사고 노드와 직접 연결된
관계입니다.</figcaption></figure>
<figure>{C['c8']}<figcaption><b>그림 8.</b> 건강대조군 증상 네트워크 (n=3,047). 동일한 척도·동일한 추정법을
적용했습니다.</figcaption></figure>
</div>
<div class="tablewrap"><table>
<thead><tr><th>네트워크</th><th>n</th><th>자살사고 노드 강도</th><th>매개중심성</th><th>근접중심성</th></tr></thead>
<tbody>
<tr><td>전체 표본</td><td>6,044</td><td>0.838</td><td>0.038</td><td>0.074</td></tr>
<tr><td><span class="tag tag-mdd">재발성 우울장애군</span></td><td>2,997</td><td><b>0.866</b></td><td><b>0.048</b></td><td>0.075</td></tr>
<tr><td><span class="tag tag-ctrl">건강대조군</span></td><td>3,047</td><td>0.630</td><td><b>0.000</b></td><td>0.058</td></tr>
</tbody></table></div>
<div class="callout"><b>핵심 소견 —</b> 자살사고 노드는 두 네트워크 모두에서 <i>무가치감·절망감·자기비난·울음·기력저하</i>와
직접 연결되었지만, <b>재발성 우울장애군에서 자살사고 노드가 전반적으로 더 중심적</b>이었습니다(강도 0.866 vs
0.630). 특히 매개중심성이 대조군에서는 0으로, 대조군의 네트워크에서 자살사고는 다른 증상들을 잇는 경로에 놓여있지
않은 <b>주변적(peripheral)</b> 노드인 반면, 환자군에서는 증상들 사이를 잇는 가교 역할을 하는 <b>더 통합된</b> 노드로
나타났습니다. 두 집단 모두에서 '덫에 갇힌 느낌(entrapment)' 문항이 자살사고의 근접 이웃 상위권에 있었는데
(환자군 6위, 대조군 2위), 이는 자살위험의 통합운동모형(Integrated Motivational-Volitional Model)이 강조하는
심리적 갇힘(entrapment)-자살사고 경로와 부합하는 소견입니다.</div>
<p style="font-size:14px" class="small"><b>환자군에서 자살사고와 직접 연결된 상위 증상:</b> {mdd_edges}<br>
<b>대조군에서 자살사고와 직접 연결된 상위 증상:</b> {ctrl_edges}</p>
</section>
""")

# ============================================================ 9. Risk-factor network
B.append(f"""
<section id="rfnetwork">
<div class="sec-num">09</div>
<h2>위험요인 네트워크 — 심리사회적 요인들의 상호연결 구조</h2>
<p class="lede">4절의 회귀모형이 "각 변수를 다른 변수로 통제했을 때의 효과"를 보여준다면, 네트워크 분석은 변수들이
서로 어떻게 얽혀 있는지를 시각화합니다. 환자군(n=2,231)에서 자살행동 심각도(0=사고없음···3=시도)를 포함한 17개
노드로 두 번째 가우스 그래프 모형을 추정했습니다.</p>
<figure>{C['c9']}<figcaption><b>그림 9.</b> 환자군 위험요인 네트워크. <span style="color:var(--status-critical)">빨간 노드/선</span> =
자살행동 심각도 노드 및 그와 직접 연결된 관계.</figcaption></figure>
<div class="tablewrap"><table>
<thead><tr><th>자살행동 심각도의 직접 이웃</th><th>편상관계수</th></tr></thead>
<tbody>
<tr><td>우울증상 개수</td><td class="tag tag-risk">0.271</td></tr>
<tr><td>생애 스트레스 사건 총점</td><td class="tag tag-risk">0.120</td></tr>
<tr><td>정신과 입원력</td><td class="tag tag-risk">0.118</td></tr>
<tr><td>신경증 성향</td><td class="tag tag-risk">0.097</td></tr>
<tr><td>발병 연령</td><td class="tag tag-protect">-0.064</td></tr>
<tr><td>공황장애 동반</td><td>0.046</td></tr>
<tr><td>범불안장애 동반</td><td>0.036</td></tr>
<tr><td>사회적 지지</td><td class="tag tag-protect">-0.029</td></tr>
</tbody></table></div>
<div class="callout"><b>핵심 소견 —</b> <b>아동기 성적학대와 우울증 가족력은 자살행동 심각도 노드와 직접 연결되지
않았습니다</b> — 즉 이 요인들의 영향은 증상 심각도·생애 스트레스·신경증 등을 <b>매개로 간접적으로</b> 전달되는
것으로 보이며, 이는 4절 다변량 회귀에서 이 두 변수가 유의성을 상실한 것과 정확히 일치하는 패턴입니다. 두 가지
서로 다른 통계기법(회귀분석과 네트워크분석)이 동일한 결론에 수렴한다는 점에서 이 매개 구조의 신뢰도를 높여줍니다.
반대로 신경증 성향은 네트워크 전체에서 가장 강도가 높은 노드(0.83, 전체 17개 노드 중 1위)로, 자살행동뿐 아니라
다른 위험요인들과도 광범위하게 연결된 '허브'로 기능했습니다.</div>
</section>
""")

# ============================================================ 10. Survival
B.append(f"""
<section id="survival">
<div class="sec-num">10</div>
<h2>발병에서 첫 자살시도까지의 시간</h2>
<p class="lede">우울증 발병 연령을 기준시점으로, 첫 자살시도까지의 시간을 Kaplan–Meier 방법으로 추정했습니다
(n=2,923, 사건 725건). 시도가 없는 대상자는 평가 시점 연령에서 중도절단 처리했습니다.</p>
<div class="callout"><b>발병 초기가 가장 위험한 시기입니다 —</b> 첫 자살시도의 <b>46.1%는 발병 후 1년 이내</b>,
<b>63.3%는 5년 이내</b>에 발생했습니다 (시도까지의 중앙값 2년). 이는 "질환 초기가 자살위험이 가장 높은 시기"라는
최근 고위험군 코호트 연구들의 반복된 결론과 일치하며, 초발 우울증 환자에 대한 조기·집중적 자살위험 평가의
필요성을 강력히 뒷받침합니다.</div>
<div class="grid3">
<figure>{C['c10']}<figcaption><b>그림 10.</b> 아동기 성적학대 유무에 따른 시도-자유(attempt-free) 생존곡선
(로그순위 p&lt;.001).</figcaption></figure>
<figure>{C['c11']}<figcaption><b>그림 11.</b> 멜랑콜리아 아형 유무 (로그순위 p&lt;.001).</figcaption></figure>
<figure>{C['c12']}<figcaption><b>그림 12.</b> 이른 발병(≤25세) 여부 (로그순위 p&lt;.001).</figcaption></figure>
</div>
<div class="tablewrap"><table>
<thead><tr><th>층화 변수</th><th>집단</th><th>n</th><th>평생 시도율</th><th>로그순위 χ²</th><th>p</th></tr></thead>
<tbody>
<tr><td rowspan="2">아동기 성적학대</td><td>없음</td><td>2,001</td><td>23.2%</td><td rowspan="2">13.35</td><td rowspan="2">&lt;.001</td></tr>
<tr><td>있음</td><td>922</td><td>28.3%</td></tr>
<tr><td rowspan="2">멜랑콜리아 아형</td><td>없음</td><td>408</td><td>16.4%</td><td rowspan="2">19.86</td><td rowspan="2">&lt;.001</td></tr>
<tr><td>있음</td><td>2,515</td><td>26.2%</td></tr>
<tr><td rowspan="2">우울증 가족력</td><td>없음</td><td>1,344</td><td>24.1%</td><td rowspan="2">0.81</td><td rowspan="2">.369</td></tr>
<tr><td>있음</td><td>1,579</td><td>25.4%</td></tr>
<tr><td rowspan="2">발병 연령</td><td>&gt;25세</td><td>1,983</td><td>20.8%</td><td rowspan="2">26.14</td><td rowspan="2">&lt;.001</td></tr>
<tr><td>≤25세</td><td>940</td><td>33.3%</td></tr>
</tbody></table></div>
<p style="font-size:14.5px">아동기 성적학대, 멜랑콜리아 아형, 이른 발병은 모두 <b>첫 시도까지의 시간을 유의하게
단축</b>시켰습니다. 반면 우울증 가족력 자체는 시도 시점을 앞당기지는 않았는데, 이는 가족력이 발병 시점을
앞당기는 경로로 간접 작용할 가능성(조기발병 경로)과 구분됩니다.</p>
</section>
""")

with open(os.path.join(OUT, "_part_d.pkl"), "wb") as f:
    pickle.dump(B, f)
print("sections so far:", len(B))
