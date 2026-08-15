import pickle, os
import pandas as pd

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
with open(os.path.join(OUT, "charts.pkl"), "rb") as f:
    C = pickle.load(f)
with open(os.path.join(OUT, "_part_a.pkl"), "rb") as f:
    B = pickle.load(f)

uni = pd.read_csv(os.path.join(OUT, "univariate_screen_attempt.csv"))
uni_label = {
    "SCLscore_": "현재 우울증상 총점(SCL)", "n_wished_dead": "죽고 싶다는 생각(EPQ-N 문항)",
    "SLEscore_": "생애 스트레스 사건 총점", "dsm_symptom_count": "우울 증상 개수(DSM)",
    "Nscore_": "신경증 성향(Neuroticism)", "age_onset": "발병 연령", "panic_bin": "공황장애 동반",
    "dysthymia_bin": "기분저하증 동반", "gad_bin": "범불안장애 동반", "age": "현재 연령",
    "melancholia_bin": "멜랑콜리아 아형", "manic_bin": "조증 증상 동반", "csa_any": "아동기 성적학대",
    "fh_man_ratio": "조증 가족력 비율", "n_episodes": "우울 삽화 횟수", "fh_dep_ratio": "우울증 가족력 비율",
    "atypical_bin": "비정형 증상 동반", "illness_duration": "이환 기간", "fh_man_any": "조증 가족력 유무",
    "fh_dep_any": "우울증 가족력 유무",
}
rows_html = []
for _, r in uni.iterrows():
    sig = r.p < 0.05
    tag = f'<span class="tag {"tag-risk" if (sig and r.OR>1) else ("tag-protect" if (sig and r.OR<1) else "")}">{r.OR:.2f}</span>' if sig else f"{r.OR:.2f}"
    pstr = f"{r.p:.1e}" if r.p < 0.001 else f"{r.p:.3f}"
    rows_html.append(f"<tr><td>{uni_label.get(r['var'], r['var'])}</td><td>{r.n:.0f}</td><td>{tag}</td>"
                      f"<td>{r.CI_low:.2f}–{r.CI_high:.2f}</td><td>{pstr}</td></tr>")
uni_table = "\n".join(rows_html)

B.append(f"""
<section id="riskfactors">
<div class="sec-num">04</div>
<h2>평생 자살시도의 위험요인</h2>
<p class="lede">문헌에서 반복적으로 보고된 임상·심리사회적 후보 변수 19개를 재발성 우울장애군(n=3,000) 내에서
자살시도 유무에 대한 단변량 로지스틱 회귀로 선별한 뒤, 유의한 변수를 포함한 다변량 모형으로 독립적 위험요인을
확인했습니다 (Frontiers in Psychiatry 2025 노모그램 연구의 접근을 참고).</p>
<h3>4.1 단변량 선별</h3>
<div class="tablewrap"><table>
<thead><tr><th>변수</th><th>n</th><th>OR</th><th>95% CI</th><th>p</th></tr></thead>
<tbody>{uni_table}</tbody>
</table></div>
<p class="small">범주형 변수(카이제곱검정)도 모두 유의했습니다: 혼인상태(p=1.3×10⁻¹⁰), 직업 유무(p=1.8×10⁻⁴), 음주
문제(p=1.2×10⁻³), 교육수준(p=1.3×10⁻³), 직종(p=2.2×10⁻³). 약물 남용은 표본 내 유병률이 낮아(5/3,000) 유의하지
않았습니다.</p>

<h3>4.2 다변량 모형 — 독립적 위험요인</h3>
<figure>{C['c5']}<figcaption><b>그림 5.</b> 평생 자살시도에 대한 다변량 로지스틱 회귀 (표준화 OR, 95% CI, n=2,079).
<span style="color:var(--status-critical)">●빨강</span> = 통계적으로 유의한 위험 증가, <span style="color:var(--status-good)">●초록</span> = 유의한 보호 효과,
회색 = 비유의. 혼인상태 더미변수 3개는 그림에서 생략(본문 참조). 모형의 5-fold 교차검증 AUC = <b>0.694</b>(SD 0.025).</figcaption></figure>
<div class="callout"><b>핵심 소견 —</b> 다른 모든 변수를 통제한 뒤에도 <b>현재 우울증상의 심각도(개수)</b>가 가장 강력한
독립 예측인자였고(OR 1.58/SD), <b>생애 누적 스트레스</b>(OR 1.27), <b>신경증 성향</b>(OR 1.18), <b>공황장애 동반</b>
(OR 1.35), <b>이른 발병 연령</b>(OR 0.86/SD, 즉 발병이 이를수록 위험 증가), <b>삽화 재발 횟수</b>(OR 1.10),
<b>조증 가족력 비율</b>(OR 1.11)이 독립적으로 유의했습니다. 반면 단변량에서 유의했던 <b>아동기 성적학대와 멜랑콜리아
아형은 다변량에서 유의성을 상실</b>했는데, 이는 이 효과의 상당 부분이 증상 심각도·신경증·생애 스트레스를 매개로
작동함을 시사합니다 (아래 9절 위험요인 네트워크 분석에서 재확인).</div>
</section>
""")

# ---- section 5: repeat attempt ----
m2 = pd.read_csv(os.path.join(OUT, "model2_repeat_attempt.csv"), index_col=0)
m2_label = {**uni_label, "marital_Separated": "혼인상태: 별거", "marital_Married": "혼인상태: 기혼",
            "marital_Widowed": "혼인상태: 사별", "marital_NeverMarried": "혼인상태: 미혼",
            "pbi_father_care": "부 돌봄", "pbi_father_overprotect": "부 과보호",
            "pbi_mother_care": "모 돌봄", "pbi_mother_overprotect": "모 과보호",
            "social_support": "사회적 지지", "social_conflict": "사회적 갈등", "edu_level": "교육수준"}
sig2 = m2[(m2.CI_low > 1) | (m2.CI_high < 1)]
rows2 = "\n".join(f"<tr><td>{m2_label.get(i,i)}</td><td>{r.OR:.2f}</td><td>{r.CI_low:.2f}–{r.CI_high:.2f}</td>"
                   f"<td>{r.p:.3f}</td></tr>" for i, r in sig2.iterrows())
B.append(f"""
<section id="repeat">
<div class="sec-num">05</div>
<h2>반복(2회 이상) 자살시도의 위험요인</h2>
<p class="lede">평생 자살시도 경험자(n=802) 중 1회 시도자와 2회 이상(반복) 시도자를 비교했습니다. 반복시도자가
64.9%로 다수를 차지해, 첫 시도 이후의 위험관리가 특히 중요함을 시사합니다. 동일한 예측변수 세트로 다변량 로지스틱
회귀를 적용한 결과(n=456), 유의한 독립 예측인자는 다음과 같았습니다.</p>
<div class="tablewrap"><table>
<thead><tr><th>변수</th><th>OR</th><th>95% CI</th><th>p</th></tr></thead>
<tbody>{rows2}</tbody>
</table></div>
<p style="font-size:14.5px">신경증 성향이 높을수록 반복시도 위험이 뚜렷이 증가했습니다(OR 1.36/SD, p=.008). 첫 시도의
위험요인(증상심각도·생애스트레스·발병연령 등)은 반복 여부는 잘 구분하지 못해, <b>'시도 자체'와 '반복'은 부분적으로
다른 위험기제</b>를 가질 수 있음을 시사합니다 — 이는 최근 문헌에서 자살시도(attempt)와 자살사망(death)의 위험요인이
"중첩되지만 구별된다"는 JAMA Psychiatry의 보고와 궤를 같이합니다.</p>
</section>
""")

# ---- section 6: full-sample current SI ----
m3 = pd.read_csv(os.path.join(OUT, "model3_current_si_fullsample.csv"), index_col=0)
m3_label = {**m2_label, "case": "재발성 우울장애 진단(vs 대조군)"}
def _pfmt(p):
    return f"{p:.1e}" if p < 0.001 else f"{p:.3f}"
rows3 = "\n".join(f"<tr><td>{m3_label.get(i,i)}</td><td>{r.OR:.2f}</td><td>{r.CI_low:.2f}–{r.CI_high:.2f}</td>"
                   f"<td>{_pfmt(r.p)}</td></tr>" for i, r in m3.iterrows())
B.append(f"""
<section id="fullsample">
<div class="sec-num">06</div>
<h2>전체 표본(환자+대조군)에서 현재 자살사고의 예측요인</h2>
<p class="lede">SCL 자살사고 문항은 대조군에도 존재하므로, 진단군 여부를 통제한 상태에서 신경증·부모양육태도·사회적
지지·CSA가 <b>진단과 무관하게</b> 현재 자살사고를 예측하는지 전체 표본(n=5,039)에서 검증했습니다.</p>
<div class="tablewrap"><table>
<thead><tr><th>변수</th><th>OR (per SD)</th><th>95% CI</th><th>p</th></tr></thead>
<tbody>{rows3}</tbody>
</table></div>
<div class="callout"><b>핵심 소견 —</b> 신경증 성향은 진단 여부와 무관하게 현재 자살사고의 가장 강력한 예측인자였습니다
(OR 3.47/SD) — 재발성 우울장애 진단 자체의 효과(OR 1.96)보다도 컸습니다. 사회적 지지(OR 0.89)와 교육수준(OR 0.88)은
보호적으로 작용했습니다. 이는 자살사고가 진단 범주를 넘어서는 <b>차원적(dimensional)</b> 현상이며, 신경증과 같은
성격 특성이 진단명보다 더 강력한 근접 예측인자일 수 있다는 최근 네트워크·차원 정신병리 연구의 주장과 일치합니다.</div>
</section>
""")

with open(os.path.join(OUT, "_part_b.pkl"), "wb") as f:
    pickle.dump(B, f)
print("Part B done, sections:", len(B))
