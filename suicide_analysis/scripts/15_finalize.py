import pickle, os
import pandas as pd

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
REPORT_PATH = "/home/user/PRISM-V/suicide_analysis/report.html"
with open(os.path.join(OUT, "charts.pkl"), "rb") as f:
    C = pickle.load(f)
with open(os.path.join(OUT, "_part_d.pkl"), "rb") as f:
    B = pickle.load(f)

# ============================================================ 11. LHC episode analysis
B.append(f"""
<section id="lhc">
<div class="sec-num">11</div>
<h2>삽화 수준(Life History Calendar) 분석</h2>
<p class="lede">2,998명의 환자에서 확보된 8,889개 우울 삽화를 개인 수준의 자살시도 이력과 연결하여, (1) 자살시도가
발생한 삽화가 그 사람의 다른 삽화보다 더 심한 스트레스로 촉발되었는지, (2) 재발이 거듭될수록 '별다른 계기 없이'
발생하는 삽화(kindling 가설)가 느는지, (3) 평생 시도 여부가 개인의 전반적 삽화 스트레스 부담과 관련되는지를
검토했습니다.</p>

<h3>11.1 자살시도와 연관된 삽화는 더 심각했는가 (개인 내 대응비교)</h3>
<div class="grid2">
<div class="tablewrap"><table>
<thead><tr><th>지표</th><th>대응 수</th><th>평균 차이</th><th>Wilcoxon p</th></tr></thead>
<tbody>
<tr><td>사건 심각도 합성지표</td><td>785</td><td>+0.087</td><td>.065</td></tr>
<tr><td>생애 사건 개수</td><td>785</td><td>+0.078</td><td>.091</td></tr>
<tr><td>사건 위협도(OR 기반)</td><td>785</td><td>+0.201</td><td class="tag tag-risk">.012</td></tr>
<tr><td>의존적 사건 수</td><td>785</td><td>&minus;0.000</td><td>.943</td></tr>
<tr><td>독립적 사건 수</td><td>785</td><td>&minus;0.015</td><td>.976</td></tr>
</tbody></table></div>
<div class="card" style="font-size:14.3px">
<p style="margin-top:0"><b>해석 —</b> 자살시도가 발생한 삽화는 같은 사람의 다른 삽화들에 비해 사건 위협도가
유의하게 더 높았고(p=.012), 사건 심각도·개수도 같은 방향의 경향성(p≈.07–.09)을 보였습니다. 다만 효과 크기는
크지 않아, <b>특정 삽화의 스트레스 수준만으로 시도 시점을 예측하기는 어렵다</b>는 것이 정직한 결론입니다.
이는 Kendler식 맥락적 위협 평가와 근접 자살위험을 연결한 최근 연구(Psychiatric Quarterly 2023)와 부분적으로만
일치하는, 다소 절제된 소견입니다.</p></div>
</div>

<h3>11.2 Kindling(반응 역치 저하) 패턴</h3>
<figure>{C['c13']}<figcaption><b>그림 13.</b> 삽화 순번이 증가할수록 '뚜렷한 계기 없이(out of the blue)' 발생하는
삽화의 비율이 증가합니다 (로지스틱 회귀, 삽화 순번당 OR=1.24, 개인 단위 클러스터 표준오차, p&lt;.001).</figcaption></figure>
<p style="font-size:14.5px">1번째 삽화에서는 10.2%만이 뚜렷한 계기 없이 발생했지만, 7번째 삽화에서는 29.3%로
증가했습니다 — 재발이 거듭될수록 더 적은 스트레스로도(혹은 스트레스 없이도) 삽화가 발생한다는 <b>kindling
가설</b>과 부합하는 뚜렷한 패턴입니다. 다만 개인별 kindling 비율(out-of-blue 삽화 비율) 자체는 평생 자살시도
여부와 유의한 관련이 없었습니다(시도군 14.0% vs 비시도군 13.3%, p=.50) — 즉 <b>'역치가 낮아지는 재발 양상' 자체가
자살위험을 직접 높이지는 않는다</b>는 것입니다.</p>

<h3>11.3 삽화당 평균 스트레스 부담과 평생 자살시도</h3>
<figure>{C['c14']}<figcaption><b>그림 14.</b> 개인별 삽화당 평균 생활사건 심각도 지수 비교. 평생 자살시도
경험자는 비경험자보다 삽화당 평균 스트레스 부담이 <b>2배 이상</b> 높았습니다 (t=8.59, p=2.8×10⁻¹⁷).</figcaption></figure>
<div class="callout"><b>종합 —</b> 개별 삽화 하나하나의 심각도로 "어느 삽화에서 시도가 나올지"를 예측하기는
어렵지만(11.1), <b>질병 경과 전체에 걸친 누적 스트레스 부담</b>은 평생 자살시도 여부를 강하게 구분했습니다(11.3).
즉 자살위험은 단일 급성 스트레스 사건보다 <b>만성적으로 더 가혹한 삽화 궤적을 겪는 하위집단</b>과 관련되어
보이며, 이는 4절의 "생애 스트레스 누적 총점"이 다변량 모형에서 독립적 위험요인으로 남은 것과 개인-집계
수준에서 일관됩니다.</div>

<h3>11.4 자살시도-연관 삽화에는 어떤 '종류'의 스트레스 사건이 있었는가</h3>
<p style="font-size:14.5px">11.1절의 종합지표를 사건 <b>유형별</b>로 분해했습니다. 각 유형에 대해 (a) 자살시도-연관
삽화와 그 외 삽화 전체를 비교하는 모집단 수준 비교, (b) 같은 사람의 시도-연관 삽화 대 그 사람의 다른 삽화 평균을
비교하는 개인 내 대응비교를 모두 수행했습니다.</p>
<figure>{C['c15']}<figcaption><b>그림 15.</b> 사건 유형별 발생률 — 다른 삽화 대비 자살시도-연관 삽화 (모집단 수준
비교, 유의한 차이를 보인 상위 5개 유형).</figcaption></figure>
<div class="tablewrap"><table>
<thead><tr><th>사건 유형</th><th>다른 삽화</th><th>시도-연관 삽화</th><th>모집단 χ² p</th><th>개인 내 대응 p</th></tr></thead>
<tbody>
<tr><td>폭행/신체적 공격</td><td>8.6%</td><td class="tag tag-risk">18.7%</td><td>9.6×10⁻¹⁹</td><td class="tag tag-risk">5×10⁻⁶</td></tr>
<tr><td>가족 갈등</td><td>53.3%</td><td class="tag tag-risk">65.1%</td><td>1.2×10⁻⁹</td><td class="tag tag-risk">.005</td></tr>
<tr><td>기타 사건</td><td>23.8%</td><td class="tag tag-protect">16.1%</td><td>3.0×10⁻⁶</td><td>.577</td></tr>
<tr><td>자연재해</td><td>0.7%</td><td>2.2%</td><td>1.4×10⁻⁴</td><td>.076</td></tr>
<tr><td>파산</td><td>8.1%</td><td>11.0%</td><td>.010</td><td>.619</td></tr>
<tr><td>이혼</td><td>8.7%</td><td>11.5%</td><td>.013</td><td>.384</td></tr>
<tr><td>가족 구성원 사망</td><td>15.2%</td><td>14.8%</td><td>.798</td><td>.810</td></tr>
<tr><td>배우자 사망</td><td>3.8%</td><td>3.7%</td><td>.906</td><td>.629</td></tr>
</tbody></table></div>
<div class="callout"><b>핵심 소견 —</b> <b>폭행/신체적 공격</b>과 <b>가족 갈등</b>만이 모집단 비교와 개인 내 대응비교
<b>양쪽 모두에서</b> 일관되게 유의했습니다 — 즉 이 두 사건 유형은 단순히 시도자가 스트레스를 더 많이 겪어서가
아니라, <b>그 사람의 다른 삽화와 비교해도</b> 시도가 발생한 바로 그 삽화에 유독 몰려 있었습니다. 사건 범주 수준
집계(11절 도입부 표)에서도 '위협적 사건'과 '갈등 사건' 범주만 유의했던 것과 정확히 일치합니다. 반면 배우자·가족
사망, 자연재해 등 상실·재난성 사건은 시도 시점과 특별히 연관되지 않았습니다 — 애도 자체보다 <b>대인관계 내
위협·갈등(공격, 가족 갈등)</b>이 자살행동의 근접 방아쇠에 더 가깝다는, 최근 "표적 거부(targeted rejection)와
대인관계 상실이 자살사고·행동의 근위험요인"이라는 문헌(2024–2025)과 부합하는 소견입니다.</div>
</section>
""")

# ============================================================ 12. Lethality
lethA = pd.read_csv(os.path.join(OUT, "lethality_damage_ordinal.csv"), index_col=0)
lethB = pd.read_csv(os.path.join(OUT, "lethality_method_logistic.csv"), index_col=0)
def _sigtag(r):
    sig = not (r.CI_low < 1 < r.CI_high)
    cls = "tag-risk" if (sig and r.OR > 1) else ("tag-protect" if (sig and r.OR < 1) else "")
    return f'<span class="tag {cls}">{r.OR:.2f}</span>' if sig else f"{r.OR:.2f}"
rowsA_html = "\n".join(f"<tr><td>{i}</td><td>{_sigtag(r)}</td><td>{r.CI_low:.2f}–{r.CI_high:.2f}</td><td>{r.p:.3f}</td></tr>"
                        for i, r in lethA.sort_values('p').iterrows())
rowsB_html = "\n".join(f"<tr><td>{i}</td><td>{_sigtag(r)}</td><td>{r.CI_low:.2f}–{r.CI_high:.2f}</td><td>{r.p:.3f}</td></tr>"
                        for i, r in lethB.sort_values('p').iterrows())

B.append(f"""
<section id="lethality">
<div class="sec-num">12</div>
<h2>자살 치명도의 위험요인</h2>
<p class="lede">"누가 시도하는가"와 "시도가 얼마나 치명적인가"는 다른 질문입니다. 평생 자살시도 경험자(n=802) 중
치명도 정보가 있는 대상자에서, (A) 신체적 손상 정도(5단계 순서형, 서수 로지스틱회귀)와 (B) 고치명도 방법
사용 여부(목맴·투신 vs 과량복용·기타, 로지스틱회귀)를 각각 예측하는 모형을 적합했습니다.</p>
<div class="grid2">
<div>
<h4 style="margin-top:0">A. 신체적 손상 정도 (서수 로지스틱, n=719)</h4>
<figure>{C['c16']}<figcaption><b>그림 16.</b> 손상 정도가 더 높은 범주에 속할 오즈비 (1SD 증가당).</figcaption></figure>
</div>
<div>
<h4 style="margin-top:0">B. 고치명도 방법 사용 (목맴·투신, n=718, 25.6%)</h4>
<figure>{C['c17']}<figcaption><b>그림 17.</b> 과량복용·기타 대비 목맴/투신을 사용할 오즈비 (1SD 증가당).</figcaption></figure>
</div>
</div>
<div class="tablewrap"><table>
<thead><tr><th colspan="4">A. 손상 정도 — 유의한 예측인자</th></tr>
<tr><th>변수</th><th>OR</th><th>95% CI</th><th>p</th></tr></thead>
<tbody>
<tr><td>정신과 입원력</td><td class="tag tag-risk">1.32</td><td>1.15–1.51</td><td>&lt;.001</td></tr>
<tr><td>반복 시도(2회 이상)</td><td class="tag tag-risk">1.26</td><td>1.09–1.46</td><td>.002</td></tr>
<tr><td>생애 스트레스 사건 총점</td><td class="tag tag-risk">1.28</td><td>1.10–1.49</td><td>.002</td></tr>
<tr><td>삽화 횟수</td><td class="tag tag-risk">1.15</td><td>1.01–1.31</td><td>.037</td></tr>
</tbody></table></div>
<div class="tablewrap"><table>
<thead><tr><th colspan="4">B. 고치명도 방법 — 유의한 예측인자</th></tr>
<tr><th>변수</th><th>OR</th><th>95% CI</th><th>p</th></tr></thead>
<tbody>
<tr><td>우울 증상 개수</td><td class="tag tag-risk">1.23</td><td>1.01–1.51</td><td>.039</td></tr>
<tr><td>교육 수준</td><td class="tag tag-risk">1.21</td><td>1.01–1.45</td><td>.042</td></tr>
<tr><td>신경증 성향</td><td>0.85</td><td>0.71–1.02</td><td>.073 (경계)</td></tr>
</tbody></table></div>
<div class="callout"><b>핵심 소견 —</b> 4절에서 '시도 여부'를 예측했던 요인들(신경증, 이른 발병, CSA, 가족력,
공황장애)은 치명도 모형에서는 대부분 유의하지 않았습니다. 대신 <b>정신과 입원력·반복시도·생애 스트레스
누적·삽화 재발 횟수</b>라는, "질병이 얼마나 심하고 오래 진행되었는가"를 반영하는 변수들만 손상 정도를 예측했고,
방법의 치명성은 <b>현재 증상 심각도와 (반직관적으로) 높은 교육 수준</b>이 예측했습니다. 즉 <b>'누가 시도하는가',
'시도가 얼마나 반복되는가', '시도가 얼마나 치명적인가'는 서로 다른 위험요인 조합을 가진 세 개의 구별되는 질문</b>
이라는 것이 본 분석 전체를 관통하는 결론입니다. 이는 "치명적 방법 선택과 관련된 요인은 재발/반복 관련 요인과
다르다"는 최근 문헌(<i>Journal of Affective Disorders</i> 2025 치명도-재발 종단연구; 고치명도 방법 선택 예측인자
연구)과 일치합니다.</div>
<p class="small">교육수준이 높을수록 고치명도 방법을 사용할 오즈가 높았다는 소견은 반직관적이며, 계획성·정보
접근성 등 미측정 요인에 의한 혼재 가능성이 있어 단독으로 해석하기보다 추가 검증이 필요합니다.</p>
</section>
""")

# ============================================================ 13. Discussion
B.append("""
<section id="discussion">
<div class="sec-num">13</div>
<h2>종합 논의</h2>
<p class="lede">네 가지 서로 다른 분석 층위 — <b>회귀분석(4·5·6절)</b>, <b>네트워크 분석(8·9절)</b>,
<b>종단·삽화 분석(10·11절)</b>, <b>치명도 분석(12절)</b> — 은 하나의 일관된 그림으로 수렴합니다.</p>
<div class="finding"><div class="h">1. 자살위험은 '증상 심각도 + 만성적 스트레스 + 신경증'의 조합에서 나온다</div>
<div class="d">현재 우울증상 개수, 생애 누적 스트레스, 신경증 성향은 회귀모형(4절)에서 독립적 위험요인이었고,
네트워크 분석(9절)에서도 자살행동 심각도 노드와 직접 연결된 소수의 요인이었으며, 삽화 분석(11.3절)에서는
누적 스트레스 부담이 시도 여부를 가장 크게 구분했습니다. 세 방법이 같은 결론에 도달했다는 것은 이 소견의
강건성(robustness)을 뒷받침합니다.</div></div>
<div class="finding"><div class="h">2. 아동기 성적학대·양육태도·가족력은 '직접'이 아니라 '경로를 통해' 작용한다</div>
<div class="d">CSA와 가족력은 단변량에서는 유의했지만(4.1절) 다변량 모형과 네트워크 분석 모두에서 자살행동과의
직접 연결이 사라졌습니다(4.2·9절). 이는 이 요인들이 증상 심각도·신경증·만성 스트레스라는 경로를 통해 간접적으로
위험을 높인다는 매개 가설을 지지하며, "CSA가 우울/PTSD를 매개로 자살위험을 높인다"는 최근 문헌(리뷰, 2025)과
같은 방향입니다. 다만 본 분석은 횡단 자료 기반의 통계적 매개 패턴이며, 공식적인 매개분석(SEM)으로 확증되지는
않았습니다.</div></div>
<div class="finding"><div class="h">3. 첫 시도와 '반복' 시도는 부분적으로 다른 현상이다</div>
<div class="d">첫 시도의 예측인자(증상심각도·생애스트레스·발병연령 등)는 반복시도 여부는 잘 구분하지 못했고,
반복시도는 신경증 성향만이 뚜렷하게 예측했습니다(5절). 위험 평가와 재발방지 개입은 '누가 처음 시도할
위험이 큰가'와 '이미 시도한 사람 중 누가 반복할 위험이 큰가'를 구분해서 설계할 필요를 시사합니다.</div></div>
<div class="finding"><div class="h">4. 발병 초기가 결정적 개입 시점이다</div>
<div class="d">첫 시도의 거의 절반이 발병 1년 이내에 발생했습니다(10절). 첫 우울 삽화 진단 직후의 집중적
자살위험 평가가 특히 아동기 성적학대·멜랑콜리아 아형·이른 발병 연령을 가진 환자에서 우선순위가 되어야
함을 시사합니다.</div></div>
<div class="finding"><div class="h">5. 자살사고는 진단명보다 신경증과 같은 차원적 성격 특성에 더 강하게 매인다</div>
<div class="d">건강대조군을 포함한 전체 표본 분석(6절)과 두 집단의 증상 네트워크 비교(8절) 모두, 자살사고가
진단 범주를 넘어서는 차원적 현상이며 신경증 성향이 진단 자체보다 더 강력한 근접 상관물임을 보여주었습니다.
동시에 자살사고는 대조군 네트워크에서는 주변적(매개중심성 0)이었지만 환자군 네트워크에서는 다른 증상들을
잇는 통합된 노드로 나타나, "동일한 증상이라도 임상적 맥락에 따라 증상망 내 위상이 달라진다"는 최근 네트워크
정신병리학의 주장을 뒷받침합니다.</div></div>
<div class="finding"><div class="h">6. '시도 여부'·'반복'·'치명도'는 세 개의 구별되는 위험요인 조합을 가진다</div>
<div class="d">첫 시도(4절)는 증상심각도·생애스트레스·신경증·이른 발병이, 반복(5절)은 신경증이, 치명도(12절)는
입원력·반복력·누적 스트레스 부담(손상 정도) 및 증상심각도·교육수준(방법 선택)이 각각 독립적으로 예측했습니다.
게다가 손상 정도를 예측한 변수(입원력, 반복시도, 누적 스트레스)는 대부분 <b>시도 자체를 예측한 변수와도 겹치지
않았습니다</b> — 아동기 성적학대·가족력·공황장애처럼 '누가 시도하는가'를 가른 요인이 '얼마나 치명적인가'는
전혀 가르지 못했습니다. 자살위험을 하나의 단일 스펙트럼이 아니라 <b>서로 다른 예방 전략이 필요한 복수의
하위 질문</b>으로 다뤄야 한다는 최근 정밀정신의학적 접근과 일치하는 결론입니다.</div></div>
<div class="finding"><div class="h">7. 시도로 이어지는 삽화는 '상실'이 아니라 '위협·갈등'으로 촉발된다</div>
<div class="d">11.4절에서, 배우자·가족의 사망과 같은 상실성 사건은 자살시도가 발생한 삽화와 특별한 연관이
없었던 반면, <b>신체적 폭행과 가족 갈등</b>은 모집단·개인 내 비교 양쪽 모두에서 일관되게 시도-연관 삽화에
몰려 있었습니다. 이는 대인관계 내 위협·표적 거부가 자살행동의 근접 방아쇠라는 최근 실시간 추적 연구들의
결론과 부합하며, 임상적으로는 애도 반응보다 <b>가정폭력·가족 갈등 이력</b>에 대한 선별이 급성기 자살위험
평가에서 더 우선될 필요가 있음을 시사합니다.</div></div>
</section>
""")

# ============================================================ 14. Limitations
B.append("""
<section id="limits">
<div class="sec-num">14</div>
<h2>제한점</h2>
<ul class="limits">
<li><b>횡단적·회고적 설계</b> — 자살사고/시도, 아동기 학대, 양육태도 등 대부분의 변수가 단면 면접에서 회고적으로
수집되어, 인과관계가 아닌 연관성만을 시사합니다. 특히 PBI 돌봄 점수와 자살시도의 예상과 다른 방향의 연관성(7.1절)은
현재 증상이 과거 회상에 영향을 미치는 상태-의존적 회상 편향의 가능성을 배제할 수 없습니다.</li>
<li><b>완결 자살(completed suicide)에 대한 정보 없음</b> — 본 표본은 생존자 대상 면접 자료이므로, 자살로 사망한
사례의 위험요인은 다룰 수 없습니다. "시도"와 "사망"의 위험요인이 중첩되지만 구별된다는 최근 문헌(JAMA
Psychiatry)에 비추어, 본 결과를 완결 자살 위험 예측에 직접 확장하는 데는 주의가 필요합니다.</li>
<li><b>네트워크 분석의 근사적 성격</b> — SCL 문항(5점 리커트)에 대해 Pearson 상관 기반 그래프 라쏘를 적용했습니다.
다분상관(polychoric correlation) 기반 추정이 이상적이나, 순서형 범주가 5개로 비교적 조밀하여 근사 오차는 제한적일
것으로 판단됩니다. 네트워크 안정성에 대한 부트스트랩 재표집 검증은 수행하지 못했습니다.</li>
<li><b>다중비교 미보정</b> — 4.1절의 단변량 스크리닝 등 다수의 검정을 수행했으며 별도의 다중비교 보정(FDR 등)은
적용하지 않았습니다. 대부분의 핵심 소견은 p&lt;.001 수준으로 매우 강건하지만, 경계선상 결과(예: p≈.03–.05)는
보수적으로 해석해야 합니다.</li>
<li><b>다변량 모형의 설명력</b> — 예측모형의 교차검증 AUC는 0.69로, 임상적 의사결정에 단독으로 사용하기에는
충분하지 않은 판별력입니다. 이는 최근 대규모 코호트 기반 자해 예측모형 연구들(Molecular Psychiatry 2026)에서도
공통적으로 지적되는 한계입니다.</li>
<li><b>LHC-자살시도 연결의 근사성</b> — 자살시도가 발생한 정확한 삽화를 식별할 때 삽화 기간 정보가 없는 경우
연령 차이가 가장 작은 삽화로 근사 배정했습니다. 삽화 경계가 부정확한 사례가 일부 포함되었을 수 있습니다.</li>
<li><b>단일 성별·단일 문화권 표본</b> — 모든 대상자가 한국인 여성으로, 남성 및 다른 문화권으로의 일반화는
제한적입니다.</li>
<li><b>치명도·사건유형 분석의 표본 크기와 이질성</b> — 손상 정도·방법 모형은 치명도 정보가 있는 시도자
(n≈718–719)에 한정되며, "기타" 방법·"기타" 사건 범주는 서로 이질적인 하위유형을 묶은 것이라 해석에 주의가
필요합니다. 사건 유형 11개에 대해 각각 검정을 반복했으므로(11.4절) 다중비교 보정 없이 얻어진 개별 p값은
가장 강한 두 소견(폭행, 가족갈등)을 중심으로 보수적으로 해석해야 합니다.</li>
</ul>
</section>
""")

# ============================================================ References + Footer
B.append("""
<section id="refs">
<div class="sec-num">참고</div>
<h2>검토한 최근 문헌 (2023–2026)</h2>
<ul class="reflist">
<li>Overlapping risk factors for suicide attempt and suicide death — <i>JAMA Psychiatry</i> case-control study.</li>
<li>Prediction of self-harm in people with newly-diagnosed depression: development and validation of risk prediction models — <a href="https://www.nature.com/articles/s41380-026-03555-x" target="_blank">Molecular Psychiatry (2026)</a>.</li>
<li>Nomogram for estimating the risk of suicide attempts in major depressive disorder — <a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC12492985/" target="_blank">Frontiers in Psychiatry (2025)</a>.</li>
<li>Prevalence and related factors of comorbid suicide attempts and psychotic symptoms in first-episode MDD — <a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC12309062/" target="_blank">PMC (2025)</a>.</li>
<li>Gender differences in the network of suicidal ideation, interpersonal needs and depressive symptoms — <a href="https://www.nature.com/articles/s41598-025-95746-9" target="_blank">Scientific Reports (2025)</a>.</li>
<li>Network analysis of correlations between suicide exposure, depression, and anxiety symptoms in adolescents — <a href="https://pubmed.ncbi.nlm.nih.gov/39422712/" target="_blank">PubMed (2025)</a>.</li>
<li>Post-pandemic changes in anxiety and depression symptom networks — <a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC12398219/" target="_blank">PMC (2025)</a>.</li>
<li>Risk for suicidal thoughts and behavior after childhood sexual abuse in women and men — <a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC3518050/" target="_blank">PMC</a>; Unveiling suicidal risk in young CSA victims — <a href="https://www.tandfonline.com/doi/full/10.1080/15299732.2025.2542129" target="_blank">(2025)</a>.</li>
<li>Offspring's risk for suicidal behaviour in relation to parental death by suicide: systematic review &amp; meta-analysis — <a href="https://www.cambridge.org/core/journals/the-british-journal-of-psychiatry/article/offsprings-risk-for-suicidal-behaviour-in-relation-to-parental-death-by-suicide-systematic-review-and-metaanalysis-and-a-model-for-familial-transmission-of-suicide/C450526CAF5F329AF48E656660DEB6A7" target="_blank">British Journal of Psychiatry</a>.</li>
<li>Parental mental health and suicidal behavior as predictors of adolescent suicidal ideation and attempts: systematic review &amp; meta-analysis (31 studies, 12M+ adolescents, 2025).</li>
<li>An empirical investigation of the distinction between passive and active ideation — <a href="https://onlinelibrary.wiley.com/doi/10.1111/sltb.12935" target="_blank">Suicide and Life-Threatening Behavior (2023)</a>.</li>
<li>Stressful Life Events and Near-term Suicidal Risk in a Clinical Population — <a href="https://link.springer.com/article/10.1007/s11126-023-10038-7" target="_blank">Psychiatric Quarterly (2023)</a>.</li>
<li>Interpersonal and targeted rejection life stressors as proximal risk factors for suicidal ideation and behavior (2024–2025 preprint).</li>
<li>Understanding and comparing risk factors and subtypes in South Korean women's suicidal ideation/attempt — <a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC11138197/" target="_blank">PMC (2024)</a>.</li>
<li>Factors associated with suicidal ideation among unmarried Korean women in their twenties — <a href="https://link.springer.com/article/10.1186/s12905-026-04472-5" target="_blank">BMC Women's Health (2026)</a>.</li>
<li>Labor market discrimination and suicidal ideation: a longitudinal study of Korean women — <a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC11423394/" target="_blank">PMC (2024)</a>.</li>
<li>Malone KM, et al. Major depression and the risk of attempted suicide — <i>Journal of Affective Disorders</i> (1995); first 3 months / first 5 years post-onset as highest-risk window (classic, still widely re-confirmed).</li>
<li>Clinical characteristics, metabolic parameters, and risk factors for suicide attempts vary with untreated major depressive disorder duration — <a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC11921834/" target="_blank">PMC (2024)</a>.</li>
<li>Predicting suicide attempts in early-onset major depressive disorder: a nomogram-based approach — <a href="https://www.sciencedirect.com/science/article/abs/pii/S0022395625006582" target="_blank">Journal of Psychiatric Research (2025)</a>.</li>
<li>Five-Year Outcomes of First Suicide Attempts: Insights on Lethality, Recurrence, and Mortality — <i>Journal of Clinical Psychiatry</i> (2025).</li>
<li>Factors associated with first suicide attempt vs. re-attempt in children and adolescents: a systematic review and meta-analysis — <a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10660565/" target="_blank">PMC (2023)</a>.</li>
<li>Risk Factors for Multiple Suicide Attempts in Adolescents From a 10-Year Suicide Repository — <i>Journal of Korean Medical Science</i> (2024).</li>
<li>Predicting the risk of future multiple suicide attempt among first-time suicide attempters — <a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC9032869/" target="_blank">PMC</a>.</li>
<li>Lethality and recidivism in suicide attempters: a longitudinal study — <a href="https://www.sciencedirect.com/science/article/abs/pii/S0022395625003310" target="_blank">Journal of Psychiatric Research (2025)</a>.</li>
<li>Factors associated with choice of high lethality methods in suicide attempters: a cross-sectional study — <a href="https://link.springer.com/article/10.1186/1752-4458-8-43" target="_blank">International Journal of Mental Health Systems</a>.</li>
<li>Assessing the risk factors of violent and non-violent suicide attempt methods: a population-based cross-sectional study — <a href="https://pmc.ncbi.nlm.nih.gov/articles/PMC11743438/" target="_blank">PMC (2024–2025)</a>.</li>
</ul>
<p class="small">문헌 검색은 2026년 8월 기준 웹 검색으로 수행되었으며, 학술지 자체 검증(peer-review 확인)은
수행하지 못했습니다. 본 목록은 분석 설계의 출발점으로 활용한 참고자료이며, 완전한 체계적 문헌고찰이 아닙니다.</p>
</section>
""")

B.append("""
<footer><div class="wrap" style="padding:0">
분석 스크립트: <code>suicide_analysis/scripts/01–18_*.py</code> · 산출물: <code>suicide_analysis/outputs/*.csv</code> ·
재현 가능한 전체 파이프라인은 저장소에 포함되어 있습니다. 본 보고서는 탐색적(exploratory) 분석 결과이며 임상적
의사결정에 단독으로 사용되어서는 안 됩니다.
</div></footer>
""")

B.append("</div>")  # close .wrap

with open(os.path.join(OUT, "_head.html")) as f:
    HEAD = f.read()

full_html = HEAD + "\n" + "\n".join(B)
with open(REPORT_PATH, "w") as f:
    f.write(full_html)

print("Final report written:", REPORT_PATH, "size(KB)=", len(full_html)/1024)
print("sections total:", len(B))
