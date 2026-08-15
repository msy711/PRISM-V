import pickle, os
import pandas as pd

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
with open(os.path.join(OUT, "charts.pkl"), "rb") as f:
    C = pickle.load(f)
with open(os.path.join(OUT, "_part_b.pkl"), "rb") as f:
    B = pickle.load(f)

# ---------------------------------------------------------- 7. Factor analysis
loadF = pd.read_csv(os.path.join(OUT, "efa_loadings_pbi_father.csv"), index_col=0)
loadM = pd.read_csv(os.path.join(OUT, "efa_loadings_pbi_mother.csv"), index_col=0)
loadS = pd.read_csv(os.path.join(OUT, "efa_loadings_social_support.csv"), index_col=0)
rel = pd.read_csv(os.path.join(OUT, "efa_reliability.csv"))
rel_d = rel.set_index("factor")["alpha"].to_dict()

item_kor = {
 'pbi.father.warm':'따뜻했다','pbi.father.better.upset':'속상할 때 위로해줬다','pbi.father.understand':'문제를 이해했다',
 'pbi.father.talk.over':'대화를 즐겼다','pbi.father.smile':'자주 미소지었다','pbi.father.cold':'냉정했다(역채점)',
 'pbi.father.not.talk':'대화가 적었다(역채점)','pbi.father.make.dependent':'의존적으로 만들려 했다',
 'pbi.father.protective':'과보호적이었다','pbi.father.control':'모든 것을 통제하려 했다','pbi.father.baby':'아기 취급했다',
 'pbi.mother.warm':'따뜻했다','pbi.mother.better.upset':'속상할 때 위로해줬다','pbi.mother.understand':'문제를 이해했다',
 'pbi.mother.talk.over':'대화를 즐겼다','pbi.mother.smile':'자주 미소지었다','pbi.mother.cold':'냉정했다(역채점)',
 'pbi.mother.not.talk':'대화가 적었다(역채점)','pbi.mother.make.dependent':'의존적으로 만들려 했다',
 'pbi.mother.protective':'과보호적이었다','pbi.mother.control':'모든 것을 통제하려 했다','pbi.mother.baby':'아기 취급했다',
 'sl.fr.criticize.frq':'친구가 비판함','sl.fr.arguments.frq':'친구와 다툼','sl.fr.xs.demand.frq':'친구의 과도한 요구',
 'sl.fr.care.frq':'친구가 관심 가져줌','sl.fr.interest.frq':'친구가 흥미 보여줌',
 'sl.rel.criticize.frq':'친척이 비판함','sl.rel.arguments.frq':'친척과 다툼','sl.rel.xs.demand.frq':'친척의 과도한 요구',
 'sl.rel.care.frq':'친척이 관심 가져줌','sl.rel.interest.frq':'친척이 흥미 보여줌',
}

def top_items(loadings, col, n=5):
    s = loadings[col].sort_values(key=abs, ascending=False).head(n)
    return ", ".join(f"{item_kor.get(idx, idx)}({v:.2f})" for idx, v in s.items())

B.append(f"""
<section id="factor">
<div class="sec-num">07</div>
<h2>요인분석 — 부모 양육태도(PBI) &amp; 사회적 지지</h2>
<p class="lede">부/모 양육태도 각 16문항과 친구·친척 관계망 10문항에 대해 탐색적 요인분석(minres, oblimin 회전)을
적용했습니다. 표본 적합도는 모두 우수했습니다 (부 PBI: KMO=0.910; 모 PBI: KMO=0.912; 사회적 지지: KMO=0.691; 모든
Bartlett 구형성 검정 p&lt;.001).</p>
<div class="grid3">
<div class="card"><h4 style="margin-top:0">부(父) PBI — 2요인</h4>
<p style="font-size:13.6px"><b>돌봄(Care)</b> α={rel_d['pbi_father_care']:.2f} — {top_items(loadF,'F1')}<br><br>
<b>과보호(Overprotection)</b> α={rel_d['pbi_father_overprotect']:.2f} — {top_items(loadF,'F2')}</p></div>
<div class="card"><h4 style="margin-top:0">모(母) PBI — 2요인</h4>
<p style="font-size:13.6px"><b>돌봄(Care)</b> α={rel_d['pbi_mother_care']:.2f} — {top_items(loadM,'F1')}<br><br>
<b>과보호(Overprotection)</b> α={rel_d['pbi_mother_overprotect']:.2f} — {top_items(loadM,'F2')}</p></div>
<div class="card"><h4 style="margin-top:0">사회적 관계망 — 2요인</h4>
<p style="font-size:13.6px"><b>갈등(Conflict)</b> α={rel_d['social_conflict']:.2f} — {top_items(loadS,'F1')}<br><br>
<b>지지(Support)</b> α={rel_d['social_support']:.2f} — {top_items(loadS,'F2')}</p></div>
</div>
<p style="font-size:14.5px">고전적 PBI의 돌봄/과보호 2요인 구조가 본 한국인 여성 표본에서도 명확히 재현되었습니다
(설명분산 부 46.8%, 모 43.4%). 이는 서구에서 개발된 PBI의 요인구조가 한국 문화권에서도 안정적임을 시사합니다.</p>
<h3>7.1 요인점수와 평생 자살시도의 연관성 (비보정, 표준화 OR)</h3>
<figure>{C['c6']}<figcaption><b>그림 6.</b> 각 요인 1SD 증가당 평생 자살시도 오즈비 (환자군, 비보정).</figcaption></figure>
<div class="callout"><b>예상과 다른 방향의 소견 —</b> 부/모 <b>돌봄(Care)</b> 점수가 <i>높을수록</i> 오히려 평생 자살시도
오즈가 소폭 높았습니다(부 OR 1.14, 모 OR 1.26; 둘 다 p&lt;.01). 이는 "낮은 돌봄이 자살위험을 높인다"는 일반적 가설과
반대 방향입니다. 반면 과보호는 예상대로 보호적 방향(OR&lt;1)이었습니다. 4절의 다변량 모형에서는 돌봄·과보호 효과가
모두 유의성을 상실했으므로, 이 비보정 연관성은 혼재변수(예: 증상 심각도에 따른 회상 편향, 혼인상태)에 의한
가능성이 높아 <b>단독으로 인과적 해석을 하지 않도록 주의</b>가 필요합니다.</div>
</section>
""")

with open(os.path.join(OUT, "_part_c.pkl"), "wb") as f:
    pickle.dump(B, f)
print("Part C (section 7) done, sections:", len(B))
