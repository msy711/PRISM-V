import pandas as pd
import numpy as np
import pickle, os, sys

sys.path.insert(0, os.path.dirname(__file__))
from svg_charts import grouped_bar_chart, forest_plot

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
with open(os.path.join(OUT, "charts.pkl"), "rb") as f:
    C = pickle.load(f)

# ---- c15: event-type prevalence, attempt-episode vs other episodes ----
pop = pd.read_csv(os.path.join(OUT, "lhc_event_type_population.csv")).set_index("event")
top_events = ["폭행/신체적 공격", "가족 갈등", "기타 사건", "파산", "이혼"]
c15 = grouped_bar_chart(
    top_events,
    [("다른 삽화", [pop.loc[e, "prev_other_ep"]*100 for e in top_events]),
     ("자살시도-연관 삽화", [pop.loc[e, "prev_attempt_ep"]*100 for e in top_events])],
    colors=("--series-1", "--status-critical"), h=320, margin=(30, 20, 70, 60),
)

# ---- c16: forest plot, lethality damage-severity ordinal model ----
lethA = pd.read_csv(os.path.join(OUT, "lethality_damage_ordinal.csv"), index_col=0)
rowsA = []
for idx, r in lethA.iterrows():
    sig = not (r.CI_low < 1 < r.CI_high)
    direction = "risk" if (sig and r.OR > 1) else ("protect" if (sig and r.OR < 1) else "ns")
    rowsA.append(dict(label=idx, or_=r.OR, lo=r.CI_low, hi=r.CI_high, sig=sig, direction=direction))
rowsA = sorted(rowsA, key=lambda x: -x['or_'])
c16 = forest_plot(rowsA, h=len(rowsA)*26+90, xmax=2.0, margin=(30, 40, 40, 190))

# ---- c17: forest plot, high-lethality method logistic model ----
lethB = pd.read_csv(os.path.join(OUT, "lethality_method_logistic.csv"), index_col=0)
rowsB = []
for idx, r in lethB.iterrows():
    sig = not (r.CI_low < 1 < r.CI_high)
    direction = "risk" if (sig and r.OR > 1) else ("protect" if (sig and r.OR < 1) else "ns")
    rowsB.append(dict(label=idx, or_=r.OR, lo=r.CI_low, hi=r.CI_high, sig=sig, direction=direction))
rowsB = sorted(rowsB, key=lambda x: -x['or_'])
c17 = forest_plot(rowsB, h=len(rowsB)*26+90, xmax=2.0, margin=(30, 40, 40, 190))

C['c15'] = c15
C['c16'] = c16
C['c17'] = c17
with open(os.path.join(OUT, "charts.pkl"), "wb") as f:
    pickle.dump(C, f)
print("Added charts c15, c16, c17. Total charts:", list(C.keys()))
