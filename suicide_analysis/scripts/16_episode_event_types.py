"""
Which specific stressful-life-event TYPES are enriched in the depressive episode
during which a person's first suicide attempt occurred, compared to (a) all other
episodes in the sample and (b) that same person's other episodes (within-subject,
McNemar paired test -- controls for all time-invariant confounds, analogous to a
case-crossover design)?
"""
import pandas as pd
import numpy as np
from scipy import stats
from statsmodels.stats.contingency_tables import mcnemar
import os

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
lhc = pd.read_pickle(os.path.join(OUT, "episode_level_clean.pkl"))
df = pd.read_pickle(os.path.join(OUT, "subject_level_with_factors.pkl"))
cases = df[df.case == 1].copy()

lhc = lhc.merge(cases[["study_id", "sattempt_lifetime", "dep.suicide.first.age"]], on="study_id", how="inner")
lhc["ep_end_age"] = lhc["episode_age"] + lhc["episode_duration_weeks"].fillna(0) / 52.0

attempters = lhc[lhc.sattempt_lifetime == 1].dropna(subset=["dep.suicide.first.age"])
for sid, g in attempters.groupby("study_id"):
    A = g["dep.suicide.first.age"].iloc[0]
    g = g.sort_values("episode_number")
    contains = g[(g.episode_age <= A) & (g.ep_end_age >= A)]
    target_idx = contains.index[0] if len(contains) >= 1 else (g["episode_age"] - A).abs().idxmin()
    lhc.loc[target_idx, "is_attempt_episode"] = 1
lhc["is_attempt_episode"] = lhc["is_attempt_episode"].fillna(0)
print("Attempt-linked episodes:", int(lhc.is_attempt_episode.sum()))

event_cols = ['sle_assault', 'sle_divorce', 'sle_bankruptcy', 'sle_debt', 'sle_injury', 'sle_lose_job',
              'sle_natural_disaster', 'sle_spouse_die', 'sle_family_member_die', 'sle_family_conflict', 'sle_other']
event_label = {
    'sle_assault': '폭행/신체적 공격', 'sle_divorce': '이혼', 'sle_bankruptcy': '파산',
    'sle_debt': '부채', 'sle_injury': '부상/사고', 'sle_lose_job': '실직',
    'sle_natural_disaster': '자연재해', 'sle_spouse_die': '배우자 사망', 'sle_family_member_die': '가족 사망',
    'sle_family_conflict': '가족 갈등', 'sle_other': '기타 사건',
}
cat_cols = ['n_interpersonal', 'n_threat', 'n_financial', 'n_conflict', 'n_other', 'n_independent', 'n_dependent']
cat_label = {
    'n_interpersonal': '대인관계 사건 수', 'n_threat': '위협적 사건 수', 'n_financial': '경제적 사건 수',
    'n_conflict': '갈등 사건 수', 'n_other': '기타 사건 수', 'n_independent': '독립적 사건 수', 'n_dependent': '의존적 사건 수',
}

# ---- (a) population-level prevalence: attempt-linked vs all-other episodes ----
rows = []
for c in event_cols:
    d = lhc.dropna(subset=[c])
    p_att = d.loc[d.is_attempt_episode == 1, c].mean()
    p_oth = d.loc[d.is_attempt_episode == 0, c].mean()
    tab = pd.crosstab(d.is_attempt_episode, d[c])
    chi2, p, _, _ = stats.chi2_contingency(tab)
    rows.append(dict(event=event_label[c], prev_attempt_ep=p_att, prev_other_ep=p_oth, chi2=chi2, p=p))
pop_df = pd.DataFrame(rows).sort_values("p")
pop_df.to_csv(os.path.join(OUT, "lhc_event_type_population.csv"), index=False)
print("\n=== Population-level: event type prevalence, attempt-linked vs other episodes ===")
print(pop_df.to_string(index=False))

# ---- (b) within-subject paired comparison: attempt-episode value vs MEAN rate across
#      that person's other episodes (avoids the "any-of-several" bias of a naive McNemar
#      test, consistent with the severity_composite/sle_count pairing used in 11.1) ----
mc_rows = []
for c in event_cols:
    diffs = []
    for sid, g in lhc.dropna(subset=[c]).groupby("study_id"):
        att = g.loc[g.is_attempt_episode == 1, c]
        oth = g.loc[g.is_attempt_episode == 0, c]
        if len(att) == 1 and len(oth) >= 1:
            diffs.append(att.values[0] - oth.mean())
    diffs = pd.Series(diffs).dropna()
    if len(diffs) > 10 and diffs.abs().sum() > 0:
        stat, p = stats.wilcoxon(diffs)
        mc_rows.append(dict(event=event_label[c], n_pairs=len(diffs), mean_diff=diffs.mean(),
                             pct_higher_in_attempt_ep=(diffs > 0).mean(), wilcoxon_p=p))
mc_df = pd.DataFrame(mc_rows).sort_values("wilcoxon_p")
mc_df.to_csv(os.path.join(OUT, "lhc_event_type_paired.csv"), index=False)
print("\n=== Within-subject paired: attempt-episode event rate minus person's other-episode mean rate ===")
print(mc_df.to_string(index=False))

# ---- category counts, paired mean comparison ----
cat_rows = []
for c in cat_cols:
    diffs = []
    for sid, g in lhc.dropna(subset=[c]).groupby("study_id"):
        att = g.loc[g.is_attempt_episode == 1, c]
        oth = g.loc[g.is_attempt_episode == 0, c]
        if len(att) == 1 and len(oth) >= 1:
            diffs.append(att.values[0] - oth.mean())
    diffs = pd.Series(diffs).dropna()
    if len(diffs) > 10:
        stat, p = stats.wilcoxon(diffs)
        cat_rows.append(dict(category=cat_label[c], n_pairs=len(diffs), mean_diff=diffs.mean(), wilcoxon_p=p))
cat_df = pd.DataFrame(cat_rows).sort_values("wilcoxon_p")
cat_df.to_csv(os.path.join(OUT, "lhc_event_category_paired.csv"), index=False)
print("\n=== Event category counts, paired (attempt episode vs person's other episodes) ===")
print(cat_df.to_string(index=False))

print("\nDone.")
