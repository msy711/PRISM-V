"""
Data preparation for suicide-related analysis.
Loads the subject-level (combined_3000) and episode-level (LHC) datasets,
builds derived suicidality phenotypes, and saves cleaned versions for
downstream analysis scripts.
"""
import pandas as pd
import numpy as np
import re
import os

RAW_TXT = "/root/.claude/uploads/fa13428d-a096-5077-b94d-4322a2c3017f/484781e8-combined_3000_190625_SCLupdated.txt"
RAW_XLSX = "/root/.claude/uploads/fa13428d-a096-5077-b94d-4322a2c3017f/bd49ad60-komogen_LHC_clean.xlsx"
OUT_DIR = "/home/user/PRISM-V/suicide_analysis/outputs"
os.makedirs(OUT_DIR, exist_ok=True)

df = pd.read_csv(RAW_TXT, sep="\t", low_memory=False)
lhc = pd.read_excel(RAW_XLSX)

print("subject-level:", df.shape, "episode-level:", lhc.shape)

# ---------------------------------------------------------------
# Basic recodes
# ---------------------------------------------------------------
df["group"] = df["case"].map({1: "MDD", 0: "Control"})

# lethality ordinal (dep.suicide.damage) - among those with an attempt
damage_order = {
    "No physical damage or very minor physical damage": 0,
    "Minor physical damage": 1,
    "Moderate physical damage": 2,
    "Moderately severe physical damage": 3,
    "Severe physical damage": 4,
}
df["suicide_damage_ord"] = df["dep.suicide.damage"].map(damage_order)

plannow_order = {
    "No": 0,
    "Suicidal ideation only": 1,
    "Suicidal ideation with specific plan": 2,
}
df["suicide_plannow_ord"] = df["dep.suicide.plan.now"].map(plannow_order)

# Core binary suicidality phenotypes (defined for cases; NaN for controls since
# these lifetime items were only asked of the MDD group)
df["si_lifetime"] = df["dep.suicide.thought"].clip(upper=1)     # 0/1 (2 treated as 1: yes)
df["splan_lifetime"] = df["dep.suicide.plan"].clip(upper=1)
df["sattempt_lifetime"] = df["dep.suicide.attempt"].clip(upper=1)
df["sattempt_multiple"] = (df["dep.suicide.number"] >= 2).astype(float)
df.loc[df["dep.suicide.number"].isna() & (df["sattempt_lifetime"] == 1), "sattempt_multiple"] = np.nan

# Current (past-week/month) suicidal ideation from SCL item - available in BOTH groups
# scl.suicide.thought is Likert 1(not at all)-5(extremely) in this SCL-style instrument
df["si_current_any"] = (df["scl.suicide.thought"] >= 2).astype(float)  # any endorsement above "not at all"
df["si_current_clinical"] = (df["scl.suicide.thought"] >= 3).astype(float)  # moderate or more

# Illness severity / burden proxies
df["dsm_symptom_count"] = df["dep.dsm.criteria"]
df["n_episodes"] = df["dep.number.episodes"]
df["age_onset"] = df["dep.age.onset"]
df["illness_duration"] = df["age"] - df["age_onset"]

# Subtype flags -> binary presence (0/1). melancholia & dysthymia are already 0/1;
# atypical.symptoms/manic/psychotic are symptom counts -> presence = count>=1
subtype_cols = ["atypical.symptoms", "manic", "psychotic", "melancholia", "dysthymia"]
for col in subtype_cols:
    newcol = col.split(".")[0] + "_bin"
    df[newcol] = (df[col] >= 1).astype(float)
    df.loc[df[col].isna(), newcol] = np.nan

print(df[subtype_cols].apply(lambda c: c.value_counts(dropna=False).head(5)))

# Comorbid anxiety
df["gad_bin"] = (df["GAD"] == 1).astype(float)
df["panic_bin"] = (df["panic"] == 1).astype(float)

# Family history of mood disorder (proportion of informative relatives affected)
df["fh_dep_ratio"] = df["fh.dep.ratio"]
df["fh_man_ratio"] = df["fh.man.ratio"]
df["fh_dep_any"] = (df["fh_dep_ratio"] > 0).astype(float)
df["fh_man_any"] = (df["fh_man_ratio"] > 0).astype(float)

# Childhood sexual abuse
df["csa_any"] = df["csa.any"].astype(float)

# Neuroticism
df["Nscore_"] = df["Nscore"]
df["n_wished_dead"] = df["n.wished.dead"]  # EPQ-N item: "have you ever wished you were dead?"

# Stressful life events / hospitalisation / meds
df["SLEscore_"] = df["SLEscore"]
df["psy_inpatient"] = df["psy.inpatient"]  # already coded 0/1 (NaN for controls)
df["SCLscore_"] = df["SCLscore"]

df.to_pickle(os.path.join(OUT_DIR, "subject_level_clean.pkl"))
lhc.to_pickle(os.path.join(OUT_DIR, "episode_level_clean.pkl"))

print("Saved cleaned pickles.")
print("\n--- Suicidality prevalence among MDD cases (n=3000) ---")
cases = df[df.case == 1]
for c in ["si_lifetime", "splan_lifetime", "sattempt_lifetime", "sattempt_multiple"]:
    print(c, cases[c].mean().round(4), cases[c].sum(), "/", cases[c].notna().sum())

print("\n--- current SCL suicidal-thought item, by group ---")
print(df.groupby("group")["scl.suicide.thought"].describe())
print(df.groupby("group")["si_current_any"].mean())
