"""
prism_softImpute4_smile.csv에 각 방문별 세부 문항 점수 병합
- HAMD1-17, SSI1-19, PHQ1-9, BAI1-21, BHOL1-20 (총 86개 컬럼)
- id 매핑: 'prism_P001' → 'P001'
- case_episode 기준으로 각 방문 파일 할당
"""

import pandas as pd
import numpy as np

# ─────────────────────────────────────────────
# 1. 원본 파일 로드
# ─────────────────────────────────────────────
main = pd.read_csv('/root/.claude/uploads/182da1f2-ce75-4f20-9aa1-09191b685df9/e668796d-prism_softImpute4_smile.csv')
print(f'Main: {main.shape}  |  case_episode: {main["case_episode"].value_counts().to_dict()}')

# ─────────────────────────────────────────────
# 2. 방문별 파일 로드 & 통합
# ─────────────────────────────────────────────
visit_files = {
    'baseline': '/root/.claude/uploads/50ea09dd-8bac-4b71-b23b-3d3b48deebbd/6a96f1af-prism_V_dataset_baseline.csv',
    '2m':       '/root/.claude/uploads/50ea09dd-8bac-4b71-b23b-3d3b48deebbd/49e26a4c-prism_V_dataset_2mo.csv',
    '4m':       '/root/.claude/uploads/50ea09dd-8bac-4b71-b23b-3d3b48deebbd/924f50e7-prism_V_dataset_4mo.csv',
    '8m':       '/root/.claude/uploads/50ea09dd-8bac-4b71-b23b-3d3b48deebbd/b15b87ba-prism_V_dataset_8mo.csv',
    '12m':      '/root/.claude/uploads/50ea09dd-8bac-4b71-b23b-3d3b48deebbd/83146800-prism_V_dataset_12mo.csv',
}

# 추출할 컬럼
TARGET_COLS = (
    [f'HAMD{i}' for i in range(1, 18)] +   # HAMD1-17
    [f'SSI{i}'  for i in range(1, 20)] +   # SSI1-19
    [f'PHQ{i}'  for i in range(1, 10)] +   # PHQ1-9
    [f'BAI{i}'  for i in range(1, 22)] +   # BAI1-21
    [f'BHOL{i}' for i in range(1, 21)]     # BHOL1-20
)
print(f'Target columns: {len(TARGET_COLS)}')

visit_dfs = []
for episode, path in visit_files.items():
    df = pd.read_csv(path, encoding='cp949')

    # id 포맷 통일 (3가지 혼재):
    #   prism_P001 / prism-P022 / Prism_P092 → P001 / P022 / P092
    df['id'] = df['id'].str.replace(r'(?i)prism[-_]', '', regex=True)
    df['case_episode'] = episode

    # 존재하는 target 컬럼만 선택
    available = [c for c in TARGET_COLS if c in df.columns]
    missing   = [c for c in TARGET_COLS if c not in df.columns]
    if missing:
        print(f'  [{episode}] Missing columns: {missing}')

    sub = df[['id', 'case_episode'] + available].copy()
    print(f'  [{episode}] rows={len(sub)}, target_cols={len(available)}')
    visit_dfs.append(sub)

# 세로로 합치기
visit_all = pd.concat(visit_dfs, ignore_index=True)
print(f'\nCombined visit data: {visit_all.shape}')
print(f'Duplicate (id, case_episode): {visit_all.duplicated(["id","case_episode"]).sum()}')

# ─────────────────────────────────────────────
# 3. 원본과 병합
# ─────────────────────────────────────────────
# 기존에 같은 이름 컬럼이 있으면 제거 후 재병합
existing_target = [c for c in TARGET_COLS if c in main.columns]
if existing_target:
    print(f'\nDropping existing columns from main: {existing_target}')
    main = main.drop(columns=existing_target)

merged = main.merge(visit_all, on=['id', 'case_episode'], how='left')
print(f'\nMerged: {merged.shape}')

# ─────────────────────────────────────────────
# 4. 병합 결과 검증
# ─────────────────────────────────────────────
print('\n--- Merge validation ---')
for col in TARGET_COLS[:5]:
    n_filled = merged[col].notna().sum()
    print(f'  {col}: filled={n_filled}/{len(merged)}  ({n_filled/len(merged)*100:.1f}%)')

print()
print('NaN count per case_episode (SSI1 기준):')
print(merged.groupby('case_episode')['SSI1'].apply(lambda x: x.isna().sum()))

# ─────────────────────────────────────────────
# 5. 저장
# ─────────────────────────────────────────────
out_path = '/home/user/PRISM-V/data/prism_softImpute4_smile_with_items.csv'
import os
os.makedirs('/home/user/PRISM-V/data', exist_ok=True)
merged.to_csv(out_path, index=False)
print(f'\n✓ Saved: {out_path}')
print(f'  Shape: {merged.shape}')
print(f'  New columns added: {len(TARGET_COLS)}')
print(f'  Columns: {list(merged.columns[-10:])}')
