# CLAUDE.md — PRISM-V Codebase Guide

## Project Overview

PRISM-V is a **machine learning research pipeline for analyzing suicidal speech patterns** in psychiatric patients. The pipeline:

1. Extracts acoustic and linguistic features from patient audio recordings (WAV files)
2. Trains ML classifiers to distinguish suicidal vs. non-suicidal subjects based on speech
3. Evaluates models with bootstrap resampling and confidence intervals
4. Performs statistical analysis in R for publication-grade results

**Target variable:** Binary label — suicidal (SSI ≥ 15) vs. non-suicidal

---

## Repository Structure

```
PRISM-V/
├── classify.py                         # Core ML pipeline (classify, evaluate, bootstrap)
├── train_models.py                     # Train models (cross-sectional & longitudinal)
├── train_models_male_female.py         # Train models stratified by sex
├── rank_features.py                    # Iterative feature ranking experiments
├── feature_importances.py              # Extract feature importances from saved models
├── extract_K_3_voice_wo_glottal.py     # Batch acoustic feature extraction from WAV files
├── PRISM-V_baseline_statistics.R       # Demographic/clinical baseline analysis
├── PRISM-V_acoustic_statistics.R       # Acoustic feature statistical analysis
├── PRISM-V_linguistic_statistics.R     # Linguistic feature statistical analysis
├── README.md                           # Minimal usage instructions
│
├── features/                           # (Required, not in repo) CSV feature files
│   ├── cross_train.csv
│   ├── cross_test.csv
│   ├── longi_internal.csv
│   └── longi_external.csv
│
├── models/                             # (Required, not in repo) Saved joblib model files
│
└── data/                               # (Required, not in repo) Raw WAV audio recordings
```

**Note:** The `features/`, `models/`, and `data/` directories contain data files that are not tracked in git (patient data, large binaries).

---

## Missing Module: `collect_features.py`

Several scripts import from a `collect_features` module that is **not present in the repository**. This module defines feature column name lists used throughout the pipeline:

```python
from collect_features import (
    original_features,        # Baseline/demographic feature column names
    demographic_features,     # Demographic feature column names (subset)
    acoustic_features,        # All acoustic feature column names
    acoustic_features_cross,  # Acoustic features for cross-sectional data
    acoustic_features_longi,  # Acoustic features for longitudinal data
    linguistic_features,      # Linguistic/sentiment feature column names
)
```

**If creating this file**, it should export Python lists of strings matching column names in the CSV feature files.

---

## Tech Stack

### Python (ML Pipeline)
- **scikit-learn 1.3.1** — classifiers, feature selection, metrics
- **xgboost 2.0.3** — XGBoostClassifier
- **lightgbm** — LGBMClassifier
- **imbalanced-learn** — SMOTE oversampling
- **pandas / numpy** — data processing
- **scipy.stats** — confidence interval calculations (t-distribution)
- **librosa** — acoustic feature extraction from audio
- **joblib** — model serialization/deserialization
- **tqdm** — progress bars

### R (Statistical Analysis)
- **dplyr** — data manipulation
- **readr** — CSV reading
- **e1071** — statistical functions (kurtosis, skewness)
- Base R: t-tests, Mann-Whitney U, chi-square, Fisher's exact, Benjamini-Hochberg correction

---

## Installation

```bash
pip install scikit-learn==1.3.1
pip install xgboost==2.0.3
pip install tqdm
pip install lightgbm
pip install imbalanced-learn
pip install librosa pandas numpy scipy joblib
```

---

## Core Module: `classify.py`

This is the central module imported by all training scripts. **Do not run it directly for training** (the `__main__` block is a standalone feature-selection prototype, not the main training workflow).

### Key Functions

#### `get_clf(algorithm, svc_predict_proba=False)`
Factory function returning a scikit-learn compatible classifier.

| Algorithm | Classifier |
|-----------|------------|
| `"gbc"` | GradientBoostingClassifier (random_state=42) |
| `"xgb"` | XGBClassifier (objective="reg:squarederror", random_state=42) |
| `"lgb"` | LGBMClassifier (random_state=42) |
| `"svc"` | SVC (kernel="linear", C=1, random_state=42) |
| `"nb"` | GaussianNB |
| `"ensemble_soft"` | VotingClassifier (gbc + xgb, soft voting) |
| `"ensemble_stack"` | StackingClassifier (gbc + xgb, LogisticRegression meta) |
| `"ensemble_votereg"` | VotingRegressor (gbc + xgb) |

**Known issue:** `svm`, `GaussianNB`, `VotingRegressor`, and `LogisticRegression` are used in `get_clf()` but their imports are missing from `classify.py`. The script will fail at runtime for `svc`, `nb`, `ensemble_votereg`, and `ensemble_stack` algorithms unless imports are added.

#### `run_experiment(run_id, split, algorithm, df, do_smote, model_save_path)`
Main entry point for a single experiment run.

- **`split="train"`**: Bootstrap training — fits 1000 models on resampled subsets, evaluates on out-of-bag samples, then fits a final model on all data and saves it with joblib.
- **`split="test"`** (any non-"train"): Loads a saved model from `model_save_path`, runs 1000 bootstrap evaluations on the test data.
- **`do_smote=True`**: Applies SMOTE oversampling before training (for class imbalance).
- Returns a dict with: AUC, accuracy, F1, precision, sensitivity, specificity — each with mean, and 90/95/99% confidence intervals.

**Constant:** `TEST_ITERATIONS = 1000` — number of bootstrap iterations.
**Global seed:** `random.seed(78)` at module level.

#### `evaluate(evaluations)`
Takes a list of `(estimator, X_test, y_test)` tuples, runs predictions, and aggregates metrics. Returns 12 lists (aucs, accuracies, confusion matrices, specificities, reports, f1s, precisions, sensitivities, specificities, fprs, tprs, cuts).

**Note:** `specificities` appears twice in the return tuple (positions 3 and 8) — this is a bug in the original code.

#### `confidence_interval(array, confidence)`
Computes t-distribution confidence interval using `scipy.stats.t.interval`. Valid confidence values: `0.90`, `0.95`, `0.99`.

---

## Training Workflows

### Standard Model Training (`train_models.py`)

```bash
python train_models.py -r 20240627.json -p 20240625_cutoff_runs.json
```

- Requires `collect_features.py` for feature column definitions
- Requires previous results JSON (`-p`) to select optimal feature subsets
- Trains on 4 dataset splits: `cross_train`, `cross_test`, `longi_train`, `longi_test`
- Tests feature combinations: `demographic1`, `demographic2`, `demographic+speech`, `speech`, `acoustic`, `linguistic`, `compact`
- Algorithms: `gbc`, `xgb`; SMOTE: off
- Model naming: `models/rf_{experiment_type}_{feature_name}_{algorithm}_smote{bool}`
- Saves results JSON with all metrics and confidence intervals

### Gender-Stratified Training (`train_models_male_female.py`)

```bash
python train_models_male_female.py -r 20240724_male_female.json -p 20240625_cutoff_runs.json
```

- Same as above but splits data by sex (0=male, 1=female column)
- Uses demographic features (age, suicide_hx) as baseline
- Loads previous cutoff results to select optimal feature counts

### Feature Ranking (`rank_features.py`)

```bash
python feature_importances.py -r 20240625_feature_importances.json
python rank_features.py -r 20240625_cutoff_runs.json -p 20240625_feature_importances.json
```

Step 1: Extract feature importances from previously trained models and save to JSON.
Step 2: Iteratively train models using top-1 through top-100 features (by importance rank) to find the optimal feature count cutoff.

---

## Acoustic Feature Extraction (`extract_K_3_voice_wo_glottal.py`)

```bash
python extract_K_3_voice_wo_glottal.py
```

- Processes all WAV files in `data/` directory
- **Expected filename format:** `prism_P001 12m_20.wav` → subject_id=`P001 12m`, case=`20`
- Extracts **62 features per file** (averaged per subject-case if multiple files):
  - 6 spectral features: duration, spectral centroid, bandwidth, rolloff, RMSE, tempo
  - 6 formant features: formants and bandwidths via LPC
  - 8 pitch/magnitude features: pitch mean/error/change, magnitude mean/error/change, zero-crossing rate, delay
  - 40 MFCC coefficients
- Output: Excel file with one row per subject-case

---

## R Statistical Analysis Scripts

All three R scripts follow the same structure:
1. Load feature CSV and clinical data
2. Merge/join datasets on subject ID
3. Split into suicidal vs. non-suicidal groups
4. Run statistical tests:
   - **Continuous variables:** t-test (normal) or Mann-Whitney U (non-normal)
   - **Categorical variables:** chi-square or Fisher's exact test
5. Apply **Benjamini-Hochberg multiple testing correction**
6. Print results table with means, SDs, test statistics, p-values, adjusted p-values

### `PRISM-V_baseline_statistics.R`
Compares demographic and clinical characteristics between groups. Variables: age, BMI, education, income, medication dose, HAMD, SSI, PHQ, BAI, BHOL, BIS, sex, diagnosis, marital status, mental health history, suicide history.

### `PRISM-V_acoustic_statistics.R` (821 lines)
Tests all 60 acoustic features (including all 40 MFCCs). Also runs linear regression: `feature ~ suicidal + age + sex + AP_dose`.

### `PRISM-V_linguistic_statistics.R`
Tests linguistic/sentiment features. Regression: `feature ~ suicidal + age + sex + education_years`.

---

## Result File Format

All training scripts output JSON files with this structure:

```json
{
  "experiment_id": {
    "fprs": [[...], ...],
    "tprs": [[...], ...],
    "cuts": [[...], ...],
    "confusion_matrices": [[...], ...],
    "classification_reports": [{...}, ...],
    "used_features": ["feature1", "feature2", ...],
    "aucs": [0.71, 0.68, ...],
    "auc_mean": 0.695,
    "auc_c90": [0.68, 0.71],
    "auc_c95": [0.67, 0.72],
    "auc_c99": [0.65, 0.74],
    "accuracies": [...],
    "accuracy_mean": 0.70,
    ...
    "important_features": ["top_feat", ...]
  }
}
```

Each metric has `_mean` and `_c90`/`_c95`/`_c99` confidence interval variants.

---

## Data Pipeline

```
Raw WAV files (data/)
        │
        ▼
extract_K_3_voice_wo_glottal.py
        │
        ▼
Excel feature file → (manual processing) → features/cross_train.csv
                                            features/cross_test.csv
                                            features/longi_internal.csv
                                            features/longi_external.csv
        │
        ▼
feature_importances.py (initial run) → {date}_feature_importances.json
        │
        ▼
rank_features.py → {date}_cutoff_runs.json (optimal feature counts per experiment)
        │
        ▼
train_models.py → {date}_results.json + models/ directory
        │
        ▼
R scripts (statistical validation)
```

---

## Experiment ID Naming Convention

Experiment IDs are constructed as:
```
{experiment_type}_{split}_{feature_set}_{n_features}_{algorithm}_smote{True|False}
```

Examples:
- `cross_train_acoustic_20_gbc_smoteFalse`
- `longi_test_linguistic_21_xgb_smoteTrue`

Model filenames:
```
models/rf_{experiment_type}_{feature_name}_{algorithm}_smote{bool}
```

Result JSON filenames follow `{YYYYMMDD}_{description}.json` convention.

---

## Key Constants and Seeds

- `TEST_ITERATIONS = 1000` — bootstrap iterations in `classify.py`
- `random.seed(78)` — global Python random seed (set in classify.py, train_models.py, train_models_male_female.py)
- `random_state=42` — sklearn/xgb classifier seed
- `SMOTE(random_state=78)` — SMOTE seed

---

## Known Issues

1. **Missing `collect_features.py`** — required by `train_models.py`, `train_models_male_female.py`, `rank_features.py`, `feature_importances.py`. Must be created or obtained.

2. **Missing imports in `classify.py`** — `svm`, `GaussianNB`, `VotingRegressor`, `LogisticRegression` are used but not imported. Only `gbc`, `xgb`, `lgb`, and `ensemble_soft` algorithms work without fixing imports.

3. **Duplicate return value in `evaluate()`** — `specificities` is returned at both index 3 and index 8 of the 12-tuple; this appears intentional in usage but is a code smell.

4. **Incomplete `classify.py` `__main__` block** — the standalone script block references `sex`, `acoustic_features_cross`, `acoustic_features_longi`, `original_features`, `linguistic_features` which are not imported or defined in that context.

5. **No `.gitignore`** — the repository has no `.gitignore`, risking accidental commits of large model/data files or patient audio.

6. **Hardcoded directory paths** — `features/`, `models/`, `data/` are assumed to exist relative to the working directory with no creation or validation logic.

---

## Development Conventions

- **Language:** Python 3, R
- **Style:** No enforced linter/formatter; follow PEP 8 informally
- **Naming:** Snake_case for Python variables and functions
- **Constants:** UPPER_CASE (e.g., `TEST_ITERATIONS`)
- **No tests:** No test suite exists; validation is through result JSON metrics
- **No CI/CD:** No automated pipelines; scripts run manually
- **Seeds:** Always set random seeds for reproducibility (`random.seed(78)`, `random_state=42`)
- **Results:** Always save to JSON with the date-prefixed naming convention
- **Models:** Save with joblib to the `models/` directory using the standard naming pattern

---

## Running the Pipeline (End-to-End)

```bash
# Step 1: Extract acoustic features from WAV files
python extract_K_3_voice_wo_glottal.py
# → outputs Excel file; process manually into features/*.csv

# Step 2: Extract feature importances from an initial training run
python feature_importances.py -r 20240625_feature_importances.json

# Step 3: Find optimal feature cutoffs
python rank_features.py -r 20240625_cutoff_runs.json -p 20240625_feature_importances.json

# Step 4: Train final prediction models
python train_models.py -r 20240627.json -p 20240625_cutoff_runs.json

# Step 5: Train gender-stratified models
python train_models_male_female.py -r 20240724_male_female.json -p 20240625_cutoff_runs.json

# Step 6: Run R statistical analyses (in RStudio or Rscript)
Rscript PRISM-V_baseline_statistics.R
Rscript PRISM-V_acoustic_statistics.R
Rscript PRISM-V_linguistic_statistics.R
```
