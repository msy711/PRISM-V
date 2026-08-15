# Suicide risk-factor analysis (KOMOGEN female sample)

Analysis pipeline for the suicide-related integrated analysis of the recurrent
MDD / healthy-control Korean women dataset (subject-level questionnaire data +
episode-level Life History Calendar data). Raw source files are **not**
included in this repository (individual-level clinical research data) — the
scripts expect them at the paths set at the top of `scripts/01_data_prep.py`.

## Pipeline

Run in order from `scripts/`:

1. `01_data_prep.py` – load both raw files, build suicidality phenotypes and derived variables.
2. `02_univariate_screen.py` – univariate risk-factor screen (lifetime suicide attempt).
3. `03_factor_analysis.py` – EFA of parental bonding (PBI) and social-support items.
4. `04_regression_models.py` – multivariable logistic regression models (attempt, repeat attempt, current SI full sample).
5. `05_network_analysis.py` – SCL symptom network (graphical lasso / GGM), MDD vs. control.
6. `06_riskfactor_network.py` – case-only psychosocial risk-factor network.
7. `07_time_to_attempt.py` – Kaplan–Meier time-from-onset-to-first-attempt + log-rank tests.
8. `08_lhc_episode_analysis.py` – episode-level (LHC) analysis: attempt-linked episode severity, kindling.
9. `09_generate_charts.py` (`10_build_report.py` in this repo) → `svg_charts.py` – inline SVG chart builders.
10. `11_assemble_html.py` … `15_finalize.py` – assemble the final HTML report (`../report.html`).

`outputs/` holds the aggregate result tables (CSV/TXT) each script produces —
model coefficients, network centrality/partial-correlation matrices, EFA
loadings, KM survival tables, etc. No individual-level (row-per-subject) data
is stored here; the large intermediate pickles used during development were
removed after the report was built.

The finished report (`report.html`, also published as a Claude artifact) is
an exploratory analysis and is not intended for standalone clinical use.
