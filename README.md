# CLEREVA Student Success Assistant

A reproducible R / Quarto project that analyses student academic performance
data (UCI *Student Performance* dataset, Mathematics cohort) to identify
at-risk students, compare groups, and simulate support interventions.

---

## Project Architecture

```
clereva-student-success-assistant/
├── README.md
├── .gitignore
├── _quarto.yml                      # Quarto multi-page website config
├── renv.lock                        # Reproducible R package snapshot
│
├── .agents/                         # AI agent customisation (auto-discovered)
│   ├── rules/                       # Project-scoped agent rules (AGENTS.md)
│   └── skills/                      # Agent skills — loaded automatically
│       ├── explore/SKILL.md
│       ├── compare/SKILL.md
│       ├── model/SKILL.md
│       ├── absence-analysis/SKILL.md
│       ├── support-ranking/SKILL.md
│       └── scenario-analysis/SKILL.md
│
├── data/
│   ├── raw/
│   │   └── student-mat.csv          # Original semicolon-delimited CSV (read-only)
│   ├── processed/
│   │   └── student_clean.csv        # Cleaned + feature-engineered dataset
│   └── outputs/
│       ├── tables/                  # CSV / RDS summary tables
│       └── figures/                 # PNG / SVG plots
│
├── models/
│   ├── student_risk_model.rds       # Fitted logistic regression model
│   ├── model_metrics.csv            # Accuracy, AUC, sensitivity, specificity
│   └── predictor_reference.csv      # Predictor coefficients & odds ratios
│
├── R/                               # Source scripts (sourced by Quarto pages)
│   ├── import_data.R
│   ├── clean_data.R
│   ├── validation.R
│   ├── explore_performance.R
│   ├── compare_study_groups.R
│   ├── model_risk.R
│   ├── analyze_absences.R
│   ├── rank_support_priority.R
│   ├── scenario_analysis.R
│   └── run_all.R                    # Orchestrator — runs full pipeline
│
├── index.qmd
├── industry-problem.qmd
├── dataset.qmd
├── explore.qmd
├── compare.qmd
├── model.qmd
├── absence-analysis.qmd
├── support-ranking.qmd
├── scenario-analysis.qmd
├── validation-limitations.qmd
│
├── final_report.qmd                 # Standalone PDF report
├── final_report.pdf
│
├── tests/
│   ├── test_validation.R            # Unit tests for validation layer
│   ├── test_skills.R                # Integration tests for skill scripts
│   └── invalid_request_examples.R   # Edge-case / boundary examples
│
└── docs/                            # Rendered Quarto website (git-ignored)
```

---

## Quickstart

```r
# 1. Restore package environment
renv::restore()

# 2. Run the full analysis pipeline
source("R/run_all.R")

# 3. Render the Quarto website
quarto::quarto_render()
```

---

## Six Statistical Skills

| Skill | Question Answered | R Script | Quarto Page |
|---|---|---|---|
| **Explore** | What does student performance look like? | `R/explore_performance.R` | `explore.qmd` |
| **Compare** | Do two student groups differ meaningfully? | `R/compare_study_groups.R` | `compare.qmd` |
| **Model** | Which factors are associated with academic risk? | `R/model_risk.R` | `model.qmd` |
| **Absence Analysis** | How are absences related to final grades? | `R/analyze_absences.R` | `absence-analysis.qmd` |
| **Support Ranking** | Which student profiles should be reviewed first? | `R/rank_support_priority.R` | `support-ranking.qmd` |
| **Scenario Analysis** | How does modelled risk change under alternatives? | `R/scenario_analysis.R` | `scenario-analysis.qmd` |

Agent skill definitions live in `.agents/skills/` and are auto-discovered.

---

## Validation Layer

All analysis scripts run through a shared validation layer (`R/validation.R`)
that enforces:

- ✅ Column presence & type checking
- ✅ Sample size thresholds (min. 30 for CLT)
- ✅ Variable range bounds (grades 0–20, age 15–22, absences 0–100)
- ✅ Two-group structure for comparisons
- ✅ Missingness detection
- ✅ Causal language prevention (observational data guardrail)

---

## Dataset

**Source**: UCI Machine Learning Repository — *Student Performance Data Set*  
**File**: `data/raw/student-mat.csv` (Mathematics cohort, N = 395)  
**Key variables**: G1, G2, G3 (period grades, 0–20 scale), absences, studytime,
failures, and 26 sociodemographic predictors.  
**Target**: `at_risk` = `G3 < 10` (engineered in `R/clean_data.R`)

> ⚠️ All findings are **associational**, not causal. This is observational data.

---

## Responsible-Use Controls

- **Privacy**: No personal identifiers stored or displayed.
- **Human Oversight**: Model outputs are decision-support indicators only.
- **Statistical Honesty**: Uncertainty intervals reported; limitations documented.
- **Model Governance**: Baseline comparison and fairness checks included.
