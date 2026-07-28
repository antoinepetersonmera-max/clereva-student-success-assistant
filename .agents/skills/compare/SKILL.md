---
name: compare
description: >
  Compare student performance between two groups in the CLEREVA dataset.
  Use this skill when asked to compare grades, study habits, or outcomes
  between subgroups such as school (GP vs MS), sex (M vs F), address type
  (urban vs rural), or any other binary categorical variable in the dataset.
---

# Compare Study Groups

This skill runs two-group statistical comparisons for the CLEREVA Student
Success project, using Wilcoxon rank-sum tests and visual side-by-side summaries.

## Relevant Files

- **R script**: `R/compare_study_groups.R`
- **Output page**: `compare.qmd`

## What This Skill Does

1. Loads the cleaned dataset from `data/processed/student_clean.csv`
2. Validates dataset structure and confirms exactly 2 groups via
   `validate_two_groups()` and `validate_sample_size()`
3. Runs non-parametric Wilcoxon rank-sum tests on G1, G2, G3
4. Produces side-by-side boxplots and density plots per group
5. Computes effect sizes (rank-biserial correlation)
6. Saves outputs to `data/outputs/figures/` and `data/outputs/tables/`

## Constraints & Guardrails

- Only accepts **binary group variables** (exactly 2 levels). Use
  `validate_two_groups()` before any comparison.
- Minimum **10 observations per group** enforced via `validate_sample_size()`.
- Results are **descriptive/inferential only** — do not claim causation.
- Report p-values with appropriate corrections (Bonferroni or FDR) when
  running multiple comparisons.

## Example Triggers

- "Compare GP vs MS students"
- "Are there grade differences between male and female students?"
- "How do urban vs rural students perform?"
- "Run a group comparison on G3"
