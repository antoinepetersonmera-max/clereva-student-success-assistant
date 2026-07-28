---
name: explore
description: >
  Explore and summarize student performance data from the CLEREVA dataset.
  Use this skill when asked to describe distributions, summarize grade statistics
  (G1, G2, G3), visualize key variables, or provide an initial exploratory data
  analysis (EDA) of the student-mat dataset.
---

# Explore Student Performance

This skill handles exploratory data analysis (EDA) for the CLEREVA Student
Success project. It produces descriptive statistics, distribution plots, and
correlation summaries for the Portuguese secondary school dataset.

## Relevant Files

- **R script**: `R/explore_performance.R`
- **Output page**: `explore.qmd`

## What This Skill Does

1. Loads the cleaned dataset from `data/processed/student_clean.csv`
2. Validates it using `R/validation.R` (`validate_student_dataset()`)
3. Computes summary statistics for numeric variables (G1, G2, G3, absences,
   studytime, failures, etc.)
4. Generates distribution plots (histograms, boxplots) for grade outcomes
5. Produces a correlation heatmap of key numeric predictors
6. Saves figures to `data/outputs/figures/` and tables to `data/outputs/tables/`

## Constraints & Guardrails

- **Observational data only** — all findings are associations, not causal claims.
  Always use language like "associated with" or "correlated with", never
  "causes" or "leads to". `validate_causal_claims()` is called on any narrative
  text before rendering.
- Grades are on a **0–20 Portuguese scale**; interpret accordingly.
- Do not filter out any student records without documenting the reason.

## Example Triggers

- "Explore the student dataset"
- "Show me the grade distribution"
- "What does the EDA look like?"
- "Summarize performance variables"
