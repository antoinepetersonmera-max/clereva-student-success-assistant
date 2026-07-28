---
name: absence-analysis
description: >
  Analyze the relationship between student absences and academic outcomes in
  the CLEREVA dataset. Use this skill when asked about absenteeism patterns,
  the impact of absences on grades, absence thresholds, or attendance-related
  risk factors.
---

# Absence Analysis

This skill investigates how student absences relate to academic performance
(G3) and at-risk status in the CLEREVA Student Success project.

## Relevant Files

- **R script**: `R/analyze_absences.R`
- **Output page**: `absence-analysis.qmd`

## What This Skill Does

1. Loads and validates the cleaned dataset (`data/processed/student_clean.csv`)
2. Validates that `absences` is within range [0, 100] via `validate_ranges()`
3. Computes absence distribution statistics (median, IQR, outlier flags)
4. Segments students into absence tiers (e.g., 0, 1–5, 6–10, 11+)
5. Compares mean G3 and at-risk rates across absence tiers
6. Fits a simple linear regression of G3 ~ absences for effect estimation
7. Produces scatter plots with regression lines and boxplots by absence tier
8. Saves figures to `data/outputs/figures/` and tables to `data/outputs/tables/`

## Constraints & Guardrails

- Absences are validated as **non-negative integers** (range 0–100).
- Absence data is **observational** — high absenteeism may be a symptom of
  other factors (illness, socioeconomic barriers), not the sole cause of
  poor grades. Frame findings accordingly.
- Flag extreme outliers (absences > 40) and note their potential influence
  on regression estimates.
- Do not claim causal direction; use `validate_causal_claims()` on narrative.

## Example Triggers

- "Analyze student absences"
- "How do absences affect grades?"
- "Which students have the most absences?"
- "Is there a threshold of absences that predicts failure?"
