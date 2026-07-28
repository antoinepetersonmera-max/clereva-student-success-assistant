---
name: support-ranking
description: >
  Rank students by their need for academic support in the CLEREVA project.
  Use this skill when asked to prioritize students for intervention, generate
  a support priority list, score students by risk level, or identify which
  students need the most help based on model predictions and risk factors.
---

# Support Priority Ranking

This skill generates a ranked list of students by their estimated need for
academic support, combining model risk scores with absence and failure flags.

## Relevant Files

- **R script**: `R/rank_support_priority.R`
- **Output page**: `support-ranking.qmd`
- **Requires**: `models/student_risk_model.rds` (produced by the `model` skill)

## What This Skill Does

1. Loads the cleaned dataset and the saved logistic regression model
2. Generates predicted at-risk probabilities for all students
3. Combines risk probability with auxiliary indicators:
   - `failures` (past course failures)
   - `absences` (attendance record)
   - `studytime` (self-reported study hours)
   - `schoolsup` (whether school support is already in place)
4. Computes a composite **Support Priority Score** (weighted combination)
5. Ranks all students from highest to lowest priority
6. Flags the **top 20%** as "High Priority" for counsellor review
7. Saves the ranked table to `data/outputs/tables/support_priority.csv`

## Constraints & Guardrails

- The risk model (`models/student_risk_model.rds`) **must exist** before
  running this skill. Run the `model` skill first.
- Priority scores are **decision-support tools only**, not deterministic
  gatekeepers. Human review is required before any intervention action.
- Do not expose individual student identifiers in rendered reports; use
  anonymised row indices.
- Validate that all input predictors exist and are in range before scoring.

## Example Triggers

- "Who needs support the most?"
- "Rank students by support priority"
- "Generate the intervention priority list"
- "Which students should counsellors contact first?"
