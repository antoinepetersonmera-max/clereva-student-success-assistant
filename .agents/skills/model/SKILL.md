---
name: model
description: >
  Build and evaluate a logistic regression risk model for the CLEREVA project.
  Use this skill when asked to predict which students are at academic risk
  (G3 < 10), train a classification model, evaluate model performance, or
  generate a ranked list of predictors from the student-mat dataset.
---

# Model Academic Risk

This skill trains and evaluates a logistic regression model to classify
students as "At Risk" (G3 < 10) or "Low Risk" using the CLEREVA dataset.

## Relevant Files

- **R script**: `R/model_risk.R`
- **Output page**: `model.qmd`
- **Saved model**: `models/student_risk_model.rds`
- **Metrics**: `models/model_metrics.csv`
- **Predictor reference**: `models/predictor_reference.csv`

## What This Skill Does

1. Loads and validates the cleaned dataset (`data/processed/student_clean.csv`)
2. Splits data into training (80%) and test (20%) sets with stratified sampling
3. Trains a logistic regression model predicting `at_risk` (engineered in
   `R/clean_data.R` as `G3 < 10`)
4. Evaluates performance: accuracy, sensitivity, specificity, AUC-ROC,
   confusion matrix
5. Saves the fitted model as `models/student_risk_model.rds`
6. Saves metrics to `models/model_metrics.csv`
7. Produces predictor importance table saved to `models/predictor_reference.csv`

## Constraints & Guardrails

- The target variable `at_risk` must be pre-engineered by `R/clean_data.R`
  before calling this skill.
- **Never use G1 or G2 as predictors** if the goal is early intervention
  (they are mid-year grades and would create data leakage in real deployment).
- All model outputs are **predictive, not prescriptive** — flag this clearly
  in any narrative or report.
- Validate causal language before rendering results (`validate_causal_claims()`).

## Example Triggers

- "Train the risk prediction model"
- "Which students are at risk of failing?"
- "Build a logistic regression for academic risk"
- "Evaluate the model on the test set"
