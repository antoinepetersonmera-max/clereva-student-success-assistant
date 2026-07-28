---
name: scenario-analysis
description: >
  Run what-if scenario simulations on the CLEREVA student risk model.
  Use this skill when asked to simulate how changing a variable (e.g.,
  increasing study time, reducing absences, adding school support) would
  affect a student's predicted risk score or probability of passing.
---

# Scenario Analysis

This skill simulates hypothetical interventions by adjusting input variables
and re-scoring students through the trained risk model, enabling "what-if"
planning for student support strategies.

## Relevant Files

- **R script**: `R/scenario_analysis.R`
- **Output page**: `scenario-analysis.qmd`
- **Requires**: `models/student_risk_model.rds` (produced by the `model` skill)

## What This Skill Does

1. Loads the cleaned dataset and the saved logistic regression model
2. Accepts a **scenario specification**: one or more variables to modify
   and their hypothetical new values (e.g., `studytime = 4`, `schoolsup = "yes"`)
3. Creates a modified copy of the dataset with the scenario applied
4. Re-predicts at-risk probabilities under the scenario
5. Computes the **delta** (change in predicted risk) for each student
6. Summarises: how many students shift from "At Risk" to "Low Risk" under
   the scenario, and by how much on average
7. Produces a comparison table (baseline vs. scenario risk scores)
8. Saves outputs to `data/outputs/tables/` and `data/outputs/figures/`

## Constraints & Guardrails

- Scenarios must only modify **actionable variables** (studytime, schoolsup,
  internet, paid tutoring, absences). Do not allow scenarios that change
  immutable attributes (sex, age, family background).
- Results are **simulated, not guaranteed** — scenarios are model-based
  extrapolations and depend on the model's assumptions.
- Always label outputs clearly as "Simulated Scenario" in figures and tables.
- Validate that scenario variable names and values match the dataset schema
  before running.
- Do not claim causal effects; use `validate_causal_claims()` on all
  generated narrative text.

## Example Triggers

- "What if all at-risk students doubled their study time?"
- "Simulate adding school support for high-absence students"
- "What would happen to risk scores if absences dropped to zero?"
- "Run a scenario where paid tutoring is enabled for failing students"
