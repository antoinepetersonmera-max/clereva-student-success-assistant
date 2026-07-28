# Pipeline Orchestrator — CLEREVA Student Performance
#
# Sources all analysis scripts in the correct dependency order.
# Run this file to execute the full analysis pipeline from raw data to outputs.
#
# Execution order:
#   1. import_data       — read raw CSV
#   2. clean_data        — feature engineering + save processed CSV
#   3. explore_performance — EDA + figures
#   4. compare_study_groups — group comparison (school, sex, address)
#   5. model_risk        — train + evaluate logistic regression model
#   6. analyze_absences  — absence–grade relationship
#   7. rank_support_priority — student priority ranking
#   8. scenario_analysis — what-if simulation examples

cat("==========================================================\n")
cat(" CLEREVA Student Success Assistant — Full Pipeline\n")
cat("==========================================================\n")
cat(format(Sys.time(), " Started: %Y-%m-%d %H:%M:%S\n\n"))

run_step <- function(step_name, expr) {
  cat(sprintf("[START] %s ...\n", step_name))
  start_time <- proc.time()
  status <- "OK"
  value  <- NULL

  value <- tryCatch(
    withCallingHandlers(
      force(expr),
      warning = function(w) {
        message("  Warning: ", conditionMessage(w))
        status <<- "OK (with warning)"
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      status <<- paste("ERROR:", conditionMessage(e))
      NULL
    }
  )

  elapsed <- round((proc.time() - start_time)["elapsed"], 2)
  cat(sprintf("[ %s ] %s (%.1fs)\n\n", status, step_name, elapsed))
  invisible(value)   # return the actual result, not the status string
}

# ---------------------------------------------------------------------------
# 1. Import raw data
# ---------------------------------------------------------------------------
source("R/import_data.R")
raw_df <- run_step("import_data", import_data())

# ---------------------------------------------------------------------------
# 2. Clean & engineer features
# ---------------------------------------------------------------------------
source("R/clean_data.R")
clean_df <- run_step("clean_data", clean_data(raw_df))
if (!is.data.frame(clean_df)) {
  stop("clean_data did not return a data frame. Pipeline aborted.")
}

# ---------------------------------------------------------------------------
# 3. Exploratory Data Analysis
# ---------------------------------------------------------------------------
source("R/explore_performance.R")
run_step("explore_performance", explore_performance())

# ---------------------------------------------------------------------------
# 4. Group comparisons (school, sex, address)
# ---------------------------------------------------------------------------
source("R/compare_study_groups.R")
run_step("compare_study_groups (school)",  compare_study_groups(group_col = "school"))
run_step("compare_study_groups (sex)",     compare_study_groups(group_col = "sex"))
run_step("compare_study_groups (address)", compare_study_groups(group_col = "address"))

# ---------------------------------------------------------------------------
# 5. Risk model
# ---------------------------------------------------------------------------
source("R/model_risk.R")
run_step("model_risk", model_risk())

# ---------------------------------------------------------------------------
# 6. Absence analysis
# ---------------------------------------------------------------------------
source("R/analyze_absences.R")
run_step("analyze_absences", analyze_absences())

# ---------------------------------------------------------------------------
# 7. Support priority ranking
# ---------------------------------------------------------------------------
source("R/rank_support_priority.R")
run_step("rank_support_priority", rank_support_priority())

# ---------------------------------------------------------------------------
# 8. Scenario analysis examples
# ---------------------------------------------------------------------------
source("R/scenario_analysis.R")
run_step("scenario: studytime + schoolsup",
  scenario_analysis(
    scenario = list(studytime = 4, schoolsup = "yes"),
    label    = "increased_study_and_support"
  )
)
run_step("scenario: absences = 0",
  scenario_analysis(
    scenario = list(absences = 0),
    label    = "zero_absences"
  )
)

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("==========================================================\n")
cat(format(Sys.time(), " Completed: %Y-%m-%d %H:%M:%S\n"))
cat(" All outputs saved to: data/outputs/ and models/\n")
cat("==========================================================\n")
