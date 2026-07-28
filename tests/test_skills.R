# Integration Tests for CLEREVA Statistical Skills
#
# Tests that each skill script runs end-to-end on the real cleaned dataset
# and produces its expected output files.
#
# Prerequisites:
#   - data/raw/student-mat.csv must exist
#   - Run R/run_all.R first to populate data/processed/ and models/

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
assert_file_exists <- function(path, msg = NULL) {
  msg <- msg %||% paste("Expected output file not found:", path)
  if (!file.exists(path)) stop(msg)
  if (file.info(path)$size == 0L) stop(paste("Output file is empty:", path))
  invisible(TRUE)
}

assert_no_error <- function(expr, label) {
  result <- tryCatch(expr, error = function(e) e)
  if (inherits(result, "error")) {
    stop(sprintf("[FAIL] %s — %s", label, result$message))
  }
  message(sprintf("[PASS] %s", label))
  invisible(result)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

PASS <- 0L
FAIL <- 0L

run_test <- function(label, expr) {
  tryCatch({
    force(expr)
    message(sprintf("  [PASS] %s", label))
    PASS <<- PASS + 1L
  }, error = function(e) {
    message(sprintf("  [FAIL] %s\n         -> %s", label, e$message))
    FAIL <<- FAIL + 1L
  })
}

# ---------------------------------------------------------------------------
# Setup: ensure pipeline outputs exist
# ---------------------------------------------------------------------------
cat("\n=== CLEREVA Skill Integration Tests ===\n\n")

# Run import + clean if processed data doesn't exist yet
if (!file.exists("data/processed/student_clean.csv")) {
  message("Processed data not found — running import + clean pipeline first...")
  source("R/import_data.R")
  source("R/clean_data.R")
  raw_df   <- import_data()
  clean_df <- clean_data(raw_df)
}

# ---------------------------------------------------------------------------
# Test 1: explore_performance
# ---------------------------------------------------------------------------
cat("--- explore_performance ---\n")
source("R/explore_performance.R")

run_test("explore_performance runs without error", {
  explore_performance()
})
run_test("explore_summary.csv created and non-empty", {
  assert_file_exists("data/outputs/tables/explore_summary.csv")
})
run_test("explore_categorical.csv created", {
  assert_file_exists("data/outputs/tables/explore_categorical.csv")
})
run_test("grade_distribution.png created", {
  assert_file_exists("data/outputs/figures/grade_distribution.png")
})
run_test("correlation_heatmap.png created", {
  assert_file_exists("data/outputs/figures/correlation_heatmap.png")
})

# ---------------------------------------------------------------------------
# Test 2: compare_study_groups
# ---------------------------------------------------------------------------
cat("\n--- compare_study_groups ---\n")
source("R/compare_study_groups.R")

run_test("compare_study_groups (school) runs without error", {
  compare_study_groups(group_col = "school")
})
run_test("compare_results.csv created and has 3 rows (G1, G2, G3)", {
  assert_file_exists("data/outputs/tables/compare_results.csv")
  res <- read.csv("data/outputs/tables/compare_results.csv")
  if (nrow(res) != 3L) stop(paste("Expected 3 rows, got", nrow(res)))
})
run_test("compare_study_groups (sex) runs without error", {
  compare_study_groups(group_col = "sex")
})
run_test("compare_study_groups rejects non-existent column", {
  err <- tryCatch(compare_study_groups(group_col = "nonexistent_col"), error = function(e) e)
  if (!inherits(err, "error")) stop("Expected error for non-existent column")
})

# ---------------------------------------------------------------------------
# Test 3: model_risk
# ---------------------------------------------------------------------------
cat("\n--- model_risk ---\n")
source("R/model_risk.R")

run_test("model_risk runs without error", {
  model_risk()
})
run_test("student_risk_model.rds saved", {
  assert_file_exists("models/student_risk_model.rds")
})
run_test("model_metrics.csv has expected metrics", {
  assert_file_exists("models/model_metrics.csv")
  m <- read.csv("models/model_metrics.csv")
  expected <- c("Accuracy", "Sensitivity", "Specificity", "AUC_ROC")
  missing  <- setdiff(expected, m$metric)
  if (length(missing) > 0) stop(paste("Missing metrics:", paste(missing, collapse = ", ")))
})
run_test("predictor_reference.csv saved with odds ratios", {
  assert_file_exists("models/predictor_reference.csv")
  pr <- read.csv("models/predictor_reference.csv")
  if (!"odds_ratio" %in% colnames(pr)) stop("odds_ratio column missing")
})
run_test("model AUC > 0.5 (better than random)", {
  m   <- read.csv("models/model_metrics.csv")
  auc <- m$value[m$metric == "AUC_ROC"]
  if (is.na(auc)) {
    message("  [SKIP] pROC not installed — AUC not available")
  } else if (auc <= 0.5) {
    stop(paste("AUC =", auc, "is not better than random"))
  }
})

# ---------------------------------------------------------------------------
# Test 4: analyze_absences
# ---------------------------------------------------------------------------
cat("\n--- analyze_absences ---\n")
source("R/analyze_absences.R")

run_test("analyze_absences runs without error", {
  analyze_absences()
})
run_test("absence_summary.csv created", {
  assert_file_exists("data/outputs/tables/absence_summary.csv")
})
run_test("absence_tier_summary.csv has 4 tiers", {
  assert_file_exists("data/outputs/tables/absence_tier_summary.csv")
  t <- read.csv("data/outputs/tables/absence_tier_summary.csv")
  if (nrow(t) != 4L) stop(paste("Expected 4 tiers, got", nrow(t)))
})

# ---------------------------------------------------------------------------
# Test 5: rank_support_priority
# ---------------------------------------------------------------------------
cat("\n--- rank_support_priority ---\n")
source("R/rank_support_priority.R")

run_test("rank_support_priority runs without error", {
  rank_support_priority()
})
run_test("support_priority.csv created with rank column", {
  assert_file_exists("data/outputs/tables/support_priority.csv")
  p <- read.csv("data/outputs/tables/support_priority.csv")
  if (!"rank" %in% colnames(p)) stop("rank column missing")
  if (nrow(p) != 394L) stop(paste("Expected 394 rows, got", nrow(p)))
})
run_test("Top 20% are flagged High Priority", {
  p   <- read.csv("data/outputs/tables/support_priority.csv")
  pct <- mean(p$priority_band == "High Priority")
  if (abs(pct - 0.20) > 0.05) stop(paste("High Priority fraction =", round(pct, 3)))
})

# ---------------------------------------------------------------------------
# Test 6: scenario_analysis
# ---------------------------------------------------------------------------
cat("\n--- scenario_analysis ---\n")
source("R/scenario_analysis.R")

run_test("scenario_analysis with valid scenario runs without error", {
  scenario_analysis(scenario = list(studytime = 4), label = "test_studytime_4")
})
run_test("scenario results file created", {
  assert_file_exists("data/outputs/tables/scenario_results_test_studytime_4.csv")
})
run_test("scenario_analysis rejects immutable variable", {
  err <- tryCatch(
    scenario_analysis(scenario = list(sex = "M"), label = "test_immutable"),
    error = function(e) e
  )
  if (!inherits(err, "error")) stop("Expected error for immutable variable")
  if (!grepl("immutable", err$message, ignore.case = TRUE)) {
    stop("Error message does not mention 'immutable'")
  }
})

# ---------------------------------------------------------------------------
# Final report
# ---------------------------------------------------------------------------
cat(sprintf("\n=== Test Summary: %d passed, %d failed ===\n", PASS, FAIL))
if (FAIL > 0L) {
  stop(sprintf("%d test(s) failed. See messages above.", FAIL))
} else {
  message("All integration tests passed successfully!")
}
