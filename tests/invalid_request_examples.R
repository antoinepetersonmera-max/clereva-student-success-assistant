# Invalid Request Examples — CLEREVA Student Performance
#
# Demonstrates boundary conditions, guardrail triggers, and invalid inputs
# that the validation and skill layers are designed to catch.
# Each block is labelled with the expected behaviour.

source("R/validation.R")

cat("=== CLEREVA Invalid Request Examples ===\n\n")

# ---------------------------------------------------------------------------
# 1. Dataset too small (< 30 rows)
# ---------------------------------------------------------------------------
cat("--- [1] Dataset below minimum sample size ---\n")
tiny_df <- read.csv("data/processed/student_clean.csv", stringsAsFactors = TRUE)[1:15, ]
err1 <- tryCatch(validate_sample_size(tiny_df, min_size = 30), error = function(e) e)
stopifnot(inherits(err1, "error"))
cat(sprintf("  Expected error caught: '%s'\n\n", err1$message))

# ---------------------------------------------------------------------------
# 2. Missing required column
# ---------------------------------------------------------------------------
cat("--- [2] Missing required column ---\n")
bad_cols_df <- read.csv("data/processed/student_clean.csv", stringsAsFactors = TRUE)
bad_cols_df$G3 <- NULL   # drop G3
err2 <- tryCatch(validate_columns(bad_cols_df, c("G1", "G2", "G3")), error = function(e) e)
stopifnot(inherits(err2, "error"))
cat(sprintf("  Expected error caught: '%s'\n\n", err2$message))

# ---------------------------------------------------------------------------
# 3. Out-of-range grade value
# ---------------------------------------------------------------------------
cat("--- [3] Out-of-range grade (G3 = 25) ---\n")
bad_range_df <- read.csv("data/processed/student_clean.csv", stringsAsFactors = TRUE)
bad_range_df$G3[1] <- 25   # 25 is outside [0, 20]
err3 <- tryCatch(validate_ranges(bad_range_df, "G3", 0, 20), error = function(e) e)
stopifnot(inherits(err3, "error"))
cat(sprintf("  Expected error caught: '%s'\n\n", err3$message))

# ---------------------------------------------------------------------------
# 4. Wrong type for expected factor column
# ---------------------------------------------------------------------------
cat("--- [4] Wrong type — school should be factor, not numeric ---\n")
bad_type_df <- read.csv("data/processed/student_clean.csv", stringsAsFactors = TRUE)
bad_type_df$age <- as.factor(bad_type_df$age)   # age should be numeric
err4 <- tryCatch(validate_types(bad_type_df, list(age = "numeric")), error = function(e) e)
stopifnot(inherits(err4, "error"))
cat(sprintf("  Expected error caught: '%s'\n\n", err4$message))

# ---------------------------------------------------------------------------
# 5. Grouping variable with more than 2 levels
# ---------------------------------------------------------------------------
cat("--- [5] Three-level grouping variable ---\n")
df <- read.csv("data/processed/student_clean.csv", stringsAsFactors = TRUE)
df$three_groups <- sample(c("A", "B", "C"), nrow(df), replace = TRUE)
err5 <- tryCatch(validate_two_groups(df, "three_groups"), error = function(e) e)
stopifnot(inherits(err5, "error"))
cat(sprintf("  Expected error caught: '%s'\n\n", err5$message))

# ---------------------------------------------------------------------------
# 6. Causal language in a claim string
# ---------------------------------------------------------------------------
cat("--- [6] Causal language in narrative text ---\n")
causal_text <- "Higher study time directly causes students to pass their exams."
warn_triggered <- FALSE
withCallingHandlers(
  validate_causal_claims(causal_text),
  warning = function(w) {
    warn_triggered <<- TRUE
    invokeRestart("muffleWarning")
  }
)
stopifnot(warn_triggered)
cat("  Expected warning triggered for causal language.\n\n")

# Observational phrasing — no warning expected
ok_text <- "Higher study time is associated with improved final grades."
warn_triggered2 <- FALSE
withCallingHandlers(
  validate_causal_claims(ok_text),
  warning = function(w) { warn_triggered2 <<- TRUE }
)
stopifnot(!warn_triggered2)
cat("  Observational phrasing correctly passed without warning.\n\n")

# ---------------------------------------------------------------------------
# 7. Scenario with immutable variable
# ---------------------------------------------------------------------------
cat("--- [7] Scenario modifying immutable attribute (sex) ---\n")
if (file.exists("models/student_risk_model.rds")) {
  source("R/scenario_analysis.R")
  err7 <- tryCatch(
    scenario_analysis(scenario = list(sex = "M"), label = "invalid"),
    error = function(e) e
  )
  stopifnot(inherits(err7, "error"))
  stopifnot(grepl("immutable", err7$message, ignore.case = TRUE))
  cat(sprintf("  Expected error caught: '%s'\n\n", err7$message))
} else {
  cat("  [SKIP] Model not found — run R/model_risk.R first\n\n")
}

# ---------------------------------------------------------------------------
# 8. Empty scenario list
# ---------------------------------------------------------------------------
cat("--- [8] Empty scenario list ---\n")
if (file.exists("models/student_risk_model.rds")) {
  source("R/scenario_analysis.R")
  err8 <- tryCatch(
    scenario_analysis(scenario = list(), label = "empty"),
    error = function(e) e
  )
  stopifnot(inherits(err8, "error"))
  cat(sprintf("  Expected error caught: '%s'\n\n", err8$message))
} else {
  cat("  [SKIP] Model not found — run R/model_risk.R first\n\n")
}

# ---------------------------------------------------------------------------
# 9. Rank support priority without model
# ---------------------------------------------------------------------------
cat("--- [9] rank_support_priority with missing model file ---\n")
source("R/rank_support_priority.R")
err9 <- tryCatch(
  rank_support_priority(model_path = "models/does_not_exist.rds"),
  error = function(e) e
)
stopifnot(inherits(err9, "error"))
cat(sprintf("  Expected error caught: '%s'\n\n", err9$message))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("=== All invalid request examples completed — all guardrails held. ===\n")
