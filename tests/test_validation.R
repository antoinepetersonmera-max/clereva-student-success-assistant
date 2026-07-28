# Unit Tests for CLEREVA Validation Layer
source("R/validation.R")

test_validation <- function() {
  message("Running validation unit tests...")
  
  # 1. Set up mock dataset
  mock_df <- data.frame(
    school = factor(c("GP", "MS", "GP")),
    sex = factor(c("F", "M", "F")),
    age = c(18, 17, 15),
    address = factor(c("U", "R", "U")),
    famsize = factor(c("GT3", "LE3", "GT3")),
    Pstatus = factor(c("A", "T", "T")),
    Medu = c(4, 1, 1),
    Fedu = c(4, 1, 1),
    Mjob = factor(c("at_home", "other", "other")),
    Fjob = factor(c("teacher", "services", "other")),
    reason = factor(c("course", "course", "other")),
    guardian = factor(c("mother", "father", "mother")),
    traveltime = c(2, 1, 1),
    studytime = c(2, 2, 2),
    failures = c(0, 0, 3),
    schoolsup = factor(c("yes", "no", "yes")),
    famsup = factor(c("no", "yes", "no")),
    paid = factor(c("no", "no", "yes")),
    activities = factor(c("no", "no", "no")),
    nursery = factor(c("yes", "no", "yes")),
    higher = factor(c("yes", "yes", "yes")),
    internet = factor(c("no", "yes", "yes")),
    romantic = factor(c("no", "no", "no")),
    famrel = c(4, 5, 4),
    freetime = c(3, 3, 3),
    goout = c(4, 3, 2),
    Dalc = c(1, 1, 2),
    Walc = c(1, 1, 3),
    health = c(3, 3, 3),
    absences = c(6, 4, 10),
    G1 = c(5, 5, 7),
    G2 = c(6, 5, 8),
    G3 = c(6, 6, 10)
  )
  
  # 2. Test validate_columns
  stopifnot(validate_columns(mock_df, c("school", "sex", "age")))
  err <- tryCatch(validate_columns(mock_df, c("school", "nonexistent")), error = function(e) e)
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("Missing required columns", err$message))
  
  # 3. Test validate_types
  stopifnot(validate_types(mock_df, list(school = "factor", age = "numeric")))
  err <- tryCatch(validate_types(mock_df, list(age = "factor")), error = function(e) e)
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("expected factor or character", err$message))
  
  # 4. Test validate_two_groups
  stopifnot(validate_two_groups(mock_df, "school")) # GP and MS
  mock_df_3_groups <- mock_df
  mock_df_3_groups$group3 <- c("A", "B", "C")
  err <- tryCatch(validate_two_groups(mock_df_3_groups, "group3"), error = function(e) e)
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("must have exactly 2 distinct groups", err$message))
  
  # 5. Test validate_sample_size
  stopifnot(validate_sample_size(mock_df, min_size = 3))
  err <- tryCatch(validate_sample_size(mock_df, min_size = 5), error = function(e) e)
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("Sample size 3 is below the minimum required 5", err$message))
  
  # 6. Test validate_ranges
  stopifnot(validate_ranges(mock_df, "G1", 0, 20))
  err <- tryCatch(validate_ranges(mock_df, "age", 18, 20), error = function(e) e)
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("contains values out of range", err$message))
  
  # 7. Test validate_causal_claims
  warn_triggered <- FALSE
  withCallingHandlers({
    validate_causal_claims("This intervention directly causes a massive grade increase.")
  }, warning = function(w) {
    warn_triggered <<- TRUE
    invokeRestart("muffleWarning")
  })
  stopifnot(warn_triggered)
  
  # Ensure observational claim triggers no warning
  warn_triggered <- FALSE
  withCallingHandlers({
    validate_causal_claims("Study time is positively associated with final grade performance.")
  }, warning = function(w) {
    warn_triggered <<- TRUE
  })
  stopifnot(!warn_triggered)
  
  message("All validation unit tests passed successfully!")
}

test_validation()
