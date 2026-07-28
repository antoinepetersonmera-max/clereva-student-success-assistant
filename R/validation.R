# Validation Layer for CLEREVA Student Success Assistant

#' Validate Dataset Columns
#'
#' Checks if all expected columns are present in the dataset.
#' @export
validate_columns <- function(df, expected_cols) {
  missing_cols <- setdiff(expected_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Validation Failed: Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  return(TRUE)
}

#' Validate Column Types
#'
#' Checks if variables match expected statistical/R types (e.g. numeric, factor/character).
#' @export
validate_types <- function(df, col_types) {
  for (col in names(col_types)) {
    if (!col %in% colnames(df)) next
    expected_type <- col_types[[col]]
    actual_class <- class(df[[col]])
    
    if (expected_type == "numeric" && !is.numeric(df[[col]])) {
      stop(sprintf("Validation Failed: Column '%s' is of class %s, but expected numeric.", 
                   col, paste(actual_class, collapse = "/")))
    }
    if (expected_type == "factor" && !is.factor(df[[col]]) && !is.character(df[[col]])) {
      stop(sprintf("Validation Failed: Column '%s' is of class %s, but expected factor or character.", 
                   col, paste(actual_class, collapse = "/")))
    }
  }
  return(TRUE)
}

#' Validate Two-Group Structure for Comparison
#'
#' Checks if a categorical variable has exactly two non-empty levels/values.
#' @export
validate_two_groups <- function(df, group_col) {
  if (!group_col %in% colnames(df)) {
    stop(sprintf("Validation Failed: Grouping column '%s' does not exist.", group_col))
  }
  
  vals <- unique(df[[group_col]])
  vals <- vals[!is.na(vals) & vals != ""]
  
  if (length(vals) != 2) {
    stop(sprintf("Validation Failed: Grouping column '%s' must have exactly 2 distinct groups. Found %d: %s.", 
                 group_col, length(vals), paste(vals, collapse = ", ")))
  }
  return(TRUE)
}

#' Validate Sample Size
#'
#' Checks if dataset size is sufficient. If group_col is provided, ensures each group is sufficient.
#' @export
validate_sample_size <- function(df, min_size = 10, group_col = NULL) {
  if (nrow(df) < min_size) {
    stop(sprintf("Validation Failed: Sample size %d is below the minimum required %d.", nrow(df), min_size))
  }
  
  if (!is.null(group_col) && group_col %in% colnames(df)) {
    group_counts <- table(df[[group_col]])
    low_groups <- group_counts[group_counts < min_size]
    if (length(low_groups) > 0) {
      stop(sprintf("Validation Failed: Group sample sizes are too small: %s. Minimum required per group is %d.",
                   paste(names(low_groups), " (N=", low_groups, ")", sep = "", collapse = ", "), min_size))
    }
  }
  return(TRUE)
}

#' Validate Missingness
#'
#' Evaluates NA fractions and triggers warnings for high missingness rates.
#' @export
validate_missingness <- function(df, max_missing_pct = 0.05) {
  missing_pcts <- colMeans(is.na(df))
  high_missing <- missing_pcts[missing_pcts > max_missing_pct]
  if (length(high_missing) > 0) {
    warning_msg <- sprintf("Validation Warning: Columns with missing rate > %.1f%%: %s",
                           max_missing_pct * 100,
                           paste(names(high_missing), " (", round(high_missing * 100, 1), "%)", sep="", collapse=", "))
    warning(warning_msg)
  }
  return(TRUE)
}

#' Validate Variable Ranges
#'
#' Checks if numeric variables lie within expected bounds.
#' @export
validate_ranges <- function(df, col_name, min_val, max_val) {
  if (!col_name %in% colnames(df)) {
    stop(sprintf("Validation Failed: Column '%s' does not exist.", col_name))
  }
  
  vals <- df[[col_name]]
  out_of_bounds <- vals[vals < min_val | vals > max_val]
  out_of_bounds <- out_of_bounds[!is.na(out_of_bounds)]
  
  if (length(out_of_bounds) > 0) {
    stop(sprintf("Validation Failed: Column '%s' contains values out of range [%.1f, %.1f]. Found %d invalid values.",
                 col_name, min_val, max_val, length(out_of_bounds)))
  }
  return(TRUE)
}

#' Validate Causal Claims
#'
#' Warns if statements utilize deterministic causal wording for observational results.
#' @export
validate_causal_claims <- function(text) {
  causal_keywords <- c("cause", "causes", "caused", "causing", "prove", "proves", "proven", "leads to", "lead to")
  
  text_lower <- tolower(text)
  found_keywords <- c()
  for (kw in causal_keywords) {
    pattern <- paste0("\\b", kw, "\\b")
    if (grepl(pattern, text_lower)) {
      found_keywords <- c(found_keywords, kw)
    }
  }
  
  if (length(found_keywords) > 0) {
    warning(sprintf("Validation Warning: Statement contains causal language ('%s'). Remember that association does not imply causation in observational data.",
                    paste(found_keywords, collapse = "', '")))
    return(FALSE)
  }
  return(TRUE)
}

#' Master Student Data Validator
#'
#' Runs structural and value-range checks specifically tailored for the CLEREVA Student Success dataset.
#' @export
validate_student_dataset <- function(df) {
  expected_cols <- c("school", "sex", "age", "address", "famsize", "Pstatus", 
                     "Medu", "Fedu", "Mjob", "Fjob", "reason", "guardian", 
                     "traveltime", "studytime", "failures", "schoolsup", 
                     "famsup", "paid", "activities", "nursery", "higher", 
                     "internet", "romantic", "famrel", "freetime", "goout", 
                     "Dalc", "Walc", "health", "absences", "G1", "G2", "G3")
  
  col_types <- list(
    school = "factor", sex = "factor", age = "numeric", address = "factor",
    famsize = "factor", Pstatus = "factor", Medu = "numeric", Fedu = "numeric",
    Mjob = "factor", Fjob = "factor", reason = "factor", guardian = "factor",
    traveltime = "numeric", studytime = "numeric", failures = "numeric",
    schoolsup = "factor", famsup = "factor", paid = "factor", activities = "factor",
    nursery = "factor", higher = "factor", internet = "factor", romantic = "factor",
    famrel = "numeric", freetime = "numeric", goout = "numeric", Dalc = "numeric",
    Walc = "numeric", health = "numeric", absences = "numeric",
    G1 = "numeric", G2 = "numeric", G3 = "numeric"
  )
  
  # Run structural validations
  validate_columns(df, expected_cols)
  validate_types(df, col_types)
  validate_sample_size(df, min_size = 30) # min sample size of 30 for central limit theorem
  validate_missingness(df)
  
  # Run grade range validations (grades are on a 0-20 scale)
  validate_ranges(df, "G1", 0, 20)
  validate_ranges(df, "G2", 0, 20)
  validate_ranges(df, "G3", 0, 20)
  
  # Run age range validation (expecting secondary students aged 15-22)
  validate_ranges(df, "age", 15, 22)
  
  # Run absences validation (non-negative)
  validate_ranges(df, "absences", 0, 100)
  
  return(TRUE)
}
