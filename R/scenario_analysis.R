# Scenario Analysis — CLEREVA Student Performance
#
# Simulates what-if interventions by modifying actionable predictor values
# and re-scoring students through the trained logistic regression model.
#
# Requires: models/student_risk_model.rds (run R/model_risk.R first)
#
# Outputs:
#   data/outputs/tables/scenario_results.csv
#   data/outputs/figures/scenario_delta.png

source("R/validation.R")

# Variables that are actionable (can be changed by intervention).
# Immutable attributes (sex, age, family background) are blocked.
ACTIONABLE_VARS <- c(
  "studytime", "absences", "schoolsup", "famsup", "paid",
  "activities", "internet", "romantic", "goout", "Dalc", "Walc"
)

IMMUTABLE_VARS <- c(
  "school", "sex", "age", "address", "famsize", "Pstatus",
  "Medu", "Fedu", "Mjob", "Fjob", "reason", "guardian"
)

#' Run a what-if scenario simulation
#'
#' @param scenario   Named list of variable → new value, e.g.
#'                   list(studytime = 4, schoolsup = "yes").
#' @param data_path  Path to the cleaned CSV.
#' @param model_path Path to the saved RDS model.
#' @param fig_dir    Directory for output figures.
#' @param table_dir  Directory for output tables.
#' @param label      Short label for the scenario (used in outputs).
#' @return Invisible data frame with baseline and scenario predictions.
#' @export
scenario_analysis <- function(
  scenario   = list(studytime = 4),
  data_path  = "data/processed/student_clean.csv",
  model_path = "models/student_risk_model.rds",
  fig_dir    = "data/outputs/figures",
  table_dir  = "data/outputs/tables",
  label      = "scenario"
) {
  # --- Validate scenario spec ------------------------------------------------
  if (!is.list(scenario) || length(scenario) == 0) {
    stop("scenario must be a non-empty named list, e.g. list(studytime = 4)")
  }

  blocked <- intersect(names(scenario), IMMUTABLE_VARS)
  if (length(blocked) > 0) {
    stop(paste(
      "Scenario contains immutable variable(s):", paste(blocked, collapse = ", "),
      "\nOnly actionable variables may be modified:", paste(ACTIONABLE_VARS, collapse = ", ")
    ))
  }

  unknown <- setdiff(names(scenario), ACTIONABLE_VARS)
  if (length(unknown) > 0) {
    warning(paste("Unknown variable(s) in scenario will be passed through:", paste(unknown, collapse = ", ")))
  }

  # Causal language check on label
  validate_causal_claims(label)

  # --- Load & validate -------------------------------------------------------
  if (!file.exists(data_path)) stop(paste("Cleaned dataset not found:", data_path))
  if (!file.exists(model_path)) {
    stop(paste("Model not found:", model_path,
               "\nRun R/model_risk.R first."))
  }

  df    <- read.csv(data_path, stringsAsFactors = TRUE)
  model <- readRDS(model_path)
  validate_student_dataset(df)

  dir.create(fig_dir,   showWarnings = FALSE, recursive = TRUE)
  dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

  # --- Baseline predictions -------------------------------------------------
  baseline_prob <- predict(model, newdata = df, type = "response")

  # --- Apply scenario to a copy of the dataset ------------------------------
  scenario_df <- df
  for (var in names(scenario)) {
    if (!var %in% colnames(scenario_df)) {
      warning(paste("Variable", var, "not found in dataset — skipping."))
      next
    }
    new_val <- scenario[[var]]
    # Coerce to same type as existing column
    if (is.factor(scenario_df[[var]])) {
      new_val <- factor(rep(as.character(new_val), nrow(scenario_df)),
                        levels = levels(scenario_df[[var]]))
    } else if (is.numeric(scenario_df[[var]])) {
      new_val <- as.numeric(new_val)
    }
    scenario_df[[var]] <- new_val
    message(sprintf("Scenario: set %s = %s for all students", var, as.character(scenario[[var]])))
  }

  # --- Scenario predictions -------------------------------------------------
  scenario_prob <- tryCatch(
    predict(model, newdata = scenario_df, type = "response"),
    error = function(e) stop(paste("Scenario prediction failed:", e$message))
  )

  # --- Build comparison table -----------------------------------------------
  delta <- scenario_prob - baseline_prob
  results <- data.frame(
    student_index      = seq_len(nrow(df)),
    baseline_prob      = round(baseline_prob, 4),
    scenario_prob      = round(scenario_prob, 4),
    delta_prob         = round(delta, 4),
    baseline_class     = ifelse(baseline_prob > 0.5, "At Risk", "Low Risk"),
    scenario_class     = ifelse(scenario_prob > 0.5, "At Risk", "Low Risk"),
    class_changed      = ifelse(baseline_prob > 0.5, "At Risk", "Low Risk") !=
                         ifelse(scenario_prob > 0.5, "At Risk", "Low Risk"),
    actual_at_risk     = as.character(df$at_risk),
    stringsAsFactors   = FALSE
  )
  results <- results[order(results$delta_prob), ]

  # Scenario summary
  n_improved   <- sum(results$delta_prob < -0.01)
  n_worsened   <- sum(results$delta_prob >  0.01)
  n_transitioned <- sum(results$class_changed & results$baseline_class == "At Risk" &
                        results$scenario_class == "Low Risk")

  cat("\n=== Scenario Analysis:", label, "===\n")
  cat(sprintf("  Variables modified: %s\n", paste(names(scenario), collapse = ", ")))
  cat(sprintf("  Students with reduced risk (delta < -0.01): %d\n", n_improved))
  cat(sprintf("  Students with increased risk (delta > +0.01): %d\n", n_worsened))
  cat(sprintf("  Students shifted At Risk -> Low Risk: %d\n", n_transitioned))
  cat(sprintf("  Mean delta in risk probability: %.4f\n", mean(delta)))

  out_fname <- paste0("scenario_results_", gsub("[^a-z0-9]", "_", tolower(label)), ".csv")
  write.csv(results, file.path(table_dir, out_fname), row.names = FALSE)
  message(paste("Saved:", out_fname))

  # --- Figure: delta distribution -------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 not available \u2014 skipping figures.")
    message("scenario_analysis: completed (no figures).")
    return(invisible(results))
  }
  library(ggplot2)

  results$change_dir <- ifelse(results$delta_prob < -0.01, "Risk Reduced",
                        ifelse(results$delta_prob >  0.01, "Risk Increased", "Negligible Change"))
  results$change_dir <- factor(results$change_dir,
                               levels = c("Risk Reduced", "Negligible Change", "Risk Increased"))

  p_delta <- ggplot(results, aes(x = delta_prob, fill = change_dir)) +
    geom_histogram(binwidth = 0.01, colour = "white", alpha = 0.85) +
    geom_vline(xintercept = 0, colour = "grey30", linetype = "dashed") +
    scale_fill_manual(values = c(
      "Risk Reduced"      = "#55A868",
      "Negligible Change" = "#8DA0CB",
      "Risk Increased"    = "#C44E52"
    ), name = "Change") +
    labs(
      title    = paste("Simulated Risk Probability Change \u2014", label),
      subtitle = paste("Scenario:", paste(paste(names(scenario), scenario, sep = " = "), collapse = "; ")),
      x        = "\u0394 Risk Probability (Scenario \u2212 Baseline)",
      y        = "Number of Students",
      caption  = "Simulated scenario \u2014 model-based extrapolation only. Results are NOT causal guarantees."
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title    = element_text(face = "bold"),
          legend.position = "right")

  fig_fname <- paste0("scenario_delta_", gsub("[^a-z0-9]", "_", tolower(label)), ".png")
  ggsave(file.path(fig_dir, fig_fname), p_delta, width = 9, height = 5, dpi = 150)
  message(paste("Saved:", fig_fname))

  message("scenario_analysis: completed successfully.")
  invisible(results)
}

# Example default run when sourced directly
if (!interactive() && identical(environment(), globalenv())) {
  scenario_analysis(
    scenario = list(studytime = 4, schoolsup = "yes"),
    label    = "increased_study_and_support"
  )
}
