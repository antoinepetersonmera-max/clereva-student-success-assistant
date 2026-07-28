# Exploratory Data Analysis — CLEREVA Student Performance
#
# Produces descriptive statistics, grade distribution plots, and a correlation
# heatmap for the Portuguese Mathematics cohort (N = 395).
#
# Outputs:
#   data/outputs/tables/explore_summary.csv
#   data/outputs/figures/grade_distribution.png
#   data/outputs/figures/grade_by_risk.png
#   data/outputs/figures/correlation_heatmap.png

source("R/validation.R")

#' Run Exploratory Data Analysis
#'
#' @param data_path  Path to cleaned CSV. Default: "data/processed/student_clean.csv"
#' @param fig_dir    Directory for output figures.
#' @param table_dir  Directory for output tables.
#' @return Invisible data frame of summary statistics.
#' @export
explore_performance <- function(
  data_path = "data/processed/student_clean.csv",
  fig_dir   = "data/outputs/figures",
  table_dir = "data/outputs/tables"
) {
  # --- Load & validate -------------------------------------------------------
  if (!file.exists(data_path)) stop(paste("Cleaned dataset not found:", data_path))
  df <- read.csv(data_path, stringsAsFactors = TRUE)
  validate_student_dataset(df)

  dir.create(fig_dir,   showWarnings = FALSE, recursive = TRUE)
  dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

  # --- Summary statistics ----------------------------------------------------
  numeric_cols <- c("G1", "G2", "G3", "absences", "studytime", "failures",
                    "age", "traveltime", "famrel", "freetime", "goout",
                    "Dalc", "Walc", "health", "Medu", "Fedu")

  summary_stats <- do.call(rbind, lapply(numeric_cols, function(col) {
    x <- df[[col]]
    data.frame(
      variable = col,
      n        = sum(!is.na(x)),
      mean     = round(mean(x,   na.rm = TRUE), 2),
      sd       = round(sd(x,     na.rm = TRUE), 2),
      median   = round(median(x, na.rm = TRUE), 2),
      q25      = round(quantile(x, 0.25, na.rm = TRUE), 2),
      q75      = round(quantile(x, 0.75, na.rm = TRUE), 2),
      min      = min(x, na.rm = TRUE),
      max      = max(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  write.csv(summary_stats, file.path(table_dir, "explore_summary.csv"), row.names = FALSE)
  message("Saved: explore_summary.csv")

  # --- Categorical distribution table ----------------------------------------
  cat_cols <- c("school", "sex", "address", "famsize", "Pstatus", "higher",
                "schoolsup", "internet", "romantic", "at_risk")
  cat_stats <- do.call(rbind, lapply(cat_cols, function(col) {
    tbl <- table(df[[col]])
    data.frame(
      variable = col,
      level    = names(tbl),
      count    = as.integer(tbl),
      pct      = round(100 * as.integer(tbl) / nrow(df), 1),
      stringsAsFactors = FALSE
    )
  }))
  write.csv(cat_stats, file.path(table_dir, "explore_categorical.csv"), row.names = FALSE)
  message("Saved: explore_categorical.csv")

  # --- Figures ---------------------------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 not available — skipping figures. Install with install.packages('ggplot2')")
    message("explore_performance: completed (no figures).")
    return(invisible(summary_stats))
  }
  library(ggplot2)

  # 1. Grade distribution histogram (G1, G2, G3)
  grades_long <- data.frame(
    Period = rep(c("G1 (Period 1)", "G2 (Period 2)", "G3 (Final)"), each = nrow(df)),
    Grade  = c(df$G1, df$G2, df$G3),
    stringsAsFactors = FALSE
  )
  grades_long$Period <- factor(grades_long$Period,
                               levels = c("G1 (Period 1)", "G2 (Period 2)", "G3 (Final)"))

  p_hist <- ggplot(grades_long, aes(x = Grade, fill = Period)) +
    geom_histogram(binwidth = 1, colour = "white", alpha = 0.85) +
    facet_wrap(~Period) +
    scale_fill_manual(values = c("#4C72B0", "#DD8452", "#55A868")) +
    labs(
      title    = "Grade Distribution by Period",
      subtitle = "CLEREVA dataset — Portuguese Mathematics cohort (N = 395)",
      x        = "Grade (0\u201320 scale)",
      y        = "Number of Students",
      caption  = "Observational data. Findings represent associations, not causal effects."
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none",
          plot.title    = element_text(face = "bold"),
          strip.text    = element_text(face = "bold"))

  ggsave(file.path(fig_dir, "grade_distribution.png"), p_hist,
         width = 10, height = 4, dpi = 150)
  message("Saved: grade_distribution.png")

  # 2. G3 bar chart by at_risk (showing mean grades)
  library(dplyr)
  summary_risk <- df %>%
    group_by(at_risk) %>%
    summarise(
      Mean_G3 = mean(G3, na.rm = TRUE),
      SE = sd(G3, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  p_risk_bar <- ggplot(summary_risk, aes(x = at_risk, y = Mean_G3, fill = at_risk)) +
    geom_col(alpha = 0.85, width = 0.5) +
    geom_errorbar(aes(ymin = Mean_G3 - SE, ymax = Mean_G3 + SE), width = 0.15, colour = "grey30") +
    geom_text(aes(label = round(Mean_G3, 1), y = Mean_G3 + SE), vjust = -0.5, fontface = "bold", size = 4) +
    scale_fill_manual(values = c("Low Risk" = "#55A868", "At Risk" = "#C44E52")) +
    labs(
      title    = "Average Final Grade (G3) by Risk Status",
      subtitle = "At Risk defined as G3 < 10",
      x        = "Risk Category",
      y        = "Average Final Grade (G3)",
      caption  = "Mean grade with standard error bars. Observational data."
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"))

  ggsave(file.path(fig_dir, "grade_by_risk.png"), p_risk_bar,
         width = 6, height = 5, dpi = 150)
  message("Saved: grade_by_risk.png")

  # 3. Correlation heatmap
  corr_vars <- c("G1", "G2", "G3", "absences", "studytime", "failures",
                 "Medu", "Fedu", "famrel", "goout", "Dalc", "Walc", "health")
  corr_mat  <- cor(df[, corr_vars], use = "pairwise.complete.obs")
  corr_long <- expand.grid(Var1 = rownames(corr_mat), Var2 = colnames(corr_mat),
                           stringsAsFactors = FALSE)
  corr_long$r <- as.vector(corr_mat)

  p_corr <- ggplot(corr_long, aes(x = Var1, y = Var2, fill = r)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = round(r, 2)), size = 2.8) +
    scale_fill_gradient2(low = "#C44E52", mid = "white", high = "#4C72B0",
                         midpoint = 0, limits = c(-1, 1), name = "r") +
    labs(title   = "Correlation Heatmap \u2014 Key Numeric Predictors",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x  = element_text(angle = 45, hjust = 1),
          plot.title   = element_text(face = "bold"))

  ggsave(file.path(fig_dir, "correlation_heatmap.png"), p_corr,
         width = 9, height = 8, dpi = 150)
  message("Saved: correlation_heatmap.png")

  message("explore_performance: completed successfully.")
  invisible(summary_stats)
}

# Run when sourced directly (not when called from Quarto or run_all.R)
if (!interactive() && identical(environment(), globalenv())) explore_performance()
