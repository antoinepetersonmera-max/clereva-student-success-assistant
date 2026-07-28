# Absence Analysis — CLEREVA Student Performance
#
# Investigates the relationship between student absences and final grade (G3).
# Segments students into absence tiers, runs a Wilcoxon test across tiers,
# fits a simple linear regression G3 ~ absences, and flags outliers.
#
# Outputs:
#   data/outputs/tables/absence_summary.csv
#   data/outputs/tables/absence_tier_summary.csv
#   data/outputs/figures/absence_scatter.png
#   data/outputs/figures/absence_tier_bar.png

source("R/validation.R")

#' Analyse the relationship between absences and academic outcomes
#'
#' @param data_path  Path to the cleaned CSV.
#' @param fig_dir    Directory for output figures.
#' @param table_dir  Directory for output tables.
#' @return Invisible list with regression model and tier summary.
#' @export
analyze_absences <- function(
  data_path = "data/processed/student_clean.csv",
  fig_dir   = "data/outputs/figures",
  table_dir = "data/outputs/tables"
) {
  # --- Load & validate -------------------------------------------------------
  if (!file.exists(data_path)) stop(paste("Cleaned dataset not found:", data_path))
  df <- read.csv(data_path, stringsAsFactors = TRUE)
  validate_student_dataset(df)
  validate_ranges(df, "absences", 0, 100)

  dir.create(fig_dir,   showWarnings = FALSE, recursive = TRUE)
  dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

  # --- Absence distribution summary ------------------------------------------
  abs_vec <- df$absences
  outlier_threshold <- 40
  n_outliers <- sum(abs_vec > outlier_threshold, na.rm = TRUE)

  absence_summary <- data.frame(
    statistic = c("n", "mean", "sd", "median", "q25", "q75",
                  "min", "max", "n_zero", "n_outliers_gt40"),
    value = c(
      sum(!is.na(abs_vec)),
      round(mean(abs_vec,   na.rm = TRUE), 2),
      round(sd(abs_vec,     na.rm = TRUE), 2),
      round(median(abs_vec, na.rm = TRUE), 2),
      round(quantile(abs_vec, 0.25, na.rm = TRUE), 2),
      round(quantile(abs_vec, 0.75, na.rm = TRUE), 2),
      min(abs_vec, na.rm = TRUE),
      max(abs_vec, na.rm = TRUE),
      sum(abs_vec == 0, na.rm = TRUE),
      n_outliers
    )
  )
  write.csv(absence_summary, file.path(table_dir, "absence_summary.csv"), row.names = FALSE)
  message("Saved: absence_summary.csv")

  if (n_outliers > 0) {
    message(sprintf(
      "Note: %d student(s) have absences > %d. These may exert disproportionate influence on regression estimates.",
      n_outliers, outlier_threshold
    ))
  }

  # --- Tier segmentation ------------------------------------------------------
  df$absence_tier <- cut(
    df$absences,
    breaks = c(-Inf, 0, 5, 10, Inf),
    labels = c("0 absences", "1\u20135", "6\u201310", "11+"),
    right  = TRUE
  )

  tier_summary <- do.call(rbind, lapply(levels(df$absence_tier), function(tier) {
    sub <- df[df$absence_tier == tier, ]
    data.frame(
      tier         = tier,
      n            = nrow(sub),
      pct_of_total = round(100 * nrow(sub) / nrow(df), 1),
      median_G3    = round(median(sub$G3, na.rm = TRUE), 2),
      mean_G3      = round(mean(sub$G3,   na.rm = TRUE), 2),
      pct_at_risk  = round(100 * mean(sub$at_risk == "At Risk"), 1),
      stringsAsFactors = FALSE
    )
  }))
  write.csv(tier_summary, file.path(table_dir, "absence_tier_summary.csv"), row.names = FALSE)
  message("Saved: absence_tier_summary.csv")

  cat("\n=== Absence Tier Summary ===\n")
  print(tier_summary)

  # --- Linear regression G3 ~ absences ---------------------------------------
  lm_fit <- lm(G3 ~ absences, data = df)
  lm_sum <- summary(lm_fit)
  cat("\n=== Linear Regression: G3 ~ absences ===\n")
  cat(sprintf("Intercept: %.3f | Slope (absences): %.3f | R2: %.4f | p-value: %.4f\n",
              coef(lm_fit)[1], coef(lm_fit)[2],
              lm_sum$r.squared, lm_sum$coefficients[2, 4]))

  # Wilcoxon test across tiers (Kruskal-Wallis for > 2 groups)
  kw <- kruskal.test(G3 ~ absence_tier, data = df)
  cat(sprintf("\nKruskal-Wallis test across absence tiers: H = %.3f, df = %d, p = %.4f\n",
              kw$statistic, kw$parameter, kw$p.value))

  # --- Figures ---------------------------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 not available \u2014 skipping figures.")
    message("analyze_absences: completed (no figures).")
    return(invisible(list(regression = lm_fit, tier_summary = tier_summary)))
  }
  library(ggplot2)

  # 1. Scatter plot G3 ~ absences with regression line
  p_scatter <- ggplot(df, aes(x = absences, y = G3, colour = at_risk)) +
    geom_jitter(alpha = 0.5, width = 0.3, height = 0.2, size = 1.8) +
    geom_smooth(method = "lm", colour = "grey30", se = TRUE, linewidth = 1) +
    scale_colour_manual(values = c("Low Risk" = "#55A868", "At Risk" = "#C44E52"),
                        name = "Risk Status") +
    labs(
      title    = "Absences vs. Final Grade (G3)",
      subtitle = "Linear regression line with 95% confidence interval",
      x        = "Number of Absences",
      y        = "Final Grade (G3)",
      caption  = "Observational data \u2014 associations only, not causal effects."
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title    = element_text(face = "bold"),
          legend.position = "right")

  ggsave(file.path(fig_dir, "absence_scatter.png"), p_scatter,
         width = 8, height = 5, dpi = 150)
  message("Saved: absence_scatter.png")

  # 2. Bar chart by absence tier (showing mean grades)
  library(dplyr)
  summary_tier <- df %>%
    group_by(absence_tier) %>%
    summarise(
      Mean_G3 = mean(G3, na.rm = TRUE),
      SE = sd(G3, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  p_tier <- ggplot(summary_tier, aes(x = absence_tier, y = Mean_G3, fill = absence_tier)) +
    geom_col(alpha = 0.85, width = 0.5) +
    geom_errorbar(aes(ymin = Mean_G3 - SE, ymax = Mean_G3 + SE), width = 0.15, colour = "grey30") +
    geom_text(aes(label = round(Mean_G3, 1), y = Mean_G3 + SE), vjust = -0.5, fontface = "bold", size = 4) +
    scale_fill_manual(values = c(
      "0 absences" = "#55A868",
      "1\u20135"   = "#8FBC8F",
      "6\u201310"  = "#DD8452",
      "11+"        = "#C44E52"
    )) +
    labs(
      title    = "Average Final Grade (G3) by Absence Tier",
      x        = "Absence Tier",
      y        = "Average Final Grade (G3)",
      caption  = "Mean grade with standard error bars. Kruskal\u2013Wallis test across tiers. Observational data."
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"))

  ggsave(file.path(fig_dir, "absence_tier_bar.png"), p_tier,
         width = 7, height = 5, dpi = 150)
  message("Saved: absence_tier_bar.png")

  message("analyze_absences: completed successfully.")
  invisible(list(regression = lm_fit, tier_summary = tier_summary))
}

if (!interactive() && identical(environment(), globalenv())) analyze_absences()
