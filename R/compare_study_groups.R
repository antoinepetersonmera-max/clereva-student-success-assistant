# Two-Group Comparison — CLEREVA Student Performance
#
# Compares grade distributions between two student groups using Wilcoxon
# rank-sum tests with Bonferroni correction and rank-biserial effect sizes.
#
# Outputs:
#   data/outputs/tables/compare_results.csv
#   data/outputs/figures/compare_<group_col>_bar.png
#   data/outputs/figures/compare_<group_col>_density.png

source("R/validation.R")

#' Rank-biserial correlation (effect size for Wilcoxon rank-sum test)
#' @param x Numeric vector — group 1.
#' @param y Numeric vector — group 2.
#' @return Numeric effect size r in [-1, 1].
rank_biserial <- function(x, y) {
  nx <- length(x[!is.na(x)])
  ny <- length(y[!is.na(y)])
  w  <- wilcox.test(x, y, exact = FALSE)$statistic
  r  <- 1 - (2 * w) / (nx * ny)
  round(as.numeric(r), 3)
}

#' Compare two student groups on grade outcomes
#'
#' @param group_col  Column name of the binary grouping variable. Default: "school"
#' @param outcomes   Character vector of outcome columns to compare.
#' @param data_path  Path to the cleaned CSV.
#' @param fig_dir    Directory for output figures.
#' @param table_dir  Directory for output tables.
#' @return Invisible data frame of test results.
#' @export
compare_study_groups <- function(
  group_col = "school",
  outcomes  = c("G1", "G2", "G3"),
  data_path = "data/processed/student_clean.csv",
  fig_dir   = "data/outputs/figures",
  table_dir = "data/outputs/tables"
) {
  # --- Load & validate -------------------------------------------------------
  if (!file.exists(data_path)) stop(paste("Cleaned dataset not found:", data_path))
  df <- read.csv(data_path, stringsAsFactors = TRUE)
  validate_student_dataset(df)
  validate_two_groups(df, group_col)
  validate_sample_size(df, min_size = 20, group_col = group_col)

  dir.create(fig_dir,   showWarnings = FALSE, recursive = TRUE)
  dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

  groups  <- levels(factor(df[[group_col]]))
  g1_data <- df[df[[group_col]] == groups[1], ]
  g2_data <- df[df[[group_col]] == groups[2], ]

  # --- Wilcoxon tests --------------------------------------------------------
  n_tests <- length(outcomes)
  results <- do.call(rbind, lapply(outcomes, function(out) {
    x  <- g1_data[[out]]
    y  <- g2_data[[out]]
    wt <- wilcox.test(x, y, exact = FALSE, conf.int = TRUE)
    data.frame(
      outcome           = out,
      group1            = groups[1],
      group2            = groups[2],
      n_group1          = sum(!is.na(x)),
      n_group2          = sum(!is.na(y)),
      median_group1     = round(median(x, na.rm = TRUE), 2),
      median_group2     = round(median(y, na.rm = TRUE), 2),
      mean_group1       = round(mean(x, na.rm = TRUE), 2),
      mean_group2       = round(mean(y, na.rm = TRUE), 2),
      diff_medians      = round(median(x, na.rm = TRUE) - median(y, na.rm = TRUE), 2),
      W_statistic       = round(wt$statistic, 2),
      p_value           = round(wt$p.value, 4),
      p_adj_bonferroni  = round(p.adjust(wt$p.value, method = "bonferroni", n = n_tests), 4),
      effect_size_r     = rank_biserial(x, y),
      stringsAsFactors  = FALSE
    )
  }))

  write.csv(results, file.path(table_dir, "compare_results.csv"), row.names = FALSE)
  message("Saved: compare_results.csv")

  # Print summary to console
  cat("\n=== Group Comparison:", group_col, "===\n")
  print(results[, c("outcome", "median_group1", "median_group2",
                    "p_value", "p_adj_bonferroni", "effect_size_r")])

  # --- Figures ---------------------------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 not available — skipping figures.")
    message("compare_study_groups: completed (no figures).")
    return(invisible(results))
  }
  library(ggplot2)

  # Long-format for plotting
  plot_data <- do.call(rbind, lapply(outcomes, function(out) {
    data.frame(
      Outcome = out,
      Group   = as.character(df[[group_col]]),
      Grade   = df[[out]],
      stringsAsFactors = FALSE
    )
  }))
  plot_data$Outcome <- factor(plot_data$Outcome, levels = outcomes)
  plot_data$Group   <- factor(plot_data$Group,   levels = groups)

  # Average Bar Chart
  library(dplyr)
  summary_data <- plot_data %>%
    group_by(Outcome, Group) %>%
    summarise(
      Mean_Grade = mean(Grade, na.rm = TRUE),
      SE = sd(Grade, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  p_bar <- ggplot(summary_data, aes(x = Group, y = Mean_Grade, fill = Group)) +
    geom_col(alpha = 0.85, width = 0.6) +
    geom_errorbar(aes(ymin = Mean_Grade - SE, ymax = Mean_Grade + SE), width = 0.2, colour = "grey30") +
    geom_text(aes(label = round(Mean_Grade, 1), y = Mean_Grade + SE), vjust = -0.5, fontface = "bold", size = 3.5) +
    facet_wrap(~Outcome) +
    scale_fill_manual(values = c("#4C72B0", "#DD8452")) +
    labs(
      title    = paste("Average Grade Comparison by", group_col),
      subtitle = paste(groups[1], "vs.", groups[2]),
      x        = group_col,
      y        = "Average Grade (0\u201320)",
      caption  = "Mean grade with standard error bars. Wilcoxon rank-sum test. Observational data."
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none",
          plot.title  = element_text(face = "bold"),
          strip.text  = element_text(face = "bold"))

  bar_fname <- paste0("compare_", group_col, "_bar.png")
  ggsave(file.path(fig_dir, bar_fname), p_bar, width = 10, height = 4, dpi = 150)
  message(paste("Saved:", bar_fname))

  # Density plot (G3 only)
  g3_data <- plot_data[plot_data$Outcome == "G3", ]
  p_dens <- ggplot(g3_data, aes(x = Grade, fill = Group, colour = Group)) +
    geom_density(alpha = 0.4, linewidth = 1) +
    scale_fill_manual(values   = c("#4C72B0", "#DD8452")) +
    scale_colour_manual(values = c("#4C72B0", "#DD8452")) +
    labs(
      title    = paste("Final Grade (G3) Density by", group_col),
      x        = "Final Grade (G3)",
      y        = "Density",
      fill     = group_col,
      colour   = group_col,
      caption  = "Observational data."
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))

  dens_fname <- paste0("compare_", group_col, "_density.png")
  ggsave(file.path(fig_dir, dens_fname), p_dens, width = 7, height = 4, dpi = 150)
  message(paste("Saved:", dens_fname))

  message("compare_study_groups: completed successfully.")
  invisible(results)
}

if (!interactive() && identical(environment(), globalenv())) compare_study_groups()
