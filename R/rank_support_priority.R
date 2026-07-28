# Support Priority Ranking — CLEREVA Student Performance
#
# Generates a ranked list of students by composite support priority score,
# combining model risk probability with auxiliary risk indicators.
#
# Requires: models/student_risk_model.rds (run R/model_risk.R first)
#
# Outputs:
#   data/outputs/tables/support_priority.csv
#   data/outputs/figures/support_priority_distribution.png

source("R/validation.R")

# Composite score weights (must sum to 1.0)
WEIGHT_RISK_PROB  <- 0.50
WEIGHT_FAILURES   <- 0.25
WEIGHT_ABSENCES   <- 0.15
WEIGHT_STUDYTIME  <- 0.10  # inverted: lower study time -> higher score

#' Rank all students by academic support priority
#'
#' @param data_path  Path to the cleaned CSV.
#' @param model_path Path to the saved RDS model.
#' @param fig_dir    Directory for output figures.
#' @param table_dir  Directory for output tables.
#' @param top_pct    Fraction flagged as "High Priority". Default: 0.20.
#' @return Invisible data frame of ranked students.
#' @export
rank_support_priority <- function(
  data_path  = "data/processed/student_clean.csv",
  model_path = "models/student_risk_model.rds",
  fig_dir    = "data/outputs/figures",
  table_dir  = "data/outputs/tables",
  top_pct    = 0.20
) {
  # --- Load & validate -------------------------------------------------------
  if (!file.exists(data_path)) stop(paste("Cleaned dataset not found:", data_path))
  if (!file.exists(model_path)) {
    stop(paste("Model not found:", model_path,
               "\nRun R/model_risk.R first to train the risk model."))
  }

  df    <- read.csv(data_path, stringsAsFactors = TRUE)
  model <- readRDS(model_path)
  validate_student_dataset(df)

  dir.create(fig_dir,   showWarnings = FALSE, recursive = TRUE)
  dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

  # --- Predict risk probability for all students ----------------------------
  pred_prob <- tryCatch(
    predict(model, newdata = df, type = "response"),
    error = function(e) stop(paste("Prediction failed:", e$message))
  )

  # --- Normalise auxiliary indicators to [0, 1] -----------------------------
  # failures: scale 0-3 (capped at 3 in dataset)
  norm_failures <- pmin(df$failures, 3) / 3

  # absences: scale relative to 95th percentile to dampen outlier influence
  abs_p95 <- quantile(df$absences, 0.95, na.rm = TRUE)
  norm_absences <- pmin(df$absences / abs_p95, 1)

  # studytime: 1-4 scale, inverted (lower = higher need)
  norm_studytime_inv <- 1 - ((pmin(pmax(df$studytime, 1), 4) - 1) / 3)

  # --- Composite priority score ---------------------------------------------
  priority_score <- (
    WEIGHT_RISK_PROB * pred_prob        +
    WEIGHT_FAILURES  * norm_failures    +
    WEIGHT_ABSENCES  * norm_absences    +
    WEIGHT_STUDYTIME * norm_studytime_inv
  )

  # --- Assemble ranking table -----------------------------------------------
  n <- nrow(df)
  cutoff <- quantile(priority_score, 1 - top_pct, na.rm = TRUE)

  ranking <- data.frame(
    student_index    = seq_len(n),        # anonymised index, no PII
    risk_probability = round(pred_prob, 4),
    failures         = df$failures,
    absences         = df$absences,
    studytime        = df$studytime,
    schoolsup        = as.character(df$schoolsup),
    actual_at_risk   = as.character(df$at_risk),
    priority_score   = round(priority_score, 4),
    priority_band    = ifelse(priority_score >= cutoff, "High Priority",
                       ifelse(priority_score >= median(priority_score), "Medium Priority",
                              "Low Priority")),
    stringsAsFactors = FALSE
  )
  ranking <- ranking[order(-ranking$priority_score), ]
  ranking$rank <- seq_len(n)

  write.csv(ranking, file.path(table_dir, "support_priority.csv"), row.names = FALSE)
  message("Saved: support_priority.csv")

  # Band summary
  band_summary <- table(ranking$priority_band)
  cat("\n=== Support Priority Bands ===\n")
  print(band_summary)
  cat(sprintf("Top %d%% threshold (High Priority): priority_score >= %.4f\n",
              round(top_pct * 100), cutoff))

  # --- Figure: priority score distribution ----------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 not available \u2014 skipping figures.")
    message("rank_support_priority: completed (no figures).")
    return(invisible(ranking))
  }
  library(ggplot2)

  band_colours <- c(
    "High Priority"   = "#C44E52",
    "Medium Priority" = "#DD8452",
    "Low Priority"    = "#55A868"
  )
  ranking$priority_band <- factor(ranking$priority_band,
                                  levels = c("High Priority", "Medium Priority", "Low Priority"))

  p <- ggplot(ranking, aes(x = priority_score, fill = priority_band)) +
    geom_histogram(binwidth = 0.02, colour = "white", alpha = 0.85) +
    geom_vline(xintercept = cutoff, colour = "#C44E52",
               linetype = "dashed", linewidth = 1) +
    scale_fill_manual(values = band_colours, name = "Priority Band") +
    annotate("text", x = cutoff + 0.02, y = Inf, vjust = 2,
             label = "High Priority\nthreshold",
             colour = "#C44E52", size = 3.5, hjust = 0) +
    labs(
      title    = "Distribution of Support Priority Scores",
      subtitle = paste0("Top ", round(top_pct * 100), "% flagged as High Priority"),
      x        = "Composite Priority Score (0\u20131)",
      y        = "Number of Students",
      caption  = "Priority scores are decision-support indicators only. Human review required."
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title    = element_text(face = "bold"),
          legend.position = "right")

  ggsave(file.path(fig_dir, "support_priority_distribution.png"), p,
         width = 8, height = 5, dpi = 150)
  message("Saved: support_priority_distribution.png")

  message("rank_support_priority: completed successfully.")
  invisible(ranking)
}

if (!interactive() && identical(environment(), globalenv())) rank_support_priority()
