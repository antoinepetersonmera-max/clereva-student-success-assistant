# Logistic Regression Risk Model — CLEREVA Student Performance
#
# Trains a logistic regression model to predict academic risk (at_risk = G3 < 10).
# G1 and G2 are intentionally excluded to prevent data leakage in an early-
# intervention context.
#
# Outputs:
#   models/student_risk_model.rds
#   models/model_metrics.csv
#   models/predictor_reference.csv
#   data/outputs/figures/roc_curve.png
#   data/outputs/figures/predictor_importance.png

source("R/validation.R")

# Predictors used by the model — intentionally excludes G1, G2 (data leakage)
MODEL_PREDICTORS <- c(
  "failures", "absences", "studytime", "higher", "schoolsup",
  "Medu", "Fedu", "goout", "famrel", "health", "age",
  "address", "internet", "romantic", "paid", "famsup",
  "activities", "Dalc", "Walc", "traveltime"
)

#' Compute confusion-matrix metrics
#' @param actual    Factor of actual class labels.
#' @param predicted Factor of predicted class labels.
#' @return Named numeric vector.
compute_cm_metrics <- function(actual, predicted) {
  cm <- table(Actual = actual, Predicted = predicted)
  TP <- if ("At Risk"  %in% rownames(cm) & "At Risk"  %in% colnames(cm)) cm["At Risk",  "At Risk"]  else 0L
  TN <- if ("Low Risk" %in% rownames(cm) & "Low Risk" %in% colnames(cm)) cm["Low Risk", "Low Risk"] else 0L
  FP <- if ("Low Risk" %in% rownames(cm) & "At Risk"  %in% colnames(cm)) cm["Low Risk", "At Risk"]  else 0L
  FN <- if ("At Risk"  %in% rownames(cm) & "Low Risk" %in% colnames(cm)) cm["At Risk",  "Low Risk"] else 0L
  c(
    accuracy    = round((TP + TN) / (TP + TN + FP + FN), 4),
    sensitivity = round(TP / max(TP + FN, 1), 4),
    specificity = round(TN / max(TN + FP, 1), 4),
    ppv         = round(TP / max(TP + FP, 1), 4),
    npv         = round(TN / max(TN + FN, 1), 4)
  )
}

#' Build and evaluate the academic risk logistic regression model
#'
#' @param data_path  Path to the cleaned CSV.
#' @param model_dir  Directory to save model artefacts.
#' @param fig_dir    Directory for output figures.
#' @param seed       Random seed for reproducibility.
#' @return Invisible list with model, metrics, and predictor table.
#' @export
model_risk <- function(
  data_path = "data/processed/student_clean.csv",
  model_dir = "models",
  fig_dir   = "data/outputs/figures",
  seed      = 42
) {
  # --- Load & validate -------------------------------------------------------
  if (!file.exists(data_path)) stop(paste("Cleaned dataset not found:", data_path))
  df <- read.csv(data_path, stringsAsFactors = TRUE)
  validate_student_dataset(df)

  dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(fig_dir,   showWarnings = FALSE, recursive = TRUE)

  # --- Prepare modelling data ------------------------------------------------
  model_df <- df[, c("at_risk", MODEL_PREDICTORS)]
  model_df  <- na.omit(model_df)
  n_total   <- nrow(model_df)

  # --- Stratified 80/20 train/test split ------------------------------------
  set.seed(seed)
  risk_idx  <- which(model_df$at_risk == "At Risk")
  low_idx   <- which(model_df$at_risk == "Low Risk")
  train_idx <- c(
    sample(risk_idx, size = floor(0.8 * length(risk_idx))),
    sample(low_idx,  size = floor(0.8 * length(low_idx)))
  )
  train_df <- model_df[ train_idx, ]
  test_df  <- model_df[-train_idx, ]

  message(sprintf("Split: %d train / %d test (seed = %d)", nrow(train_df), nrow(test_df), seed))

  # --- Fit logistic regression -----------------------------------------------
  formula_str <- paste("at_risk ~", paste(MODEL_PREDICTORS, collapse = " + "))
  fit <- glm(as.formula(formula_str), data = train_df, family = binomial(link = "logit"))

  # --- Predictions -----------------------------------------------------------
  pred_prob  <- predict(fit, newdata = test_df, type = "response")
  pred_class <- factor(ifelse(pred_prob > 0.5, "At Risk", "Low Risk"),
                       levels = c("Low Risk", "At Risk"))
  actual     <- test_df$at_risk

  # --- Confusion-matrix metrics ----------------------------------------------
  cm_vals <- compute_cm_metrics(actual, pred_class)

  # --- AUC-ROC ---------------------------------------------------------------
  auc_val <- NA_real_
  if (requireNamespace("pROC", quietly = TRUE)) {
    roc_obj <- pROC::roc(as.numeric(actual == "At Risk"), pred_prob, quiet = TRUE)
    auc_val <- round(as.numeric(pROC::auc(roc_obj)), 4)

    # ROC curve figure
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      library(ggplot2)
      roc_df <- data.frame(
        fpr = 1 - roc_obj$specificities,
        tpr = roc_obj$sensitivities
      )
      p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
        geom_line(colour = "#4C72B0", linewidth = 1.2) +
        geom_abline(linetype = "dashed", colour = "grey60") +
        annotate("text", x = 0.72, y = 0.15,
                 label = paste0("AUC = ", round(auc_val, 3)),
                 size = 5, colour = "#4C72B0", fontface = "bold") +
        labs(
          title    = "ROC Curve \u2014 Academic Risk Model",
          subtitle = "Logistic regression (G1/G2 excluded)",
          x        = "1 \u2212 Specificity (False Positive Rate)",
          y        = "Sensitivity (True Positive Rate)"
        ) +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold"))
      ggsave(file.path(fig_dir, "roc_curve.png"), p_roc, width = 6, height = 5, dpi = 150)
      message("Saved: roc_curve.png")
    }
  } else {
    message("pROC not installed \u2014 AUC will be NA. Install with install.packages('pROC')")
  }

  # --- Save model metrics ----------------------------------------------------
  metrics <- data.frame(
    metric = c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV", "AUC_ROC",
               "N_total", "N_train", "N_test", "N_at_risk_train", "N_at_risk_test"),
    value  = c(cm_vals["accuracy"], cm_vals["sensitivity"], cm_vals["specificity"],
               cm_vals["ppv"], cm_vals["npv"], auc_val,
               n_total, nrow(train_df), nrow(test_df),
               sum(train_df$at_risk == "At Risk"),
               sum(actual == "At Risk"))
  )
  write.csv(metrics, file.path(model_dir, "model_metrics.csv"), row.names = FALSE)
  message("Saved: model_metrics.csv")

  cat("\n=== Model Metrics ===\n")
  print(metrics)

  # --- Predictor reference (odds ratios + 95% CIs) ---------------------------
  coef_tbl <- summary(fit)$coefficients
  or_table  <- data.frame(
    predictor  = rownames(coef_tbl),
    estimate   = round(coef_tbl[, "Estimate"],    4),
    std_error  = round(coef_tbl[, "Std. Error"],  4),
    odds_ratio = round(exp(coef_tbl[, "Estimate"]), 4),
    ci_lower   = round(exp(coef_tbl[, "Estimate"] - 1.96 * coef_tbl[, "Std. Error"]), 4),
    ci_upper   = round(exp(coef_tbl[, "Estimate"] + 1.96 * coef_tbl[, "Std. Error"]), 4),
    p_value    = round(coef_tbl[, "Pr(>|z|)"],   4),
    significant = coef_tbl[, "Pr(>|z|)"] < 0.05,
    stringsAsFactors = FALSE
  )
  write.csv(or_table, file.path(model_dir, "predictor_reference.csv"), row.names = FALSE)
  message("Saved: predictor_reference.csv")

  # Predictor importance plot (|estimate|, non-intercept)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    pred_plot <- or_table[or_table$predictor != "(Intercept)", ]
    pred_plot <- pred_plot[order(abs(pred_plot$estimate)), ]
    pred_plot$predictor <- factor(pred_plot$predictor, levels = pred_plot$predictor)
    pred_plot$direction <- ifelse(pred_plot$estimate > 0, "Increases risk", "Decreases risk")

    p_pred <- ggplot(pred_plot, aes(x = estimate, y = predictor, fill = direction)) +
      geom_col(alpha = 0.85) +
      geom_vline(xintercept = 0, colour = "grey30", linetype = "dashed") +
      scale_fill_manual(values = c("Increases risk" = "#C44E52", "Decreases risk" = "#55A868")) +
      labs(
        title    = "Logistic Regression Coefficients",
        subtitle = "Positive = higher log-odds of At Risk",
        x        = "Coefficient Estimate",
        y        = NULL,
        fill     = NULL,
        caption  = "Observational data \u2014 associations only."
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title    = element_text(face = "bold"),
            legend.position = "bottom")

    ggsave(file.path(fig_dir, "predictor_importance.png"), p_pred,
           width = 8, height = 7, dpi = 150)
    message("Saved: predictor_importance.png")
  }

  # --- Save model ------------------------------------------------------------
  saveRDS(fit, file.path(model_dir, "student_risk_model.rds"))
  message("Saved: student_risk_model.rds")

  message("model_risk: completed successfully.")
  invisible(list(model = fit, metrics = metrics, predictor_reference = or_table))
}

if (!interactive() && identical(environment(), globalenv())) model_risk()
