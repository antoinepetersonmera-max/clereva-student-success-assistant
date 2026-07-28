#' Clean and Prepare Student Data
#'
#' Converts numeric columns, engineers the binary target variable 'at_risk' based on G3 < 10,
#' and converts character variables to factors. Saves the cleaned data to disk.
#'
#' @param raw_data A data frame containing the raw student data.
#' @param output_path File path to save the cleaned CSV. Defaults to "data/processed/student_clean.csv".
#' @return A data frame containing the cleaned student data.
#' @export
clean_data <- function(raw_data, output_path = "data/processed/student_clean.csv") {
  if (missing(raw_data) || is.null(raw_data)) {
    stop("Input raw_data is required")
  }
  
  # Copy data frame
  clean_df <- raw_data
  
  # 1. Convert grade columns to numeric (handles quoted values like "5" and "6")
  clean_df$G1 <- as.numeric(clean_df$G1)
  clean_df$G2 <- as.numeric(clean_df$G2)
  clean_df$G3 <- as.numeric(clean_df$G3)
  
  # 2. Engineer target variable 'at_risk' (G3 < 10 represents academic risk)
  # Level 1 is "Low Risk" (baseline/negative class)
  # Level 2 is "At Risk" (target/positive class)
  clean_df$at_risk <- factor(
    ifelse(clean_df$G3 < 10, "At Risk", "Low Risk"),
    levels = c("Low Risk", "At Risk")
  )
  
  # 3. Convert all other character columns to factors
  char_cols <- sapply(clean_df, is.character)
  for (col_name in names(clean_df)[char_cols]) {
    clean_df[[col_name]] <- as.factor(clean_df[[col_name]])
  }
  
  # 4. Save processed dataset
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  write.csv(clean_df, file = output_path, row.names = FALSE)
  
  message(sprintf("Successfully cleaned dataset. Saved to %s (%d rows, %d columns)", 
                  output_path, nrow(clean_df), ncol(clean_df)))
  
  return(clean_df)
}
