#' Ingest Raw Student Data
#'
#' Reads the semicolon-delimited raw CSV file containing student performance data.
#'
#' @param filepath Path to the raw CSV file. Defaults to "data/raw/student-mat.csv".
#' @return A data frame containing the raw student data.
#' @export
import_data <- function(filepath = "data/raw/student-mat.csv") {
  if (!file.exists(filepath)) {
    stop(paste("Raw dataset not found at:", filepath))
  }
  
  # Read semicolon-delimited CSV
  raw_data <- read.csv(filepath, sep = ";", stringsAsFactors = FALSE)
  
  message(sprintf("Successfully imported %d rows and %d columns from %s", 
                  nrow(raw_data), ncol(raw_data), filepath))
  
  return(raw_data)
}
