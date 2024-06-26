#' Apply a Boxcar Running Average to Data
#'
#' @param data A data frame containing the time series data with a `count` column.
#' @param nsmo The number of points to include in the moving average window, defaults to 3.
#' @param edge_truncate A logical indicating whether to fill edge values with original values, defaults to FALSE.
#' @return A data frame with the original `epi_week` and `count` columns, where `count` contains the smoothed data.
#' @export
apply_boxcar_average <- function(data, nsmo = 3, edge_truncate = FALSE) {
  # Ensure required package is loaded
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' is required but is not installed.")
  }
  
  # Load required library
  library(zoo)
  
  # Check that the data contains the required column
  if (!all(c("epi_week", "count") %in% names(data))) {
    stop("The data frame must contain 'epi_week' and 'count' columns.")
  }
  
  # Apply the boxcar running average
  smoothed <- zoo::rollmean(data$count, k = nsmo, fill = NA)
  
  # If edge_truncate is TRUE, replace NA values with original values
  if (edge_truncate) {
    is_na <- is.na(smoothed)
    smoothed[is_na] <- data$count[is_na]
  }
  
  # Create a new data frame with the same labels but smoothed count values
  result <- data
  result$count <- smoothed
  
  return(result)
}

