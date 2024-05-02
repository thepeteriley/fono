#' Apply a Boxcar Running Average to Data
#'
#' @param data A data frame containing the time series data with a `count` column.
#' @param nsmo The number of points to include in the moving average window, defaults to 3.
#' @return A data frame with the original data and an additional `smoothed_count` column containing the smoothed data.
#' @export
apply_boxcar_average <- function(data, nsmo = 3) {
  # Ensure required package is loaded
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' is required but is not installed.")
  }
  
  # Load required library
  library(zoo)
  
  # Check that the data contains the required column
  if (!"count" %in% names(data)) {
    stop("The data frame must contain a 'count' column.")
  }
  
  # Apply the boxcar running average
  data$smoothed_count <- zoo::rollmean(data$count, k = nsmo, fill = NA)
  
  return(data)
}

