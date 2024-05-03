#' Detect Change Points using PELT and Plot Results
#'
#' @param data A data frame containing a time series of data with `epi_week` and `count` columns.
#' @param penalty The type of penalty used in the PELT method to avoid overfitting, defaults to "BIC".
#' @param pen.value A numeric value used to adjust the sensitivity of change point detection.
#' @return A ggplot object with change points marked, and prints the dates of change points.
#' @export
detect_change_points_pelt <- function(data, penalty = "BIC", pen.value = NULL) {
  # Ensure required packages are installed
  if (!requireNamespace("changepoint", quietly = TRUE)) {
    stop("Package 'changepoint' is required but is not installed.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but is not installed.")
  }
  
  # Load required libraries
  library(changepoint)
  library(ggplot2)
  
  # Check that the data contains the required columns
  if (!all(c("epi_week", "count") %in% names(data))) {
    stop("The data frame must contain 'epi_week' and 'count' columns.")
  }

  # Apply the PELT method to detect change points
  pelt_result <- cpt.mean(data$count, method = "PELT", penalty = penalty, pen.value = pen.value)
  change_points <- cpts(pelt_result)
  change_dates <- data$epi_week[change_points]

  # Print the change point dates
  cat("Change Points Detected at the following dates:\n")
  print(change_dates)

  # Create the plot
  plot_data <- ggplot(data, aes(x = epi_week, y = count)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(change_dates), linetype = "dashed", color = "red") +
    labs(title = "Time Series with PELT Detected Change Points",
         x = "Date",
         y = "Count") +
    theme_minimal()
  
  return(plot_data)
}

