#' Detect Change Points using CUSUM and Plot Results
#'
#' @param data A data frame containing a time series of data with `epi_week` and `count` columns.
#' @param threshold The CUSUM threshold value, initially set to a reasonable value, defaults to 5.
#' @return A ggplot object with change points marked, and prints a summary of the CUSUM analysis.
#' @export
detect_change_points_cusum <- function(data, threshold = 5) {
  # Ensure required packages are installed
  if (!requireNamespace("qcc", quietly = TRUE)) {
    stop("Package 'qcc' is required but is not installed.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but is not installed.")
  }
  
  # Load required libraries
  library(qcc)
  library(ggplot2)
  
  # Check that the data contains the required columns
  if (!all(c("epi_week", "count") %in% names(data))) {
    stop("The data frame must contain 'epi_week' and 'count' columns.")
  }
  
  # Apply CUSUM
  cusum_result <- qcc::cusum(data$count, decision.interval = threshold, se.shift = 1)

  # Identify change points
  change_points <- which(cusum_result$cusum_pos > threshold | cusum_result$cusum_neg < -threshold)
  change_dates <- data$epi_week[change_points]
  
  # Print the CUSUM summary and change point dates
  cat("CUSUM Analysis Summary:\n")
  print(summary(cusum_result))
  cat("\nChange Points Detected at the following dates:\n")
  print(change_dates)
  
  # Create the plot
  plot_data <- ggplot(data, aes(x = epi_week, y = count)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(change_dates), linetype = "dashed", color = "red") +
    labs(title = "Time Series with CUSUM Detected Change Points",
         x = "Date",
         y = "Count") +
    theme_minimal()
  
  return(plot_data)
}

