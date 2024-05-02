#' Detect Change Points using Growth Rate Analysis and Plot Results
#'
#' @param data A data frame containing a time series of data with `epi_week` and `count` columns.
#' @param threshold The growth rate threshold for detecting change points, defaults to 0.5 (50% increase).
#' @return A ggplot object with change points marked, and prints the dates of change points.
#' @export
detect_change_points_growth_rate <- function(data, threshold = 0.5) {
  # Ensure required package is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but is not installed.")
  }
  
  # Load required library
  library(ggplot2)
  
  # Check that the data contains the required columns
  if (!all(c("epi_week", "count") %in% names(data))) {
    stop("The data frame must contain 'epi_week' and 'count' columns.")
  }

  # Calculate the daily growth rate
  data$growth_rate <- c(NA, diff(data$count) / lag(data$count))
  
  # Detect points where the growth rate exceeds the threshold
  change_points <- which(data$growth_rate > threshold)
  change_dates <- data$epi_week[change_points]

  # Print the change point dates
  cat("Change Points Detected at the following dates (Growth Rate > ", threshold, "):\n", sep = "")
  print(change_dates)

  # Create the plot
  plot_data <- ggplot(data, aes(x = epi_week, y = count)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(change_dates), linetype = "dashed", color = "red") +
    labs(title = "Time Series with Growth Rate Detected Change Points",
         x = "Date",
         y = "Count") +
    theme_minimal()
  
  return(plot_data)
}

