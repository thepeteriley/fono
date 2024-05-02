#' Detect Sharp Increases in Growth Rate and Plot Results
#'
#' @param data A data frame containing a time series of data with `epi_week` and `count` columns.
#' @param threshold The percentage growth rate threshold to identify sharp increases, defaults to 10 (10% increase).
#' @return A ggplot object with change points marked, and prints a summary of the growth rate analysis.
#' @export
detect_growth_rate_changes <- function(data, threshold = 10) {
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

  # Calculate growth rate as percentage change
  data$growth_rate <- c(NA, diff(data$count) / lag(data$count) * 100)

  # Identify points where growth rate exceeds the threshold
  change_points <- which(data$growth_rate > threshold)
  change_dates <- data$epi_week[change_points]

  # Print summary of growth rate analysis
  cat("Growth Rate Analysis Summary:\n")
  cat("Change Points Detected at the following dates:\n")
  print(change_dates)

  # Create the plot
  plot_data <- ggplot(data, aes(x = epi_week, y = count)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(change_dates), linetype = "dashed", color = "red") +
    labs(title = "Time Series with Detected Growth Rate Changes",
         x = "Date",
         y = "Count",
         subtitle = paste("Threshold:", threshold, "%")) +
    theme_minimal()
  
  return(plot_data)
}

