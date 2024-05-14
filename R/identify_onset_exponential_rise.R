#' Identify the Onset of Exponential Rises in Seasonal Peaks
#'
#' @param data A data frame containing a time series with `epi_week` and `count` columns.
#' @param nsmo The window size for the moving average to smooth the data, defaults to 9.
#' @param threshold_multiplier Multiplier for setting the threshold based on the standard deviation, defaults to 1.
#' @return A ggplot object showing the time series with onsets marked.
#' @export
identify_onset_exponential_rise <- function(data, nsmo = 9, threshold_multiplier = 1) {
  # Ensure required packages are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but is not installed.")
  }
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' is required but is not installed.")
  }
  
  # Load required libraries
  library(ggplot2)
  library(zoo)
  
  # Check that the data contains the required columns
  if (!all(c("epi_week", "count") %in% names(data))) {
    stop("The data frame must contain 'epi_week' and 'count' columns.")
  }

  # Apply a moving average to smooth the data
  data$smoothed_count <- rollmean(data$count, nsmo, fill = NA, align = "center")

  # Calculate the first derivative of the smoothed data
  data$rate_change <- c(NA, diff(data$smoothed_count))

  # Calculate the second derivative to find acceleration points
  data$acceleration = c(NA, diff(data$rate_change))

  # Determine a threshold for significant increases based on the historical variability of the rate change
  stdev_threshold = mean(data$rate_change, na.rm = TRUE) + threshold_multiplier * sd(data$rate_change, na.rm = TRUE)

  # Identify the points where the acceleration of the rate change is positive and exceeds the threshold
  onset_points <- which(data$acceleration > 0 & data$rate_change > stdev_threshold & !is.na(data$acceleration))
  onset_dates <- data$epi_week[onset_points]

  # Print the onset dates
  cat("Identified Onset Dates:\n")
  print(onset_dates)

  # Create the plot
  plot_data <- ggplot(data, aes(x = epi_week, y = smoothed_count)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(onset_dates), linetype = "dashed", color = "red") +
    labs(title = "Norovirus Cases with Identified Onsets of Exponential Rise",
         x = "Date",
         y = "Count") +
    theme_minimal()
  
  return(plot_data)
}

