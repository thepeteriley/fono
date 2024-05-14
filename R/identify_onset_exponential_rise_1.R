#' Identify the Onset of Exponential Rises in Seasonal Peaks with Smoothing
#'
#' @param data A data frame containing a time series with `epi_week` and `count` columns.
#' @param nsmo The window size for the moving average to smooth the data, defaults to 9.
#' @param threshold The threshold for the rate of change to detect an onset, automatically set based on the smoothed data.
#' @return A ggplot object showing the time series with onsets marked.
#' @export
identify_onset_exponential_rise_1 <- function(data, nsmo = 9, threshold = NULL) {
  # Ensure required package is installed
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
  data$smoothed_count <- rollmean(data$count, nsmo, fill = NA, align = "right")

  # Calculate the rate of change (first derivative) on the smoothed data
  data$rate_change <- c(NA, diff(data$smoothed_count))

  # Calculate threshold if not provided
  if (is.null(threshold)) {
    threshold <- mean(data$rate_change, na.rm = TRUE) + 2 * sd(data$rate_change, na.rm = TRUE)
  }

  # Identify the points where the rate of change exceeds the threshold
  onset_points <- which(data$rate_change > threshold & !is.na(data$rate_change))
  onset_dates <- data$epi_week[onset_points]

  # Print the onset dates
  cat("Identified Onset Dates:\n")
  print(onset_dates)

  # Create the plot
  plot_data <- ggplot(data, aes(x = epi_week, y = count)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(onset_dates), linetype = "dashed", color = "red") +
    labs(title = "Norovirus Cases with Identified Onsets of Exponential Rise",
         x = "Date",
         y = "Count") +
    theme_minimal()
  
  return(plot_data)
}

