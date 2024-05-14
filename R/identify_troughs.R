#' Identify the Onset of Exponential Rises by Derivative Analysis
#'
#' @param data A data frame containing a time series with `epi_week` and `count` columns.
#' @param nsmo The window size for the moving average to smooth the data, defaults to 9.
#' @return A ggplot object showing the time series with onsets marked.
#' @export
identify_troughs <- function(data, nsmo = 9) {
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

  # Calculate the first and second derivatives of the smoothed data
  data$first_derivative <- c(NA, diff(data$smoothed_count))
  data$second_derivative <- c(NA, diff(data$first_derivative))

  # Identify the troughs where the first derivative is zero and the second derivative is positive
  trough_points <- which(
    abs(data$first_derivative) < 1e-5 &  # Adjust sensitivity with this threshold
    data$second_derivative > 0 & 
    !is.na(data$first_derivative) & 
    !is.na(data$second_derivative)
  )
  
  trough_dates <- data$epi_week[trough_points]

  # Print the onset dates
  cat("Identified Trough Dates:\n")
  print(trough_dates)

  # Create a subset for plotting
  trough_data <- data[trough_points, ]

  # Create the plot
  plot_data <- ggplot(data, aes(x = epi_week, y = smoothed_count)) +
    geom_line() +
    geom_point(data = trough_data, aes(x = epi_week, y = smoothed_count), color = "red", size = 4) +
    geom_vline(xintercept = as.numeric(trough_dates), linetype = "dashed", color = "blue") +
    labs(title = "Norovirus Cases with Identified Onsets of Troughs",
         x = "Date",
         y = "Count") +
    theme_minimal()
  
  return(plot_data)
}
