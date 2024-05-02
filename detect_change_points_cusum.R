#' Detect Change Points using CUSUM and Plot Results
#'
#' @param data A data frame containing a time series of data with `epi_week` and `count` columns.
#' @param threshold The CUSUM threshold value, initially set to a reasonable value, defaults to 5.
#' @return A ggplot object with change points marked, and prints a summary of the CUSUM analysis.
#' @export
detect_change_points_cusum <- function(data, threshold = 5) {
  # Ensure required package is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but is not installed.")
  }
  
  # Load required libraries
  library(ggplot2)
  
  # Check that the data contains the required columns
  if (!all(c("epi_week", "count") %in% names(data))) {
    stop("The data frame must contain 'epi_week' and 'count' columns.")
  }

  # Initialize CUSUM statistics
  mean_count <- mean(data$count)
  cusum_pos <- cusum_neg <- rep(0, nrow(data))
  change_points <- c()

  # Calculate CUSUM statistics
  for (i in seq(2, nrow(data))) {
    cusum_pos[i] <- max(0, cusum_pos[i - 1] + (data$count[i] - mean_count))
    cusum_neg[i] <- min(0, cusum_neg[i - 1] + (data$count[i] - mean_count))

    # Check for change points
    if (cusum_pos[i] > threshold | cusum_neg[i] < -threshold) {
      change_points <- c(change_points, i)
      cusum_pos[i] <- cusum_neg[i] <- 0
    }
  }

  change_dates <- data$epi_week[change_points]

  # Print the CUSUM summary and change point dates
  cat("CUSUM Analysis Summary:\n")
  cat("Change Points Detected at the following dates:\n")
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

