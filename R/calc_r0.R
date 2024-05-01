#' Calculate R0 for Norovirus
#'
#' @param t1 The starting epidemiological week (as a Date)
#' @param t2 The ending epidemiological week (as a Date)
#' @param data A data frame with 'epi_week' and 'count' columns
#' @return The calculated R0 value
#' @export
calc_r0 <- function(t1, t2, data) {
  # Check if required packages are installed
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but is not installed.")
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package 'stats' is required but is not installed.")
  }

  # Filter data to the specified time period
  data_subset <- dplyr::filter(data, epi_week >= t1 & epi_week <= t2)
  
  # Prepare time and cases for R0 calculation
  data_subset <- data_subset %>% dplyr::mutate(time = as.numeric(epi_week - min(epi_week)) / 7)  # Convert to weeks
  log_cases <- log(data_subset$count)
  fit <- stats::lm(log_cases ~ time, data = data_subset)

  # Extract the estimated growth rate and calculate R0
  growth_rate <- coef(fit)['time']
  G <- 5  # Generation interval in weeks for norovirus
  R0 <- 1 + growth_rate * G
  
  # Format R0 for display
  R0_text <- sprintf("R0 = %.2f", R0)
  print(R0_text)
  
  return(R0)
}

