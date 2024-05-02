#' Plot SARIMA Forecasts at Specific Dates
#'
#' @param data A data frame containing the norovirus data with a `count` column and `epi_week` (Date type).
#' @param prediction_dates A vector of dates where forecasts should be made.
#' @param forecast_length The number of future data points to forecast (default is 12, for 12 weeks).
#' @return The ggplot object showing the forecast plot.
#' @export
plot_sarima_forecasts <- function(data, prediction_dates, forecast_length = 12) {
  # Ensure required packages are loaded
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package 'forecast' is required but is not installed.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but is not installed.")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required but is not installed.")
  }
  
  # Load required libraries
  library(forecast)
  library(ggplot2)
  library(lubridate)

  # Initialize the base plot with all observed data
  combined_data <- data.frame(epi_week = data$epi_week, count = data$count, Type = "Observed")
  plot_data <- ggplot(combined_data, aes(x = epi_week, y = count, color = Type)) +
    geom_line() +
    labs(title = "SARIMA Forecasts at Specific Dates",
         x = "Date",
         y = "Count") +
    theme_minimal()

  # Iterate through the prediction dates and add forecasts
  for (date in prediction_dates) {
    # Subset the data up to the current prediction date
    data_subset <- data[data$epi_week <= date, ]
    ts_data <- ts(data_subset$count, start = c(year(min(data_subset$epi_week)), week(min(data_subset$epi_week))), frequency = 52)

    # Fit a SARIMA model to the subset data
    sarima_model <- auto.arima(ts_data, seasonal = TRUE)
    
    # Forecast the next `forecast_length` points
    sarima_forecast <- forecast(sarima_model, h = forecast_length)

    # Create a data frame with forecasted values
    forecast_df <- data.frame(
      epi_week = seq(max(data_subset$epi_week) + 7, by = "week", length.out = forecast_length),
      forecast = sarima_forecast$mean,
      lower_95 = sarima_forecast$lower[,2],
      upper_95 = sarima_forecast$upper[,2]
    )

    # Add the forecast to the plot
    plot_data <- plot_data +
      geom_line(data = forecast_df, aes(x = epi_week, y = forecast), color = "red") +
      geom_ribbon(data = forecast_df, aes(x = epi_week, ymin = lower_95, ymax = upper_95), fill = "red", alpha = 0.2, inherit.aes = FALSE)
  }

  return(plot_data)
}

