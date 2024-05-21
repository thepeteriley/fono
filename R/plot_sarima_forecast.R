#' Plot SARIMA Forecast with Confidence Intervals
#'
#' @param data A data frame containing the norovirus data with a `count` column and `epi_week` (Date type).
#' @param forecast_length The number of future data points to forecast (default is 12, for 12 weeks).
#' @return The ggplot object showing the forecast plot.
#' @export
plot_sarima_forecast <- function(data, forecast_length = 12) {
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
  
  # Convert data to a time series object with weekly frequency
  start_date <- min(data$epi_week)
  ts_data <- ts(data$count, start = c(year(start_date), week(start_date)), frequency = 52)
  
  # Fit a SARIMA model to the data
  sarima_model <- auto.arima(ts_data, seasonal = TRUE)
 
 print(summary(sarima_model))

  # Forecast the next `forecast_length` points (weeks)
  sarima_forecast <- forecast(sarima_model, h = forecast_length)
  
  # Create a data frame with forecasted values and confidence intervals
  forecast_df <- data.frame(
    epi_week = seq(max(data$epi_week) + 7, by = "week", length.out = forecast_length),
    forecast = sarima_forecast$mean,
    lower_95 = sarima_forecast$lower[,2],
    upper_95 = sarima_forecast$upper[,2]
  )
  
  # Combine original data and forecast for plotting
  combined_data <- rbind(
    data.frame(epi_week = data$epi_week, count = data$count, Type = "Observed"),
    data.frame(epi_week = forecast_df$epi_week, count = forecast_df$forecast, Type = "Forecast")
  )
  
  # Plot the data and the forecast with confidence intervals
  plot_data <- ggplot(combined_data, aes(x = epi_week, y = count, color = Type)) +
    geom_line() +
    geom_ribbon(data = forecast_df, aes(x = epi_week, ymin = lower_95, ymax = upper_95), fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
    labs(title = "SARIMA Forecast with Confidence Intervals",
         x = "Date",
         y = "Count") +
    theme_minimal()
  
  return(plot_data)
}

