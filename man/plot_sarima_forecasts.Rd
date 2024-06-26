% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_sarima_forecasts.R
\name{plot_sarima_forecasts}
\alias{plot_sarima_forecasts}
\title{Plot SARIMA Forecasts at Specific Dates}
\usage{
plot_sarima_forecasts(data, prediction_dates, forecast_length = 12)
}
\arguments{
\item{data}{A data frame containing the norovirus data with a `count` column and `epi_week` (Date type).}

\item{prediction_dates}{A vector of dates where forecasts should be made.}

\item{forecast_length}{The number of future data points to forecast (default is 12, for 12 weeks).}
}
\value{
The ggplot object showing the forecast plot.
}
\description{
Plot SARIMA Forecasts at Specific Dates
}
