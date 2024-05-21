#' Plot Power Spectrum with Annotated Peaks
#'
#' @param data A data frame containing the norovirus data with a `count` column.
#' @param npeak The number of peaks to annotate, defaults to 2.
#' @return The ggplot object showing the power spectrum plot.
#' @export
plot_power_spectrum <- function(data, npeak = 2) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but is not installed.")
  }

  # Convert count data to a time series object
  count_ts <- as.numeric(data$count)

  # Compute the power spectrum
  spec <- stats::spec.pgram(count_ts, plot = FALSE)
  spec_df <- data.frame(
    frequency = spec$freq,
    spectrum = spec$spec
  )

  # Find the indices of the largest spectrum values
  top_n_indices <- order(spec_df$spectrum, decreasing = TRUE)[1:npeak]

  # Create the power spectrum plot with annotated peaks
  plot_ps <- ggplot2::ggplot(spec_df, ggplot2::aes(x = frequency, y = spectrum)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10() + 
    ggplot2::annotate("text", x = spec_df$frequency[top_n_indices] + 0.0333, 
                      y = spec_df$spectrum[top_n_indices] - 100,
                      label = paste0("Peak ", 1:npeak, ": ", round(1 / spec_df$frequency[top_n_indices], 1), " weeks"),
                      vjust = -1, color = "blue") +
    ggplot2::labs(title = "Power Spectrum",
                  x = expression(paste("Frequency (week"^-1, ")")),
                  y = "Spectrum") +
    ggplot2::theme_minimal()

  return(plot_ps)
}

