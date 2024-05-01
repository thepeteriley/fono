#' Plot Norovirus Cases with a Running Average
#'
#' @param data A data frame containing the norovirus data, with `epi_week` and `count` columns.
#' @param nsmo The number of weeks to use for the running average, defaults to 9.
#' @param ytit The y-axis title for the plot, defaults to "Number of Cases".
#' @return A ggplot object
#' @export
#' @importFrom magrittr %>%
plot_cases_w_boxcar <- function(data, nsmo = 9, ytit = "Number of Cases") {
  # Check if required packages are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but is not installed.")
  }
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' is required but is not installed.")
  }

  # Create the plot


plot <- ggplot2::ggplot(data, ggplot2::aes(x = epi_week, y = count)) +
  ggplot2::geom_line() + # Line plot
  ggplot2::geom_line(data = data %>%
  dplyr::mutate(ra = zoo::rollmean(count, k = nsmo, fill = NA)),
  ggplot2::aes(y = ra), color = "blue") + # Adding a running average line
  ggplot2::labs(title = "Norovirus cases for Polk County, Fl",
  x = "Epidemiological Week",
  y = ytit,
  caption = "Data plotted by epidemiological week") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) # Improve x-axis labels visibility

  return(plot)
}

