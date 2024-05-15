# Fit a Spline to a Data Frame with Equally Spaced X-values
#'
#' @param data4anal A data frame with at least one column: "count" for the y-values.
#' @param num_splines The number of splines to fit (optional). If not specified, the function uses the default degrees of freedom.
#' @return A plot of the original data and the spline-fitted data.
#' @export
#' @importFrom splines smooth.spline
#' @importFrom ggplot2 ggplot, geom_line, labs
library(ggplot2)  # Load ggplot2 for plotting

fit_spline <- function(data4anal, num_splines = NULL) {
  # Ensure the data frame has the required column
  if (!"count" %in% colnames(data4anal)) {
    stop('The input dataframe must have a column named "count".')
  }

  # Extract y values from the data frame
  y <- data4anal$count

  # Generate equally spaced x values (indices)
  x <- seq_along(y)

  # Create the spline fit
  if (is.null(num_splines)) {
    # Use a natural spline fit with default degrees of freedom
    spline_fit <- smooth.spline(x, y)
  } else {
    # Use a natural spline fit with specified degrees of freedom (number of splines)
    spline_fit <- smooth.spline(x, y, df = num_splines)
  }

  # Predict the spline values at the original x values
  y_spline <- predict(spline_fit, x)$y

  # Add spline-fitted values to the original data frame
  data4anal$count_spline <- y_spline

  # Combine original data and spline data into a single data frame for plotting
  plot_data <- data.frame(x = x, y_original = y, y_spline = y_spline)

  # Plotting the original data and the spline fit
  plot <- ggplot(plot_data, aes(x = x)) +
    geom_line(aes(y = y_original), color = "blue", size = 1, linetype = "dotted",
              show.legend = TRUE, name = "Original Data") +
    geom_line(aes(y = y_spline), color = "red", size = 1,
              show.legend = TRUE, name = "Spline Fit") +
    labs(title = "Comparison of Original Data and Spline Fit",
         x = "Index", y = "Count") +
    theme_minimal()

  print(plot)  # Display the plot

  # Return the modified dataframe
  return(data4anal)

}
