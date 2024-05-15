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
  plot_data <- data.frame(x = data4anal$epi_week, y_original = y, y_spline = y_spline)

  # Plotting the original data and the spline fit
  plot <- ggplot(plot_data, aes(x = x)) +
    geom_line(aes(y = y_original), color = "blue", size = 1,
              show.legend = TRUE, name = "Original Data") +
    geom_line(aes(y = y_spline), color = "red", size = 1,
              show.legend = TRUE, name = "Spline Fit") +
    labs(title = "Comparison of Original Data and Spline Fit",
         x = "Index", y = "Count") +
    theme_minimal()

  # ----------------------------------------------------------

  # add some onset lines:

  nweek_window1 <- 15
  nweek_window2 <- 30
  threshold     <- 1.45

  # Loop through the y_spline vector
  i <- 1
  while (i <= length(y_spline)) {
    # Define the window range
    start_index <- max(1, i - nweek_window1 + 1)
    end_index <- i

    # Calculate the local minimum within the window
    local_min <- min(y_spline[start_index:end_index])

    # Check if the current value is more than twice the local minimum
    if (y_spline[i] > threshold * local_min) {
      # Add a dotted green vertical line at the current index
      plot <- plot + geom_vline(xintercept = i, linetype = "solid", color = "green", size = 1)

      # Skip the next nweek_window data points
      i <- i + nweek_window2
    } else {
      # Move to the next data point
      i <- i + 1
    }
  }

  #------------------------------------------------------------

  print(plot)  # Display the plot

  # Return the modified dataframe
  return(data4anal)

}
