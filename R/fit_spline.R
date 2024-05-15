#' Fit a Spline to a Data Frame
#'
#' @param data4anal A data frame with at least two columns: the first column representing the x-values and the second column representing the y-values.
#' @param num_splines The number of splines to fit (optional). If not specified, the function uses the default degrees of freedom.
#' @return A data frame with the original x-values and the corresponding spline-fitted y-values.
#' @export
#' @importFrom splines smooth.spline
fit_spline <- function(data4anal, num_splines = NULL) {
  # Ensure the data frame has at least two columns (x and y)
  if (ncol(data4anal) < 2) {
    stop("The input dataframe must have at least two columns: x and y.")
  }
  
  # Extract x and y values from the data frame
  x <- data4anal[[1]]
  y <- data4anal[[2]]
  
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
  
  # Create a data frame with the original x values and the spline-fitted y values
  spline_data <- data.frame(x = x, y_spline = y_spline)
  
  return(spline_data)
}
