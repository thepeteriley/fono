#' Add Vertical Lines and Text Annotation to a Plot
#'
#' @param plot An existing ggplot object to annotate
#' @param t1 The starting epidemiological week (as a Date)
#' @param t2 The ending epidemiological week (as a Date)
#' @param R0_text Text to annotate the plot with the estimated R0 value.
#' @param annotate_date The date where the R0 annotation should be placed
#' @param y_max The maximum value of y-axis to position the annotation
#' @return The modified ggplot object
#' @export
add_annotations <- function(plot, t1, t2, R0_text, annotate_date = as.Date("2017-10-01"), y_max) {
  # Add vertical lines and text annotation to the existing plot
  plot <- plot +
    ggplot2::geom_vline(xintercept = as.numeric(t1), linetype = "dashed", color = "red") +
    ggplot2::geom_vline(xintercept = as.numeric(t2), linetype = "dashed", color = "blue") +
    ggplot2::annotate("text", x = annotate_date, y = y_max * 0.8, label = R0_text, color = "darkgreen", size = 5)

  return(plot)
}

