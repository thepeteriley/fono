capture_clicks <- function() {
  # Check if the plot window is open and if not, create a basic plot
  if (dev.cur() == 1) {  # No graphics device is open
    plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "X-axis", ylab = "Y-axis")
    message("Created a default plot because no plot was open.")
  }
  
  # Inform the user
  cat("Click on the plot to select points.\n")
  cat("Right-click to stop selecting points (or press ESC, depending on your system).\n")
  
  # Capture the clicks, without specifying 'n'
  points <- locator(type = "n")  # No 'n' argument, it continues until user terminates

  # Check if points were captured
  if (!is.null(points$x)) {
    cat("You selected the following points:\n")
    print(data.frame(x = points$x, y = points$y))
  } else {
    cat("No points were captured or operation was cancelled.\n")
  }
  
  # Return the points data frame
  return(data.frame(x = points$x, y = points$y))
}

