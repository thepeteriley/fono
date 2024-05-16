capture_clicks_with_dates <- function() {
  # Your existing setup for the plot
  
  # Capture the clicks
  points <- locator(type = "n")

  # Convert x-coordinates from numeric dates to Date or POSIXct
  if (!is.null(points$x)) {
    points$date <- as.Date(points$x, origin = "1970-01-01")
    points$datetime <- as.POSIXct(points$x * 86400, origin = "1970-01-01", tz = "UTC")
    
    cat("You selected the following points (dates and times):\n")
    print(data.frame(Date = points$date, DateTime = points$datetime, Y = points$y))
  } else {
    cat("No points were captured or operation was cancelled.\n")
  }
  
  return(data.frame(Date = points$date, DateTime = points$datetime, Y = points$y))
}

