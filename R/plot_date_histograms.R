library(ggplot2)
library(lubridate)

plot_date_histograms <- function(date_min, date_max) {
  # Convert date strings to Date objects
  date_min <- as.Date(date_min)
  date_max <- as.Date(date_max)
  
  # Convert dates to day of the year
  day_of_year_min <- yday(date_min)
  day_of_year_max <- yday(date_max)
  
  # Combine into a data frame for ggplot
  data_min <- data.frame(DayOfYear = day_of_year_min, Type = "Min")
  data_max <- data.frame(DayOfYear = day_of_year_max, Type = "Max")
  data_all <- rbind(data_min, data_max)
  
  # Plot using ggplot2
  p <- ggplot(data_all, aes(x = DayOfYear, fill = Type)) +
    geom_histogram(position = "identity", alpha = 0.5, binwidth = 36) +  # Adjust binwidth to suit data density
    scale_fill_manual(values = c("Min" = "blue", "Max" = "red")) +
    labs(title = "Histogram of Day of Year for Date Min and Max",
         x = "Day of Year", y = "Frequency") +
    theme_minimal()
  
  print(p)
}

