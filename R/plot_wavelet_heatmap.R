# Install and load necessary packages
# install.packages("WaveletComp")
# install.packages("ggplot2")
# library(WaveletComp)
# library(ggplot2)

# Define the function
plot_wavelet_heatmap <- function(time_series, dt = 1) {
  # Compute the wavelet transform

  # browser()

# Convert the time series vector into a data frame
  my_data <- data.frame(time_series)
colnames(my_data) <- "count"

# Compute the wavelet transform
  wavelet_result <- WaveletComp::analyze.wavelet(my_data,"count", dt = dt)

  # Extract the wavelet power spectrum
  power_spectrum <- wavelet_result$Power

  # Create a data frame for plotting
  time_index <- seq_along(time_series)
  frequency <- wavelet_result$Period
  power_df <- expand.grid(Time = time_index, Frequency = frequency)
  power_df$Power <- as.vector(t(power_spectrum))

  z_min = 0.0
  z_max = 1.0

  y_min = 0.0
  y_max = 10.0

  # Plot the heat map using ggplot2
  heatmap_plot <- ggplot(power_df, aes(x = Time, y = Frequency, fill = Power)) +
    geom_tile() +
    scale_fill_gradientn(colors = rev(heat.colors(256)), limits = c(z_min, z_max)) +
    scale_y_continuous(limits = c(y_min,y_max)) +  # Set y-axis range
    labs(title = "Wavelet Power Spectrum", x = "Time Index", y = "Frequency") +
    theme_minimal()

  # Print the plot
  print(heatmap_plot)
}
