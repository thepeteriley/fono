# Driver program to analyse, visualise, and perform basic forecasting of
# norovirus datasets

data <- read_norovirus_data(main_sheet_index = 2)

# Plot the data with a smoothed line

nsmo = 9
plot <- plot_cases_w_boxcar(data, nsmo = nsmo, ytit = "Cases")
print(plot)

#-----------------------------------------------------------------------------

# Now calculate R0 assumming exponential growth between t1 and t2

t1 <- as.Date("2017-07-24")
t2 <- as.Date("2017-12-15")
R0 <- calc_r0(t1, t2, data)
R0_text <- sprintf("R0 = %.2f", R0)

#-----------------------------------------------------------------------------

# Plotting with vertical lines at t1 and t2
y_max = 30
annotated_plot <- add_annotations(plot, t1, t2, R0_text, annotate_date = t1+90, y_max)
print(annotated_plot)

#-----------------------------------------------------------------------------

# add another R0 calculation

t1 <- as.Date("2018-09-01")
t2 <- as.Date("2018-12-01")
R0 <- calc_r0(t1, t2, data)
R0_text <- sprintf("R0 = %.2f", R0)
y_max = 30
annotated_plot2 <- add_annotations(annotated_plot, t1, t2, R0_text, annotate_date = t1+30, y_max)
print(annotated_plot2)

#-----------------------------------------------------------------------------


# compute the power spectrum

plot_ps <- plot_power_spectrum(data, npeak = 3)  # Adjust npeak as needed
print(plot_ps)

#-----------------------------------------------------------------------------

# Compute SARIMA model and plot it

plot <- plot_sarima_forecast(data, forecast_length = 12)
print(plot)


#-----------------------------------------------------------------------------

# Compute SARIMA model for a set of dates in the time  series and plot them

# let's smooth the data first

smoothed_data <- apply_boxcar_average(data, nsmo = 9, edge_truncate=TRUE)

prediction_dates <- as.Date(c("2017-07-01", "2018-01-01", "2018-07-01"))
prediction_dates <- as.Date(c("2017-03-01", "2017-10-01", "2018-04-01","2018-12-01"))
plot <- plot_sarima_forecasts(smoothed_data, prediction_dates, forecast_length = 12)
print(plot)

#-----------------------------------------------------------------------------

# apply the CUSUM method to identify change points in the data
# there's something wrong in the logic here...NOT WORKING!

plot <- detect_change_points_cusum(smoothed_data, threshold = 3)
print(plot)

#-----------------------------------------------------------------------------

# try several growth rate calculations:

# Example usage of the function with a threshold of 20% for a more noticeable increase
plot <- detect_growth_rate_changes(smoothed_data, threshold = 20)
print(plot)

plot <- detect_change_points_growth_rate(smoothed_data, threshold = 0.5)
print(plot)

#-----------------------------------------------------------------------------

# Try the PELT method...

plot <- detect_change_points_pelt(smoothed_data, penalty = "Manual", pen.value = 6)
print(plot)



