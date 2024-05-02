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
t2 <- as.Date("2017-12-25")
R0 <- calc_r0(t1, t2, data)
R0_text <- sprintf("R0 = %.2f", R0)

#-----------------------------------------------------------------------------

# Plotting with vertical lines at t1 and t2
y_max = 30
annotated_plot <- add_annotations(plot, t1, t2, R0_text, annotate_date = as.Date("2017-10-01"), y_max)
print(annotated_plot)

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
plot <- plot_sarima_forecasts(smoothed_data, prediction_dates, forecast_length = 12)
print(plot)

#-----------------------------------------------------------------------------



