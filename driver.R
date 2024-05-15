# Driver program to analyse, visualise, and perform basic forecasting of
# norovirus datasets

library(devtools)
load_all("/Users/pete/Dropbox/shared/CSMB03/norovirus-fl/fono")

data1 <- read_norovirus_data(dataset =1, main_sheet_index = 2)
data2 <- read_norovirus_data(dataset =2, main_sheet_index = 1)

data4anal = data1

# Plot the data with a smoothed line

nsmo = 9
plot <- plot_cases_w_boxcar(data4anal, nsmo = nsmo, ytit = global_ytit)
print(plot)

#-----------------------------------------------------------------------------

# Now calculate R0 assumming exponential growth between t1 and t2

t1 <- as.Date("2017-07-24")
t2 <- as.Date("2017-12-15")
R0 <- calc_r0(t1, t2, data4anal)
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
R0 <- calc_r0(t1, t2, data4anal)
R0_text <- sprintf("R0 = %.2f", R0)
annotated_plot2 <- add_annotations(annotated_plot, t1, t2, R0_text, annotate_date = t1+30, y_max)
print(annotated_plot2)

#-----------------------------------------------------------------------------

# add  third R0 calculation

t1 <- as.Date("2022-10-15")
t2 <- as.Date("2023-03-01")
R0 <- calc_r0(t1, t2, data4anal)
R0_text <- sprintf("R0 = %.2f", R0)
annotated_plot3 <- add_annotations(annotated_plot2, t1, t2, R0_text, annotate_date = t1+30, y_max)
print(annotated_plot3)

#-----------------------------------------------------------------------------


# compute the power spectrum

plot_ps <- plot_power_spectrum(data4anal, npeak = 3)  # Adjust npeak as needed
print(plot_ps)

#-----------------------------------------------------------------------------

# Compute SARIMA model and plot it

plot <- plot_sarima_forecast(data4anal, forecast_length = 12)
print(plot)

#-----------------------------------------------------------------------------

# Compute SARIMA model for a set of dates in the time  series and plot them

# let's smooth the data first

smoothed_data <- apply_boxcar_average(data4anal, nsmo = 9, edge_truncate=TRUE)

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

plot <- detect_change_points_pelt(smoothed_data, penalty = "Manual", pen.value = 24)
print(plot)

#-----------------------------------------------

plot_rise_1 <- identify_onset_exponential_rise_1(data4anal, nsmo = 9)
print(plot_rise_1)

#-----------------------------------------------

plot <- identify_onset_exponential_rise(data4anal, nsmo = 9, threshold_multiplier = 2)
print(plot)

#-----------------------------------------------

# something even simpler: we'll take two pieces of information:
# first, where the first derivative is zero is where there is either
# a peak or a trough, but this must be coupled with the sign of the
# second derivative. If the sign is positive, then it means that it is a trough.
# These are the only points that we want to identify.

plot <- identify_troughs(data4anal, nsmo = 24)
print(plot)

#-----------------------------------------------

# do a spline fit

nspline = round(nrow(data4anal)/100)*25

# Fit spline with specified number of splines (e.g., 5)
data4anal_rev <- fit_spline(data4anal, num_splines = nspline)

#-----------------------------------------------
