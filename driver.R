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

