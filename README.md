# fono
Forecasting the Onset of Norovirus Outbreaks

This R package takes as input a CSV file with two columns: epidemiological week and number of cases and performs the following tasks: 
1. Processes the data, removing obvious noise, and, optionally smoothing the data
2. Plots the data, identifying regions of exponential growth, for which it computes effective R0 estimates
3. Estimates 'onset' times both retrospectively and forward-looking.
4. Computes confidence bounds on the onset estimates.

The input CSV file can optionally have multiple sheets. One sheet must be identified as the 'primary'. Correlation analysis can be performed with datasets on the other sheets. 

