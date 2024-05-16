# Ensure the zoo package is available for moving average
if (!require(zoo)) install.packages("zoo", dependencies=TRUE)
library(zoo)

# Function to calculate moving average using rollmean from the zoo package
moving_average <- function(x, n = 5) {
  # 'n' is the number of points to smooth over
  # Use 'rollmean' with fill = NA to handle edges
  ma <- rollmean(x, n, fill = NA, align = 'center')
  return(ma)
}

