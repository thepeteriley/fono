moving_average <- function(x, n = 5) {
  # 'n' is the number of points to smooth over
  filter(rep(1/n, n), x, sides = 2)
}
