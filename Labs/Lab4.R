library(mvtnorm)

## define bivariate normal distribution parameters
sigma <- matrix(c(4,2,2,3), ncol=2)
mu <- c(1,2)

## create grids of X1 and X2 values on X-Y plane.
x <- seq(-6,6,0.1); n<-length(x)