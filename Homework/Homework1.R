library(dplyr)
library(ggplot2)
data <- read.csv(file="/Users/Jeremy/Documents/GitHub/multivariate/data/TrackRecords.csv", header = TRUE, sep = ",")

# May need to multiply/divide cols to get consistent scale

# Questions from book 1.17
meanVector <- unname(colMeans(data[-1]))
sdVector <- unname(apply(data[-1], 2, sd))
R <- cor(data[-1])

summary(data[-1])
# Spread of data increases the most for the marathon
boxplot(data[-1], main="Figure 1: Box and Whisker")

pairs(data[-1], main="Figure 2: Correlation Matrix")

# View distributions
# Less normally distributed as the distance increases
hist(data$X100m..s.)
hist(data$X200m..s.)
hist(data$X400m..s.)
hist(data$X800m..m.)
hist(data$X1500m..m.)
hist(data$X3000m..m.)
hist(data$Marathon..m.)

par(mfrow = c(1, 2))
plot(density(data$X100m..s.), main="Density plot for 100m")
plot(density(data$Marathon..m.), main="Density plot for Marathon")

