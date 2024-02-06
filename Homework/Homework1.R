library(dplyr)

data <- read.csv(file="/Users/Jeremy/Documents/GitHub/multivariate/data/TrackRecords.csv", header = TRUE, sep = ",")

# May need to multiply/divide cols to get consistent scale

# Questions from book 1.17
meanVector <- unname(colMeans(data[-1]))
sdVector <- unname(apply(data[-1], 2, sd))
R <- cor(data[-1])

# Spread of data increases the most for the marathon
boxplot(data[-1])

pairs(data[-1])

# View distributions
# Less normally distributed as the distance increases
hist(data$X100m..s.)
hist(data$X200m..s.)
hist(data$X400m..s.)
hist(data$X800m..m.)
hist(data$X1500m..m.)
hist(data$X3000m..m.)
hist(data$Marathon..m.)


