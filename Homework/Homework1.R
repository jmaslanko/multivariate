library(dplyr)

data <- read.csv(file="~/CSU\ Files/Multivariate/TrackRecords.csv", header = TRUE, sep = ",")

# May need to multiply/divde cols to get consistent scale

dataCols <- colnames(data)


meanVector <- unname(colMeans(data[-1]))

sdVector <- unname(apply(data[-1], 2, sd))


