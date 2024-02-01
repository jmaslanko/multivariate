
# Part 1
x_1 <- c(9, 2, 6, 5, 8)
x_2 <- c(12, 8, 6, 4, 10)
x_3 <- c(3, 4, 0, 2, 1)

m <- matrix(c(x_1, x_2, x_3), ncol = 3)

x_bar <- colMeans(m)

s_n <- apply(m, 2, sd)

R <- cor(m)

# Part 2
data <- read.csv("/Users/Jeremy/Documents/GitHub/multivariate/data/Cereal.csv")
