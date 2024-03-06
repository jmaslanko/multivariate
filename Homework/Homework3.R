
library(car)

# Question 3
# a
S <- matrix(c(5, 4, 4, 5), nrow = 2)
xbar<-apply(S,2,mean)
Sigma<-var(S)

par(bg='white')


plot(0, 0, type = "n", xlim = c(-8, 8), ylim = c(-8, 8), xlab = "X", ylab = "Y")
ellipse(center = xbar, shape = S, radius = 1, col = "blue", border = "blue", lwd = 2)

# b
S <- matrix(c(5, -4, -4, 5), nrow = 2)
xbar<-apply(S,2,mean)
Sigma<-var(S)

par(bg='white')


plot(0, 0, type = "n", xlim = c(-8, 8), ylim = c(-8, 8), xlab = "X", ylab = "Y")
ellipse(center = xbar, shape = S, radius = 1, col = "blue", border = "blue", lwd = 2)

# c
S <- matrix(c(3, 0, 0, 3), nrow = 2)
xbar<-apply(S,2,mean)
Sigma<-var(S)

par(bg='white')


plot(0, 0, type = "n", xlim = c(-8, 8), ylim = c(-8, 8), xlab = "X", ylab = "Y")
ellipse(center = xbar, shape = S, radius = 1, col = "blue", border = "blue", lwd = 2)

