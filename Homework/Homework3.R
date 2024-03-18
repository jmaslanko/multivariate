library(car)

################
## Question 2 ##
################

# Generate 1000 random samples from normal distribution
x1 <- rnorm(1000, mean = 0, sd = 1)

# Plot histogram of random samples
hist(x1, breaks = 30, main = "Histogram of Random Samples",
     xlab = "Random Samples", ylab = "Frequency", col = "skyblue", border = "white")

x2 <- ifelse(abs(x1) < 1, -x1, x1)
hist(x2, breaks = 30, main = "Histogram of Random Samples",
     xlab = "Random Samples", ylab = "Frequency", col = "skyblue", border = "white")


# Plot joint distribution
plot(x1, x2, 
     xlab = "x1", ylab = "x2", 
     main = "Joint Distribution of Two Samples", col = "blue", pch = 19)

################
## Question 3 ##
################
# a
S <- matrix(c(5, 4, 4, 5), nrow = 2)
xbar<-apply(S,2,mean)
Sigma<-var(S)

e <- eigen(S)
eigenvalues <- e$values
eigenvectors <- e$vectors


par(bg='white')


plot(0, 0, type = "n", xlim = c(0, 8), ylim = c(0, 8), xlab = "X", ylab = "Y")
ellipse(center = xbar, shape = S, radius = 1, col = "blue", border = "blue", lwd = 2)
arrows(xbar[1],xbar[2],xbar[1]+e$vectors[1,1]*sqrt(e$values[1]),xbar[2]+e$vectors[2,1]*sqrt(e$values[1]),length=.1,col='red',lwd=2)
arrows(xbar[1],xbar[2],xbar[1]+e$vectors[1,2]*sqrt(e$values[2]),xbar[2]+e$vectors[2,2]*sqrt(e$values[2]),length=.1,col='red',lwd=2)

legend("topleft", legend = paste("Radius", 1:length(eigenvalues), ": ", round(sqrt(eigenvalues), 2)), 
       col = "red", lty = 1, cex = 0.8, bg = "white")
title("Matrix 1")

# b
S <- matrix(c(5, -4, -4, 5), nrow = 2)
xbar<-apply(S,2,mean)
Sigma<-var(S)

e <- eigen(S)
eigenvalues <- e$values
eigenvectors <- e$vectors

par(bg='white')


plot(0, 0, type = "n", xlim = c(-4, 4), ylim = c(-4, 4), xlab = "X", ylab = "Y")
ellipse(center = xbar, shape = S, radius = 1, col = "blue", border = "blue", lwd = 2)
arrows(xbar[1],xbar[2],xbar[1]+e$vectors[1,1]*sqrt(e$values[1]),xbar[2]+e$vectors[2,1]*sqrt(e$values[1]),length=.1,col='red',lwd=2)
arrows(xbar[1],xbar[2],xbar[1]+e$vectors[1,2]*sqrt(e$values[2]),xbar[2]+e$vectors[2,2]*sqrt(e$values[2]),length=.1,col='red',lwd=2)

legend("topleft", legend = paste("Radius", 1:length(eigenvalues), ": ", round(sqrt(eigenvalues), 2)), 
       col = "red", lty = 1, cex = 0.8, bg = "white")
title("Matrix 2")

# c
S <- matrix(c(3, 0, 0, 3), nrow = 2)
xbar<-apply(S,2,mean)
Sigma<-var(S)

e <- eigen(S)
eigenvalues <- e$values
eigenvectors <- e$vectors

par(bg='white')


plot(0, 0, type = "n", xlim = c(-4, 4), ylim = c(-4, 4), xlab = "X", ylab = "Y")
ellipse(center = xbar, shape = S, radius = 1, col = "blue", border = "blue", lwd = 2)

arrows(xbar[1],xbar[2],xbar[1]+e$vectors[1,1]*sqrt(e$values[1]),xbar[2]+e$vectors[2,1]*sqrt(e$values[1]),length=.1,col='red',lwd=2)
arrows(xbar[1],xbar[2],xbar[1]+e$vectors[1,2]*sqrt(e$values[2]),xbar[2]+e$vectors[2,2]*sqrt(e$values[2]),length=.1,col='red',lwd=2)

legend("topleft", legend = paste("Radius", 1:length(eigenvalues), ": ", round(sqrt(eigenvalues), 2)), 
       col = "red", lty = 1, cex = 0.8, bg = "white")

title("Matrix 3")


################
## Question 5 ##
################

library(ggplot2)
library(dplyr)
library(tidyr)
library(gplots)

data <- read.csv("/Users/Jeremy/Documents/GitHub/multivariate/data/air_pollution.csv")
x <- data[,2:8]
#x <- as.matrix(x)
matrix <- as.matrix(x)

summary(matrix)
# box plots
par(mfrow = c(2, 4))
boxplot(matrix[,1],main="Wind")
boxplot(matrix[,2],main="Solar")
boxplot(matrix[,3],main="CO")
boxplot(matrix[,4],main="NO")
boxplot(matrix[,5],main="NO2")
boxplot(matrix[,6],main="O3")
boxplot(matrix[,7],main="HC")

# histogram plots
par(mfrow = c(2, 4))
hist(matrix[,1],main="Wind",xlab="",col="blue", border="pink")
hist(matrix[,2],main="Solar",xlab="",col="blue", border="pink")
hist(matrix[,3],main="CO",xlab="",col="blue", border="pink")
hist(matrix[,4],main="NO",xlab="",col="blue", border="pink")
hist(matrix[,5],main="NO2",xlab="",col="blue", border="pink")
hist(matrix[,6],main="O3",xlab="",col="blue", border="pink")
hist(matrix[,7],main="HC",xlab="",col="blue", border="pink")
title("A Chi-square Q-Q Plot")

## 2-d scatter plots
colnames(x) <- c("Wind", "Solar", "CO", "NO", "NO2", "O3", "HC")
pairs(x)
cor(x)

# Q-Q plots univariate for each feature
par(mfrow = c(2, 4))
qqnorm(matrix[,1],main="Wind"); qqline(x[,1],col=2)
qqnorm(matrix[,2],main="Solar"); qqline(x[,2],col=2)
qqnorm(matrix[,3],main="CO"); qqline(x[,3],col=2)
qqnorm(matrix[,4],main="NO"); qqline(x[,4],col=2)
qqnorm(matrix[,5],main="NO2"); qqline(x[,5],col=2)
qqnorm(matrix[,6],main="O3"); qqline(x[,6],col=2)
qqnorm(matrix[,7],main="HC"); qqline(x[,7],col=2)


# QQ Plot multivariate (this is checking for normality assumption)
p <- ncol(x); n<-nrow(x)
S <- var(x)
xbar <- apply(x, 2, mean)

D2 <- NULL
for (i in 1:n){
  d2 <- as.matrix(x[i,]-xbar) %*% solve(S) %*% t(x[i,]-xbar)
  D2 <- c(D2, d2)
}
D2 <- sort(D2)
par(mfrow = c(1,1))
qqplot(qchisq((1:n-.5)/n, p), D2, pch=21, bg="blue")
abline(0,1,col=2) ### intercept 0, slope 1
title("A Chi-square Q-Q Plot")

# Shapiro Wilks test, reject null that our data is normally distributed
mvnormtest::mshapiro.test(t(x))




# ACF Plot
acf(D2)
