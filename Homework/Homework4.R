# 5.20
#part a

data <- matrix(scan(file="",2,2), byrow = T)

num_rows <- 45
num_cols <- 2

# Step 2: Input data into the matrix using scan()
cat("Enter data for the matrix row by row:\n")
data <- scan(what = numeric(), n = num_rows * num_cols)
X <- matrix(data, nrow = num_rows, ncol = num_cols, byrow = TRUE)

alpha=0.05

level=1-alpha
n<-dim(X)[1]; p<-dim(X)[2]

xbar<-apply(X,2,mean)
S<-var(X)

library(car)
const<-sqrt(p*(n-1)/n/(n-p)*qf(level,p,(n-p)))
ellipse(xbar, S, const, add=F, xlab=expression(mu[1]), ylab=expression(mu[2]))
title("A 95% confidence ellipse for bird data means")

## Bonferroni SCI
(b1.upper=xbar[1]+qt(1-alpha/4,n-1)*sqrt(S[1,1]/n) )
(b1.lower=xbar[1]-qt(1-alpha/4,n-1)*sqrt(S[1,1]/n) )
(b2.upper=xbar[2]+qt(1-alpha/4,n-1)*sqrt(S[2,2]/n) )
(b2.lower=xbar[2]-qt(1-alpha/4,n-1)*sqrt(S[2,2]/n) )

## T2 SCI
(t1.upper=xbar[1]+sqrt(p*(n-1)/(n-p)*qf(1-alpha,p,n-p))*sqrt(S[1,1]/n) )
(t1.lower=xbar[1]-sqrt(p*(n-1)/(n-p)*qf(1-alpha,p,n-p))*sqrt(S[1,1]/n) )
(t2.upper=xbar[2]+sqrt(p*(n-1)/(n-p)*qf(1-alpha,p,n-p))*sqrt(S[2,2]/n) )
(t2.lower=xbar[2]-sqrt(p*(n-1)/(n-p)*qf(1-alpha,p,n-p))*sqrt(S[2,2]/n) )

## pointwise CI
(pw1.upper=xbar[1]+qt(1-alpha/2,n-1)*sqrt(S[1,1]/n) )
(pw1.lower=xbar[1]-qt(1-alpha/2,n-1)*sqrt(S[1,1]/n) )
(pw2.upper=xbar[2]+qt(1-alpha/2,n-1)*sqrt(S[2,2]/n) )
(pw2.lower=xbar[2]-qt(1-alpha/2,n-1)*sqrt(S[2,2]/n) )

abline(v = b1.upper, lty=2); abline(v = b1.lower, lty=2)
abline(h = b2.upper, lty=2); abline(h = b2.lower, lty=2)

abline(v = t1.upper, col="blue", lty=3); abline(v = t1.lower, col="blue", lty=3)
abline(h = t2.upper, col="blue", lty=3); abline(h = t2.lower, col="blue", lty=3)

abline(v = pw1.upper, col=6, lty=4); abline(v = pw1.lower, col=6, lty=4)
abline(h = pw2.upper, col=6, lty=4); abline(h = pw2.lower, col=6, lty=4)

legend(0.609, 0.595, c(expression(T^2),"Bon","ptwise"), text.col =c("blue",1,6), col=c(4,1,6), lty=c(3,2,4), cex = 1, bty="n") #no box for legend
