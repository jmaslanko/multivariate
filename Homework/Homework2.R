# Question 1

B = as.matrix(data.frame(c(1, 7, 8, 1), c(3, 2, 6, 3), c(0,0,0, 0), c(2, 2, 1, -1)))

linalg <- function(A) {
  
  dims <- dim(A)
  det <- det(A)
  
  if (det == 0) {
    print("Matrix is singular.")
  } else if (dims[1] != dims[2]) {
    print("Matix is not square")
  } else {
    rank <- qr(A)$rank
    svd <- svd(A)
    inv <- solve(A)
    
    cat("Rank of Matrix:\n")
    print(rank)
    cat("\n")
    
    cat("SVD of Matrix:\n")
    print(svd)
    cat("\n")
    
    cat("Det of Matrix:\n")
    print(det)
    cat("\n")
    
    cat("Inverse of Matrix:\n")
    print(inv)
  }
}
linalg(B)

# Question 2



# Question 3
# Part a
cov <- matrix(c(25, -2, 4, -2, 4, 1, 4, 1, 9), nrow = 3)
D <- diag(sqrt(diag(cov)))
D_inv <- solve(D)
cor <- D_inv %*% cov %*% D_inv

# Part b
V <- diag(diag(cov))
V_sqrt <- sqrt(V)

# Part c
V_sqrt %*% cor %*% V_sqrt

