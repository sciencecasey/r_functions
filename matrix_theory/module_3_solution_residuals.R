# Discussion post
# Homework starts at lint 77
# For this discussion, explore of computational complexity using a linear algebra library of your choice that provides sparse and dense matrices (Python, R, Matlab, Java all have linear algebra libraries). Please use library's ability to represent sparse or dense matrices to experiment. For example, gather run times for matrix solves, LU factorizations, matrix multiplication, etc. Compare the results with the theory from the book to see if you can explain those differences. How do you think the library stores sparse matrices? Offer some evidence of your guess. While I encourage you to explore all of these questions and more, please limit your original comment to just one observation to allow everyone a chance to comment on some part of this discussion.


library(pracma)
library(Matrix)
library(matrixStats)
library(MASS)
# Column oriented sparse matrix

# grab 10 random indeces
indx_i <- round(runif(10, 1, 10000))
indx_j <- round(runif(10, 1, 10000))

# Make a sparse matrix 10,000 X 10,000 with nonzero entries at (i,j)
# without entering values they will all be ones
s <- sparseMatrix(i = indx_i, j = indx_j, dims = c(10000, 10000))
sum(s) # as expected

isSymmetric(s)
isTriangular(s)
isDiagonal(s)
summary(s)

s %*% s # calculate the matrix crossproduct
det(s)
lu(s)
# Fails as A is near singular or out of memory

# grab 1000 random indeces
indx_i <- round(runif(1000, 1, 10000))
indx_j <- round(runif(1000, 1, 10000))

# Make a sparse matrix 10,000 X 10,000 with nonzero entries at (i,j)
# without entering values they will all be ones
s <- sparseMatrix(i = indx_i, j = indx_j, dims = c(10000, 10000))
sum(s) # as expected

isSymmetric(s)
isTriangular(s)
isDiagonal(s)
summary(s)

s %*% s # calculate the matrix crossproduct
det(s)
lu(s)
solve(s)


# modified code from Jonathan Brophy's post
runs <- 1000
x <- matrix(0, runs, 1) # using %*%
y <- matrix(0, runs, 1) # using crossprod()
for ( i in 1:runs){
    n <- i
    a <- matrix(rnorm(n*n, mean=0, sd=25), n, n)
    b <- matrix(rnorm(n*n, mean=0, sd = 25), n, n)
    start_time <- Sys.time()
    a %*% b
    end_time <- Sys.time()
    x[i] <- end_time - start_time
    start_time <- Sys.time()
    crossprod(a, b)
    end_time <- Sys.time()
    y[i] <- end_time - start_time
}
fit <- lm(x~poly(row(x), 3))
fit2 <- lm(y~poly(row(y), 3))
plot(x, main = "Cossprod() vs. %*% Functions", ylab = "Time to process", xlab = "Matrix Size")
lines(fitted(fit), col = "red")
points(y, col = "grey")
lines(fitted(fit2), col = "blue")




# Homework 3

# Problem 1: 3.9.6 NB
A = matrix(c(.89, .47, .53, .28), nrow = 2)
A
b = c(.36, .19)
x = round(solve(A, b)) # returns the true solution
x_est = c(.47, -.11)

# Different ways to compute residual of x
b - A%*%x_est
A %*% x - A %*% x_est
A%*%(x - x_est)


# relative change in x
Norm(x-x_est)/Norm(x)
# relative change in b
Norm(c(.36, .1901)-b)/Norm(b)

# Condition number from library v inverse
cond(A) # very large
norm(A)*norm(inv(A))
# why are these so different?

# Using the equation for amount changed per epsilon
# conditionA times changeX + changeB
cond(A)*(Norm(x-x_est)/Norm(x) + Norm(c(.36, .1901)-b)/Norm(b))

estimated_rel_change <- function(A, b = NULL, b_est = NULL, x = NULL, x_est = NULL, epsilon){
    #'@param A an invertible matrix
    #'@param b solution vector for A
    #'@param b_est a solution vector for A that's been changed (if not provided use b+epsilon)
    #'@param x a vector of comparison values (if not provided use solve(A, b))
    #'@param x_est a vector of estimated solution for x
    #'@param epsilon the amount of change applied to the solution vector
    #'@return the estimated change
    require(pracma)
    if(is.null(b)){
        if(is.null(x)){
            errorCondition("Must include either x or b at minimum")
        }else{
            # calculate b from true_x
            b = A %*% x
        }
    }
    if(is.null(b_est)){
        b_est = b + epsilon
    }
    if(is.null(x_est)){
        x_est = solve(A, b_est)
    }
    if(is.null(x)){
        x = solve(A, b)
    }

    rel_x = Norm(true_x - x_est)/Norm(true_x)
    rel_b = Norm(b_est -b)/Norm(b)
    return(cond(A)*(rel_x+rel_b)+epsilon^2)
}

# Problem 2 3.6.10
A = matrix(c(.89, .47, .53, .28), nrow = 2)
A
b = c(.36, .19)
x = c(1,-1)
# part a
x_est = solve(A, b)
x_est # 1, -1 as printed
# part b
error = x - x_est
error # 2.943201e13, -4.940492e-13
# c
r = b-A %*% x_est
r  # 0, 5.551115e-17
r = A%*%(x - x_est)
r # 9.880985e-17, -3.330669e-18
# relative change in x
Norm(x-x_est)/Norm(x) # 4.06638e-13
# relative change in b
Norm(A%*%x_est -b)/Norm(b) # 1.3637e-16
cond(A) # 13723


