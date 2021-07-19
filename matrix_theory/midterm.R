
# Check solution for number 2
A = matrix(c(1,0,
             2,1), nrow = 2)
B = matrix(c(1,2,
             0,1), nrow = 2)
Ainv = inv(A)
Binv = inv(B)
(A+B) %*% (A+B)

# Problem 3
library(pracma)
inv(a)
inv(b)
inv(matrix(c(1,0,0,1), nrow = 2))

source("functions/gram_schmidt.R")
vecs = list(matrix(c(1,1,-1,-1)), matrix(c(3,1,-2,0)))
vecs
vperp = gs(vecs)
t(vperp[[1]]) %*% vperp[[2]] # orthogonal
vec_normalize(vperp)
crossprod(vec_normalize(vperp)[[1]], vec_normalize(vperp)[[2]]) # orthogonal

# part b
vperp = vec_normalize(vperp)
y = matrix(c(1,2,3,4))
w1 = vperp[[1]]
w2 = vperp[[2]]
a = t(w1) %*% y # inner product vperp, y
b = t(w2) %*% y # inner product vperp, y
answer = a[[1]]*w1 + b[1]*w2
answer

# check with bulit-in
library(pracma)
vecs_mat = matrix(c(vecs[[1]], vecs[[2]]), nrow = 4)
vecs_mat
vp_mat = matrix(c(w1, w2), nrow = 4)
vp_mat
proj = linearproj(vp_mat, y) # above found same projection
proj
orth(vp_mat) # above found same space


# Checking solutions to problem 4
t(vecs_mat) %*% matrix(c(-1/2, 3/2, 0, 1))
t(vecs_mat) %*% matrix(c(1/2, 1/2, 1, 0))

# check if orthogonal to eachother
u1 = matrix(c(-1/2, 3/2, 0, 1))
u2 = matrix(c(1/2, 1/2, 1, 0))
theta = t(u1) %*% u2 # .5
theta = theta/(sqrt(crossprod(u1))*sqrt(crossprod(u2)))
theta

# create an orthogonal vector with gs process
amount = (crossprod(u1, u2)/crossprod(u1))
try1 = u2-amount[1]*u1
try1
t(try1) %*% vecs[[2]] # it works!!

udef = c(matrix(c(4/7, 2/7, 1, -1/7)))
round(udef %*% vecs_mat, 2) # works outside rounding error