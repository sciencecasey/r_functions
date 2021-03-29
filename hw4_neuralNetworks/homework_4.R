# Testing prob-neur-net

data <- iris[, -5]
classList <- list(1:50, 51:100, 101:150)
sigma <- .1 # match the matlab code
p <- pnn_og(data, classList, sigma)
p[[1]] # accuracy .9866667
# find the misclassed
which(p[[2]] == 0) # 84 and 134

# test the random 80% removed
p2 <- pnn(data, classList, sigma)
