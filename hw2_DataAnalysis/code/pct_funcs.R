# Note, there are 2 built in PCA functions
# PRcomp uses singular value decomposition while Princomp uses eigenvalues and correlation matrix
# both center the data, but it needed to be scaled to match matlab
library(dplyr)
library(lattice)
prcomp() 
princomp()
obj2 <- princomp(iris[-5]) # DOES NOT GIVE CORRECT VALUES!

# standardize the data first
R = cor(iris[-5]) # gives different values than matlab, but get same PCA
R_vals <- eigen(R)$values
R_pca <- prcomp(iris[-5], scale = TRUE) # this one centers the data but scaling makes match matlab!
summary(R_pca) # gives the cumulative propotion of each vec

# note that the below gives exact same vals as: 
#eigen(cor(iris[-5])) OR eigen(R)
R_vec <- R_pca$rotation
summary(R_pca) # gives proportion  of variance

(C <- cov(iris[-5]))
(Dinv <- sqrt(1/diag(diag(C))))
index <-  (Dinv == Inf)
Dinv[index] <-  0
Cexplained <- summary(prcomp(C))
Cexplained <- Cexplained$importance["Cumulative Proportion",] # only save proportion
C_vec <- eigen(C)$vectors
C_vals <- eigen(C)$values
X_pcacov = as.matrix(iris[-5]) %*% C_vec # multiply by covariance eigenvectors
colnames(X_pcacov) = c("PCA1", "PCA2", "PCA3", "PCA4")
Species = c(rep(c("setosa", "virginica", "versicolor"), each = 50))
X_pcacov <- cbind(X_pcacov, Species)
plot(X_pcacov, col =c("red", "green", "blue"))
X_pcacov <- as.data.frame(X_pcacov)
plot(X_pcacov)
scatterplot(X_pcacov)
principal <- function(data){
  R = cor(data)
  prinObj = princomp(data)
  vec = eigen(R)$vectors
  vals = eigen(R)$values
  
}