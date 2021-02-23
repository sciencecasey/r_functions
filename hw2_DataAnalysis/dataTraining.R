library(here)
library(lattice)
train = read.csv("MATLAB/Algs_Mod3_trainFeaturesExamples/train.csv")
str(train)

# serialize the data to load faster next time (for workin on hw)
saveRDS(train, file = "Learn_R/hw2_DataAnalysis/training_data.rds")
train = readRDS("training_data.rds")

# initialize matrix for the output
newmat = matrix(rep(1, 28), ncol = 28)
for(row in seq(1, 1000)){
  # grab one row, without first item and put into matrix
  mat = as.matrix(train[row, -1])
  # change dimensions
  dim(mat) <- c(28, 28)
  # add the matrix to the output
  newmat = rbind(newmat, mat)
}

#remove the first row from new matrix (used for initialization)
newmat = newmat[-1,]
dim(newmat)

library(pracma) # for rotate function
mat1 = t(newmat[1:28,])
mat1 = rot90(mat1, -1)
c(1*(28)+1, 1*(28)+28)
mat2 = t(newmat[29:56,])
mat2 = rot90(mat2, -1)
c(3*(28)+1, 4*(28))
mat4 = t(newmat[85:112,])
mat4 = rot90(mat4, -1)
c(6*(28)+1, 7*(28))
mat7 = t(newmat[169:196,])
mat7 = rot90(mat7, -1)
mat8 = t(newmat[197:224,])
mat8 = rot90(mat8, -1)
mat9 = t(newmat[225:252,])
mat9 = rot90(mat9, -1)
c(10*(28)+1, 11*(28))
mat11 = t(newmat[281:308,])
mat11 = rot90(mat11, -1)
mat12 = t(newmat[309:336,])
mat12 = rot90(mat12, -1)
c(16*(28)+1, 17*(28))
mat17 =  t(newmat[449:476,])
mat17 = rot90(mat17, -1)
c(21*(28)+1, 22*(28))
mat22 = t(newmat[589:616,])
mat22 = rot90(mat22, -1)

library(lattice)
levelplot(mat2, col.regions = gray(0:100/100))
levelplot(mat1, col.regions = gray(0:100/100))
levelplot(mat17, col.regions = gray(0:100/100))
levelplot(mat8, col.regions = gray(0:100/100))
levelplot(mat4, col.regions = gray(0:100/100))
levelplot(mat9, col.regions = gray(0:100/100))
levelplot(mat22, col.regions = gray(0:100/100))
levelplot(mat7, col.regions = gray(0:100/100))
levelplot(mat11, col.regions = gray(0:100/100))
levelplot(mat12, col.regions = gray(0:100/100))

