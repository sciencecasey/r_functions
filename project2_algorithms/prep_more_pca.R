# Reprocess Using the input code from HW2 and the PCA routine from HW 3
# source("../functions/DCT_2.R")
# source("../functions/pca_routine.R")
# source("../functions/FDR_ranking.R")
# train = readRDS("../hw2_DataAnalysis/input/training_data.rds")

# For turned in all in one folder
source("DCT_2.R")
source("pca_routine.R")
source("FDR_ranking.R")
train = readRDS("training_data.rds")


# initialize matrix for the output
newmat = matrix(rep(1, 784), nrow = 784) # initialize with features as columns
# process 42,000 observations with DCT and grab the coefficients
for(row in seq(1, 10000)){
    # grab one row, without first row(of labels) and put into matrix
    mat <- as.matrix(train[row, -1])
    # change dimensions
    dim(mat) <- c(28, 28)
    # take a DCT of the matrix
    mat <- DCT_2(mat)
    # get coefficients
    horiz = get_horizontal(mat) # matrix of zeros in mask locations
    coef <- horiz!= 0
    hcoef <- horiz[coef] # place nonzero coefficients into a single vector
    vert <- get_vertical(mat)
    coef <- vert != 0
    vcoef <- vert[coef]
    diag <- get_diagonal(mat)
    coef <- diag!= 0
    dcoef <- diag[coef]
    # select only the coefficients
    # order is horizontal, vertical, diag
    # reshape the matrix with the first item = label
    coefs <- c(trunc(train[row,1]), hcoef, vcoef, dcoef)
    # add the matrix to the output
    newmat = cbind(newmat, coefs)
}
# remove the initializer column
newmat <- newmat[,-1]

#rename and make features the columns
coefs <- t(newmat)

# serialize the data to load faster next time (for workin on hw)
# saveRDS(coefs, file = "all_1000_coefs.rds")
coefs <-readRDS("out_data/all_1000_coefs.rds")

# get the indeces for the values, for plotting later
indx0 <- coefs[,1] == 0
indx1 <- coefs[,1] == 1
indx2 <- coefs[,1] == 2
indx3 <- coefs[,1] == 3
indx4 <- coefs[,1] == 4
indx5 <- coefs[,1] == 5
indx6 <- coefs[,1] == 6
indx7 <- coefs[,1] == 7
indx8 <- coefs[,1] == 8
indx9 <- coefs[,1] == 9

# try overall as well
hc <- coefs[,2:225]
vc <- coefs[,226:449]
dc <- coefs[,450:784]
# remove lables
coefs <- coefs[,-1]

# correlations across features used solely to decide how many components to keep
rh = cor(hc)
rv = cor(vc)
rd = cor(dc)
rall = cor(coefs)

# principle components
rh_pc = prcomp(rh, scale = TRUE)
rv_pc = prcomp(rv, scale = TRUE)
rd_pc = prcomp(rd, scale = TRUE)
rall_pc = prcomp(rd, scale = TRUE)

# extract the components up to at least .95
# horizontal
explained = summary(rh_pc)
explained = explained$importance["Cumulative Proportion",]
keep = explained<.95
keep = explained[keep]
# grab the last (up to 95% or more)
numkeep <- length(keep)+1
hkeep <- numkeep # 75
# vertical
explained = summary(rv_pc)
explained = explained$importance["Cumulative Proportion",]
keep = explained<.95
keep = explained[keep]
# grab the last (up to 95% or more)
numkeep <- length(keep)+1
vkeep <- numkeep # 69
# diagonal
explained = summary(rd_pc)
explained = explained$importance["Cumulative Proportion",]
keep = explained<.95
keep = explained[keep]
# grab the last (up to 95% or more)
numkeep <- length(keep)+1
dkeep <- numkeep # 123
# overall ranking
explained <- summary(rall_pc)
explained <- explained$importance["Cumulative Proportion",]
keep <- explained<.95
keep <- explained[keep]
numkeep <- length(keep)+1
allkeep <- numkeep # 123

# Use covariance to reduce dimensionality for the number determined w/ correlation
# covariance of the DCT coefficients
cov_h = cov(hc)
cov_v = cov(vc)
cov_d = cov(dc)
cov_all = cov(coefs)
# principle components of the covariance
cd_pc = prin_cov(cov_d)
cv_pc = prin_cov(cov_v)
ch_pc = prin_cov(cov_h)
call_pc = prin_cov(cov_all)

# get the top items we selected from above
pcaD <- dc %*% cd_pc$vec[,1:dkeep] # grab the indeces from dkeep multiply 10 X 335 
# by 335*dkeepSize = 10*dkeepSize
pcaH <- hc %*% ch_pc$vec[,1:hkeep]
pcaV <- vc %*% cv_pc$vec[,1:vkeep]
pcaAll <- coefs %*% call_pc$vec[,1:allkeep]

# export the output to the project folder
saveRDS(pcaD, file = "diagonalKeptEigs_1000_PCA.rds")
saveRDS(pcaV, file = "verticalKeptEigs_1000_PCA.rds")
saveRDS(pcaH, file = "horizontalKeptEigs_1000_PCA.rds")
saveRDS(pcaAll, file = "anyDirectionEigs_1000_PCA.rds")


# which features give best separation for 0 and 1, then which give best between 0 and 2, and o=0 and 3
# then make a 1:1 comparison with 1 to 2, 1 to 3
# this allows us to separate the tops features for the overall set
# regardless of direction, separate with feature == principle component

# 1 vs all is when we look at 0 vs 1:9, 1 vs 0;2:9, 2 vs 0:1;3:9

# plot!
cl <-colors()
# colur <- runif(10, max = 657)
# cl <- cl[colur]
plot(pcaV[indx0,2]~pcaV[indx0,1], col = cl[10], lwd = 3,
     xlim = c(-2000, 2000), ylim = c(-2000, 2000),
     main = "Vertical Components",
     xlab = "PC 2", ylab = "PC 1")
points(pcaV[indx1,2]~pcaV[indx1,1], col = cl[20], lwd = 3)
points(pcaV[indx2,2]~pcaV[indx2,1], col = cl[30], lwd = 3)
points(pcaV[indx3,2]~pcaV[indx3,1], col = cl[40], lwd = 3)
points(pcaV[indx4,2]~pcaV[indx4,1], col = cl[50], lwd = 3)

plot(pcaH[indx0,2]~pcaH[indx0,1], col = cl[10], lwd = 3,
     xlim = c(-2000, 2000), ylim = c(-2000, 2000),
     main = "Horizontal Components",
     xlab = "PC 2", ylab = "PC 1")
points(pcaH[indx1,2]~pcaH[indx1,1], col = cl[20], lwd = 3)
points(pcaH[indx2,2]~pcaH[indx2,1], col = cl[30], lwd = 3)
points(pcaH[indx3,2]~pcaH[indx3,1], col = cl[40], lwd = 3)
points(pcaH[indx4,2]~pcaH[indx4,1], col = cl[50], lwd = 3)

plot(pcaD[indx0,2]~pcaD[indx0,1], col = cl[10], lwd = 3,
     xlim = c(-2000, 2000), ylim = c(-2000, 2000),
     main = "Diagonal Components",
     xlab = "PC 2", ylab = "PC 1")
points(pcaD[indx1,2]~pcaD[indx1,1], col = cl[20], lwd = 3)
points(pcaD[indx2,2]~pcaD[indx2,1], col = cl[30], lwd = 3)
points(pcaD[indx3,2]~pcaD[indx3,1], col = cl[40], lwd = 3)
points(pcaD[indx4,2]~pcaD[indx4,1], col = cl[50], lwd = 3)

plot(pcaAll[indx0,2]~pcaAll[indx0,1], col = cl[10], lwd = 3,
     xlim = c(-2000, 2000), ylim = c(-2000, 2000),
     main = "Any Direction Components",
     xlab = "PC 2", ylab = "PC 1")
points(pcaAll[indx1,2]~pcaAll[indx1,1], col = cl[20], lwd = 3)
points(pcaAll[indx2,2]~pcaAll[indx2,1], col = cl[30], lwd = 3)
points(pcaAll[indx3,2]~pcaAll[indx3,1], col = cl[40], lwd = 3)
points(pcaAll[indx4,2]~pcaAll[indx4,1], col = cl[50], lwd = 3)




# Find the top 2 ranked features
ranks <- rank_feat(pcaAll, class = list(indx0, indx1, indx2, indx3, indx4, indx5, indx6, indx7, indx8, indx9))
head(ranks) # 2 and 1 are the top ranked! but that's bc PCA was all directions
all_pca <- cbind(pcaD, pcaH, pcaV)
ranks2 <- rank_feat(all_pca, class = list(indx0, indx1, indx2, indx3, indx4, indx5, indx6, indx7, indx8, indx9))
head(ranks2) # now 200 and 199, but not the ranks are lower than those in any pca, use above method

# only select the top ranks!
head(pcaAll)
ranks[1:10]
pca_less <- pcaAll[,c(2,1,4,5,6,3,8,7,13,15)]
dim(pca_less)

# try with combinations of 4
inds <- list(indx0, indx1, indx2, indx3, indx4, indx5, indx6, indx7, indx8, indx9)
library("gtools")
combos <- combinations(n = 10, r = 4)
# Try separating the top on its own
# We are looking for DISCRETE top value not overall top value
attempt <- list(value1 = 0, value2 = 0, row1 = c(), row2 = c()) 
for(row in seq(1, dim(combos)[1])){
    selection <- list()
    for(item in seq(1, length(combos[row,]))){
        selection <- append(selection, inds[combos[row, item]])
    }
    temp <- rank_feat(pca_less, class = selection)
    # grab the top 2 overall
    if(temp[1] > attempt$value1){
        attempt$value1 = temp[1]
        attempt$row1 = combos[row,]
    }else if(temp[1] > attempt$value2){
        attempt$value2 = temp[1]
        attempt$row2 = combos[row,]
    }
    
    if(temp[2] > attempt$value1){
        attempt$value1 = temp[2]
        attempt$row1 = combos[row,]
    }else if(temp[2] > attempt$value2){
        attempt$value2 = temp[2]
        attempt$row2 = combos[row,]
    }
    # attempt <- append(attempt, temp[1:2])
}

attempt # both are 2 with 1,2,4,9, and 10 - get next value
attempt2 <- list(value1 = 0, value2 = 0, row1 = c(), row2 = c()) 
for(row in seq(1, dim(combos)[1])){
    selection <- list()
    for(item in seq(1, length(combos[row,]))){
        selection <- append(selection, inds[combos[row, item]])
    }
    temp <- rank_feat(pca_less, class = selection)
    # grab the top 2 overall
    if(names(temp[1]) == "2"){
        # pass
    }else if(temp[1] > attempt2$value1){
        attempt2$value1 = temp[1]
        attempt2$row1 = combos[row,]
    }else if(temp[1] > attempt2$value2){
        attempt2$value2 = temp[1]
        attempt2$row2 = combos[row,]
    }
    if(names(temp[2]) == "2"){
        # pass
    } else if(temp[2] > attempt2$value1){
        attempt2$value1 = temp[2]
        attempt2$row1 = combos[row,]
    }else if(temp[2] > attempt$value2){
        attempt2$value2 = temp[2]
        attempt2$row2 = combos[row,]
    }
}
attempt2 # both 1s
attempt
# top two values are 2 and 1
# top numbers are:
nums <- c(attempt2$row1, attempt2$row2, attempt$row1, attempt$row2)
nums[order(nums)] # 2 and 4 represented 4 times, 10, rep 3 times 1 rep 2 times

# this suggests the 1, 3 and 9 would separate best try plots again (maybe 0)
# try plotting the indeces for only these components
plot(pcaAll[indx1,2]~pcaAll[indx1,1], col = cl[10], lwd = 3,
     xlim = c(-2000, 2000), ylim = c(-2000, 2000),
     main = "Any Direction Components",
     xlab = "PC 2", ylab = "PC 1")
points(pcaAll[indx3,2]~pcaAll[indx3,1], col = cl[20], lwd = 3)
points(pcaAll[indx9,2]~pcaAll[indx9,1], col = cl[30], lwd = 3)
# points(pcaAll[indx6,2]~pcaAll[indx6,1], col = cl[40], lwd = 3)
points(pcaAll[indx0,2]~pcaAll[indx0,1], col = cl[50], lwd = 3)
# points(pcaAll[indx8,2]~pcaAll[indx8,1], col = cl[200], lwd = 3)


