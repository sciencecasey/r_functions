# Load the datasets created from PCA Hw 2
diagPCA <- readRDS("../hw2_DataAnalysis/output/diagonalKeptEigsPCA.rds")
vertPCA <- readRDS("../hw2_DataAnalysis/output/verticalKeptEigsPCA.rds")
horizPCA <- readRDS("../hw2_DataAnalysis/output/horizontalKeptEigsPCA.rds")
anyDirPCA <- readRDS("../hw2_DataAnalysis/output/anyDirectionEigsPCA.rds")
coefs <-readRDS("../hw2_DataAnalysis/output/all_coefs.rds")
# Load functions from that project
source("../functions/DCT_2.R")
source("../functions/pca_routine.R")
source("../functions/em.R")
source("../functions/FDR_ranking.R")

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

# Find the top 2 ranked features
ranks <- rank_feat(anyDirPCA, class = list(indx0, indx1, indx2, indx3, indx4, indx5, indx6, indx7, indx8, indx9))
head(ranks) # 2 and 1 are the top ranked! but that's bc PCA was all directions
all_pca <- cbind(vertPCA, horizPCA, diagPCA)
ranks2 <- rank_feat(all_pca, class = list(indx0, indx1, indx2, indx3, indx4, indx5, indx6, indx7, indx8, indx9))
head(ranks2) # still 1 and 2, but not the ranks are lower than those in any pca, use above method

# try with combinations of 4
inds <- list(indx0, indx1, indx2, indx3, indx4, indx5, indx6, indx7, indx8, indx9)
library("gtools")
combos <- combinations(n = 10, r = 4)
attempt <- list()
for(row in seq(1, dim(combos)[1])){
    selection <- list()
    for(item in seq(1, length(combos[row,]))){
        selection <- append(selection, inds[combos[row, item]])
    }
    temp <- rank_feat(all_pca, class = selection)
    attempt <- append(attempt, temp[1:2])
}

# find top components within iterations
components <- names(attempt)
components <- as.factor(components)
summary(components) # 1 and 2 are top components
# find top rank value for these
library(dplyr)
attempt2 <- as.array(attempt)
attempt2 <- as.numeric(attempt2)
names(attempt2) <- components
max(attempt2[components == 1]) # 23.34833
max(attempt2[components == 2]) # 21.81723
max(attempt2) # overall max 29.993
attempt2[attempt2 == max(attempt2)] # 64!
# sum across rank values
sum(attempt2[components == levels(components)[1]])
sum(attempt2[components == levels(components)[2]])
sum(attempt2[components == levels(components)[3]])
sum(attempt2[components == levels(components)[4]])
sum(attempt2[components == levels(components)[5]])
sum(attempt2[components == levels(components)[6]])
sum(attempt2[components == levels(components)[7]])
sum(attempt2[components == levels(components)[8]])
sum(attempt2[components == levels(components)[9]])
# 5 and 1 top
levels(components)[1] # 1
levels(components)[5] # 2
# concordance!
# find the numbers with the top values for these
maxie <- max(attempt2[components == 1])
row1 <- attempt2 == max(attempt2[components == 1])
row2 <- attempt2 == max(attempt2[components == 2])
combos[row1, ]
combos[row2, ]

# Try separating the top on its own
# We are looking for DISCRETE top value not overall top value
attempt <- list(value1 = 0, value2 = 0, row1 = c(), row2 = c()) 
for(row in seq(1, dim(combos)[1])){
    selection <- list()
    for(item in seq(1, length(combos[row,]))){
        selection <- append(selection, inds[combos[row, item]])
    }
    temp <- rank_feat(all_pca, class = selection)
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

attempt # both components are 64! run again for the second component
attempt2 <- list(value1 = 0, value2 = 0, row1 = c(), row2 = c()) 
for(row in seq(1, dim(combos)[1])){
    selection <- list()
    for(item in seq(1, length(combos[row,]))){
        selection <- append(selection, inds[combos[row, item]])
    }
    temp <- rank_feat(all_pca, class = selection)
    # grab the top 2 overall
    if(names(temp[1]) == "64"){
        # pass
    }else if(temp[1] > attempt2$value1){
        attempt2$value1 = temp[1]
        attempt2$row1 = combos[row,]
    }else if(temp[1] > attempt2$value2){
        attempt2$value2 = temp[1]
        attempt2$row2 = combos[row,]
    }
    if(names(temp[2]) == "64"){
        # pass
    } else if(temp[2] > attempt2$value1){
        attempt2$value1 = temp[2]
        attempt2$row1 = combos[row,]
    }else if(temp[2] > attempt$value2){
        attempt2$value2 = temp[2]
        attempt2$row2 = combos[row,]
    }
}
attempt2
attempt
# top two values are 64 and 1
# top numbers are:
nums <- c(attempt2$row1, attempt2$row2, attempt$row1, attempt$row2)
nums[order(nums)] # 1 2 and 4 represented 3 times, 5,8, and 9 once

#########Top features: 1 and 64; top indx: 1,2,4 and 5/8/9
# subset the data to top 2 features
top_feat_nums <- anyDirPCA[,c(1,64)]
colnames(top_feat_nums) <- c("PCA1", "PCA64")
# only grab top 4 numbers
top_feat_nums <- rbind(top_feat_nums[indx0,], 
                       top_feat_nums[indx1,], 
                       top_feat_nums[indx3,], 
                       top_feat_nums[indx4,])
seps <- em.simple(data = top_feat_nums, clusters = 4)
colSums(round(seps$expectations)) # 142, 126, 60, 67
rounded <- round(seps$expectations)
clus1 <- rounded[,1] == 1
clus2 <- rounded[,2] == 1
clus3 <- rounded[,3] == 1
clus4 <- rounded[,4] == 1
top_feat_nums <- as.data.frame(top_feat_nums)
str(top_feat_nums)
top_feat_nums$Cluster = rep("cluster", length(top_feat_nums[,1]))
top_feat_nums[clus1,3] = "cluster 1"
top_feat_nums[clus2,3] = "cluster 2"
top_feat_nums[clus3,3] = "cluster 3"
top_feat_nums[clus4,3] = "cluster 4"
top_feat_nums[,3] <- as.factor(top_feat_nums[,3])
sum((top_feat_nums) == "cluster") # ALL CORRECTLY SEPARATED! :D 
str(top_feat_nums)
# normalize
plotty <- data.frame(scale(top_feat_nums[,-3]), stringsAsFactors = TRUE)
plotty$Cluster <- top_feat_nums$Cluster
str(plotty)
levels(plotty$Cluster)

cl= colors()
### Plot the data!
plot(plotty[clus1, 1] ~ plotty[clus1, 2], 
     col = cl[10], xlab = "PCA 64", 
     ylab = "PCA 1", xlim = c(-3, 3), 
     ylim = c(-3, 3), main = "Top components clustered with EM")
points(plotty[clus2, 1] ~ plotty[clus2, 2], col = cl[30])
points(plotty[clus3, 1] ~ plotty[clus3, 2], col = cl[40])
points(plotty[clus4, 1] ~ plotty[clus4, 2], col = cl[60])
legend(-3, -1.5, c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"), 
       col = c(cl[10], cl[30], cl[40], cl[60]),
       text.col = "green4", lty = c(1, 1, 1, 1), #pch = c(NA, 3, 4),
       merge = TRUE, bg = "white")

library(car)
scatterplot(plotty$PCA1 ~ plotty$PCA64,
            groups = plotty$Cluster,
            ellipse = TRUE,
            regLine = FALSE,
            xlab = "PCA 64",
            ylab = "PCA 1",
            main = "Top Components clustered with EM")

## Compare to original data
compare_plotty <- anyDirPCA[,c(1,64)]
compare_plotty <- scale(compare_plotty)
plot(compare_plotty[indx0, 1] ~ compare_plotty[indx0, 2], 
     col = cl[10], xlab = "PCA 64", 
     ylab = "PCA 1", xlim = c(-3, 3), 
     ylim = c(-3, 3), main = "Top components in original groups")
points(compare_plotty[indx1, 1] ~ compare_plotty[indx1, 2], col = cl[35])
points(compare_plotty[indx3, 1] ~ compare_plotty[indx3, 2], col = cl[66])
points(compare_plotty[indx4, 1] ~ compare_plotty[indx4, 2], col = cl[500])
legend(-3, -1.5, c("Index 0", "Index 1", "Index 3", "Index 4"), 
       col = c(cl[10], cl[35], cl[66], cl[500]),
       text.col = "green4", lty = c(1, 1, 1, 1), #pch = c(NA, 3, 4),
       merge = TRUE, bg = "white")

compare_plotty <- anyDirPCA[,c(1,2)]
compare_plotty <- scale(compare_plotty)
plot(compare_plotty[indx0, 1] ~ compare_plotty[indx0, 2], 
     col = cl[10], xlab = "PCA 2", 
     ylab = "PCA 1", xlim = c(-3, 3), 
     ylim = c(-3, 3), main = "Top components in original groups")
points(compare_plotty[indx1, 1] ~ compare_plotty[indx1, 2], col = cl[35])
points(compare_plotty[indx3, 1] ~ compare_plotty[indx3, 2], col = cl[66])
points(compare_plotty[indx4, 1] ~ compare_plotty[indx4, 2], col = cl[500])
legend(-3, -1.5, c("Index 0", "Index 1", "Index 3", "Index 4"), 
       col = c(cl[10], cl[35], cl[66], cl[500]),
       text.col = "green4", lty = c(1, 1, 1, 1), #pch = c(NA, 3, 4),
       merge = TRUE, bg = "white")

# try again PCA 1 and 2 (as these are overall top features)
top_feat_nums <- anyDirPCA[,c(1,2)]
colnames(top_feat_nums) <- c("PCA1", "PCA2")
# only grab top 4 numbers
top_feat_nums <- rbind(top_feat_nums[indx0,], 
                       top_feat_nums[indx1,], 
                       top_feat_nums[indx3,], 
                       top_feat_nums[indx4,])
seps <- em.simple(data = top_feat_nums, clusters = 4)
colSums(round(seps$expectations)) # 99, 53, 106, 134
rounded <- round(seps$expectations)
clus1 <- rounded[,1] == 1
clus2 <- rounded[,2] == 1
clus3 <- rounded[,3] == 1
clus4 <- rounded[,4] == 1
top_feat_nums <- as.data.frame(top_feat_nums)
str(top_feat_nums)
top_feat_nums$Cluster = rep("cluster", length(top_feat_nums[,1]))
top_feat_nums[clus1,3] = "cluster 1"
top_feat_nums[clus2,3] = "cluster 2"
top_feat_nums[clus3,3] = "cluster 3"
top_feat_nums[clus4,3] = "cluster 4"
top_feat_nums[,3] <- as.factor(top_feat_nums[,3])
sum((top_feat_nums) == "cluster")
sum(rowSums(rounded) == 0) # 3 are without a home! why??
str(top_feat_nums)
# normalize
plotty <- data.frame(scale(top_feat_nums[,-3]))
plotty$Cluster <- top_feat_nums$Cluster
library(lattice)
reshape(plotty, direction = "wide")
library(car)
scatterplot(plotty$PCA1 ~ plotty$PCA2,
       # type = "p",
       groups = levels(plotty$Cluster),
       regLine = FALSE,
       legend = list(title = "Clusters"),
       xlab = "PCA 2",
       ylab = "PCA 1")
       # ellipse = TRUE,
       # auto.key = TRUE)
boxplot(plotty, horizontal = TRUE)


