source("../functions/normalize_minmax.R")
source("../functions/mahal_remove.R")
source("../functions/bayesClassify.R")
source("../functions/parzenWindow.R")
source("../functions/prob_neur_net.R")
source("../functions/rbf_neur_net.R")
source("../functions/cross_validate_separations.R")
source("../functions/confusion_matrix.R")
# turned in version
# source("normalize_minmax.R")
# source("bayesClassify.R")
# source("parzenWindow.R")
# source("mahal_remove.R")
# source("prob_neur_net.R")
# source("rbf_neur_net.R")
# source("cross_validate_separations.R")
# source("confusion_matrix.R")
# Data Cleaning
# Min-Max normalization
# cleaned_pca_less <- normalize_by_obs(pca_less)
# by feature
cleaned_pca_less <- normalize(pca_less)
min(cleaned_pca_less)
max(cleaned_pca_less)

# Mahalanobis outlier romoval
# make a routine of above for each subsequent df
# change the indeces!
dim(cleaned_pca_less)
class <- rep(-1, 10000)
indeces <- list(indx0, indx1, indx2, indx3, indx4, indx5, indx6, indx7, indx8, indx9)
for(i in 1:10){
    selected <- which(indeces[[i]])
    class[selected] <- (i - 1)
}
# append the classes to the data
cleaned_pca_less <- as.data.frame(cleaned_pca_less)
cleaned_pca_less$class <- class

# outlier removal by class
temp0 <- mahal_remove(cleaned_pca_less[indx0,-11], i=1:10, alpha = .01)
temp0[11] <- 0
temp1 <- mahal_remove(cleaned_pca_less[indx1,-11], i=1:10, alpha = .01)
temp1[11] <- 1
temp0 <- rbind(temp0[,1:11], temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx2,-11], i=1:10, alpha = .01)
temp1[11] <- 2
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx3,-11], i=1:10, alpha = .01)
temp1[11] <-3
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx4,-11], i=1:10, alpha = .01)
temp1[11] <- 4
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx5,-11], i=1:10, alpha = .01)
temp1[11] <- 5
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx6,-11], i=1:10, alpha = .01)
temp1[11] <- 6
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx7,-11], i=1:10, alpha = .01)
temp1[11] <- 7
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx8,-11], i=1:10, alpha = .01)
temp1[11] <- 8
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx9,-11], i=1:10, alpha = .01)
temp1[11] <- 9
temp0 <- rbind(temp0, temp1[,1:11])
cleaned_pca_less <- temp0
rm(temp0)
rm(temp1)
dim(cleaned_pca_less)

# remove the stats columns (flags outside distance and )
# cleaned_pca_less <- cleaned_pca_less[,-c(12, 13)]

# udpdate the indeces!
# grab indeces
indx0 <- cleaned_pca_less[,11] == 0
indx1 <- cleaned_pca_less[,11] == 1
indx2 <- cleaned_pca_less[,11] == 2
indx3 <- cleaned_pca_less[,11] == 3
indx4 <- cleaned_pca_less[,11] == 4
indx5 <- cleaned_pca_less[,11] == 5
indx6 <- cleaned_pca_less[,11] == 6
indx7 <- cleaned_pca_less[,11] == 7
indx8 <- cleaned_pca_less[,11] == 8
indx9 <- cleaned_pca_less[,11] == 9

# recheck the separation without outliers
plot(cleaned_pca_less[indx1,2]~cleaned_pca_less[indx1,1], col = cl[10], lwd = 3,
     xlim = c(0, 1), ylim = c(0, 1),
     main = "MinMaxNorm & Mahalanobis",
     xlab = "PC 2", ylab = "PC 1")
points(cleaned_pca_less[indx3,2]~cleaned_pca_less[indx3,1], col = cl[20], lwd = 3)
points(cleaned_pca_less[indx9,2]~cleaned_pca_less[indx9,1], col = cl[30], lwd = 3)
points(cleaned_pca_less[indx0,2]~cleaned_pca_less[indx0,1], col = cl[50], lwd = 3)
points(cleaned_pca_less[indx8,2]~cleaned_pca_less[indx8,1], col = cl[200], lwd = 3)

# Use Bayes Classifier 
classified <- bayes_classifier(cleaned_pca_less[,-11], 
                            classList = list(which(indx0), which(indx1), 
                                             which(indx2), which(indx3), 
                                             which(indx4), which(indx5), 
                                             which(indx6), which(indx7),
                                             which(indx8), which(indx9)), 
                            percentTrain = .8)
bayes_classifier.accuracy(classified, test_list = classified$test_classList)
# 91 % ! not bad :) 

# now apply parzen window
classList = list(which(indx0), which(indx1), 
                 which(indx2), which(indx3), 
                 which(indx4), which(indx5), 
                 which(indx6), which(indx7),
                 which(indx8), which(indx9))
windowed <- parz_window(test_data = cleaned_pca_less, 
            classList =  classList,
            spread = .2, train_amt = .8)
dim(windowed)