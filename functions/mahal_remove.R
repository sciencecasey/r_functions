# make a routine of above for each subsequent df
# 
# NOTE this is not the classic mahalanobis as I didn't return the values of distances in descending order without removing anything.  It's also not the wilks which takes the values outside the critical value (as below) but then only removes/flags one value.  Nor is it the confidence interval... I guess i blend things. aye aye
mahal_remove <- function(data, i, crit = 1, sd = 3, alpha = .01){
    #'@param data a dataframe to remove outliers from 
    #'@param i the column(s) index to inspect for outliers (can be a vector of indeces)
    #'@param crit number of columns that must be more than 3 SD outside the mean mahalanobis distance for that column
    #'@param the cutoff number of sd needed to be outside of to be considered and flagged as outlier (if a single column)
    #'@param the alpha cutoff (if looking across multiple i features/columns)
    #'@return a data frame, without outliers removed according to the statistics indicated and with an additional column flag_ stating the number of flags (only below crit), and the distance from colMean
    if(length(i)>1){
        # return for each column
        # for(j in seq(1, length(i))){
        #     return(mahal_remove(data, j)) # This doesn't work! only evaluates once :/
        # }
        # # use more complex mahalanobis distance
        tdata <- data[,i]
        dist <- mahalanobis(tdata, center = colMeans(tdata), cov = cov(tdata))
        crit1 <- qchisq(alpha, df = length(i)-1, lower.tail = FALSE)
        crit2 <- -1 # distance always positive
    } else {
        # calculate the mahalanobis distance
        dist <- one_mahl(data[,i])
        mean_m <- mean(dist)
        sd_m <- sd(dist)
        crit1 <- mean_m + sd*sd_m
        crit2 <- mean_m - sd*sd_m        
    }
    # first time, add a flag column
    if(sum(colnames(data) == "flag_") == 0){
        data <- as.data.frame(data)
        data$flag_ <- rep(0, length(data[,1]))
    }
    index <- dist>crit1 | dist<crit2
    # add flag
    data$flag_[index] <- data$flag_[index] + 1
    data$dist <- dist
    rem_index <- data$flag_ >= crit # the number needed to remove
    return(data[!rem_index, ])
}


# Make function for Mahl distance for each value by the value's mean
one_mahl <- function(x){
    #'@param x a vector of type numeric
    #'uses variance & mean to calculate mahalanobis distance
    #'@return vector of distances
    dist = (x - mean(x))^2/var(x)
    return(dist)
}


# checking against simple built in mahal
# out <- mahalanobis(iris[,-5], center = colMeans(iris[,-5]), cov = cov(iris[,-5]))
# i_o <- cbind(iris, out)
# head(i_o)
# i_o <- i_o[order(i_o$out, decreasing = TRUE), ]
# out <- mahalanobis(iris[,1], center = colMeans(iris[,1]), cov = cov(iris[,-5]))
# out <- mahal_remove(iris, 1:4, alpha = .005)
# colMeans(iris[,1:4])
