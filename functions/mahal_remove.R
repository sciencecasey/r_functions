# make a routine of above for each subsequent df
mahal_remove <- function(data, i, crit = 1){
    #'@param data a dataframe to remove outliers from 
    #'@param i the column(s) index to inspect for outliers (can be a vector of indeces)
    #'@param crit number of columns that must be more than 3 SD outside the mean mahalanobis distance for that column
    #'@return a data frame, without outliers more than 3 SD from the mean for the crit number of columns
    if(length(i)>1){
        # return for each column
        for(j in seq(1, length(i))){
            return(mahal_remove(data, j)) 
        }
    } else {
        # first time, add a flag column
        if(sum(colnames(data) == "flag_") == 0){
            data$flag_ <- rep(0, length(data[,1]))
        }
        # calculate the mahalanobis distance
        dist <- one_mahl(data[,i])
        mean_m <- mean(dist)
        sd_m <- sd(dist)
        crit1 <- mean_m + 3*sd_m
        crit2 <- mean_m - 3*sd_m
        index <- dist>crit1 | dist<crit2
        # add flag
        data$flag_[index] <- data$flag_ + 1
        rem_index <- data$flag_ >= crit # the number needed to remove
        return(data[!rem_index,])
    }
}


# Make function for Mahl distance for each value by the value's mean
one_mahl <- function(x){
    #'@param x a vector of type numeric
    #'uses variance & mean to calculate mahalanobis distance
    #'@return vector of distances
    dist = (x - mean(x))^2*var(x)
    return(dist)
}