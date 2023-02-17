normazlize_zscore <- function(data){
    #'@param data a data frame or matrix with rows of observations and cols of features (all numeric)
    #'@return a data frame or matrix of zscore normalized data
    means <- colMeans(data)
    sds <- apply(data, 2, sd)
    normed <- (data-means)/sds
    return(normed)
}

