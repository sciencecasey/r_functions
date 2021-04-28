normalize_by_feature <- function(data, normAgainst = NULL){
    #'@param data a numeric data frame with dimenstions as cols and obs as rows
    #'@param normAgainst an optional data frame matching input data structurally to normalize by/against
    #'performs a max-min normalization to bring into 0-1 range
    #'@return the normalized data frame
    #'@author Casey Jayne
    if(is.null(normAgainst)){
        d <- dim(data)[2]
        mins <- apply(data, 2, min)
        maxs <- apply(data, 2, max)
        out <- t((t(data) - mins)/(maxs - mins))
        return(out)    
    }else{
        # here we want to make sure the max and min are within the range of our test data
        mins <- apply(normAgainst, 2, min)
        maxs <- apply(normAgainst, 2, max)
        out <- t((t(data) - mins)/(maxs - mins))
        return(out)    
    }
}

normalize_by_obs <- function(data){
    #'@param data a numeric data frame with dimensions as cols and obs as rows
    #'performs a vector normalization by row (observation)
    #'@return the normalized data frame
    #'@author Casey Jayne
    sums <- rowSums(data)^2 # squared sum
    out <- data/sqrt(sums)
    return(out)
}
