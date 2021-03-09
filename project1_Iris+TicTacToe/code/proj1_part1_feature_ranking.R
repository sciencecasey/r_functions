rank_feat<- function(dataFrame, class){
    #' @param dataFrame should have all numeric columns of features
    #' @param class should be a list containing the numeric range for each class
    #' @return a vector of the FDR values in the order of the columns in data frame, 
    #' these can be interpreted with largest value as rank 1 and smallest value as lowest rank
    rank = rep(0, dim(dataFrame)[2]) 
    for (feature in seq(1, length(rank))) {
        fdr = 0
        for (i in seq(1, length(class))) {
            tot = 0
            muI = mean(dataFrame[class[[i]],feature])
            varI = var(dataFrame[class[[i]],feature])
            for (j in seq(1,length(class))) {
                if(i!=j){
                    muJ = mean(dataFrame[class[[j]],feature])
                    varJ = var(dataFrame[class[[j]],feature])
                    tot = tot + ((muI-muJ)^2/(varI+varJ))
                }
            } # end class j
            # add to overall for feature
            fdr = fdr + tot
        } # end class j
        rank[feature] = fdr
    } # end feature
    return(rank)
}

dataFrame <- iris[,-5]
classy <- list(1:50, 51:100, 101:150)
rank_feat(dataFrame = dataFrame, class = classy)

# test against mass
library(MASS)
lda(Species~., data = iris, method = "t", prior = c(1,1,1)/3)


