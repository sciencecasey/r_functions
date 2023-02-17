influence_stats <- function(model, observed_probability = null){
    #'@param model a model from glm
    #'@param observed probability the observed probabilities if repeated measures taken on regressors.  Otherwise, simply the input y values as though no repeats taken
    #'@return a data frame of observed & estimated probabilities, deviance, pearson residuals, h_ii values and standardized pearson residuals
    #'@author Casey Jayne
    if(is.null(observed_probability)){
        dv <- model$formula[[2]]
        observed_probability = model$data[,dv]
    }
    inf <- influence(model)
    df <- data.frame(observed_prob = observed_probability,
               estimated = unique(model$fitted.values),
               deviance = inf$dev.res,
               pearson = inf$pear.res,
               h_ii= inf$hat,
               standard_pearson = inf$pear.res/sqrt(1-inf$hat))
    return(df)
}

