hosmer_lemeshow <- function(y, yhat, g=10){
    #'@param y a vector of observed y values
    #'@param yhat a vector of fitted y balues
    #'@param g the number of groups (quantiles) to break the observed ys into
    #'@return a list with the hosmer statistic and the p value for chi-squared distribution with g-2 DOF
    # note that an alternative which has a different return is ResourceSelection::hoslem.test() so there may be an error here
    # cut into groups
    cutyhat <- cut(yhat,
                   breaks = quantile(yhat, probs = seq(0,1,1/g)),
                   include.lowest=TRUE)
    stat <- 0
    for(values in 1:g){
        # select values in the group
        inds <- which(cutyhat==levels(cutyhat)[values])
        obs <- sum(y[inds])
        expmean <- mean(yhat[inds])
        HL <- (obs-length(inds)*expmean)^2/(length(inds)*expmean*(1-expmean))
        stat <- stat+HL
    }
    pval <- pchisq(stat, g-2, lower.tail = F)
    return(list(hosmer_lemeshow_stat = stat, p_value=pval))
}
hosmer_lemeshow(mod$y, mod$fitted.values)


