
###############################################################################################use mahal remove this isn't right!!








wilks_outlier_remove <- function(data, alpha = NULL){
    # https://stackoverflow.com/questions/45289225/removing-multivariate-outliers-with-mvoutlier
    n <- dim(data)[1] # number of observations
    p <- dim(data)[2] # number of features
    m.dist <- n * mahalanobis(data, center = colMeans(data),
                              cov = cov(data))/(n-1)^2
    F.stat <- ((n-p-1)/p)*(1/(1-m.dist-1))
    desicion <- 1 - pf(F.stat, p, n-p-1)
    stast <- cbind(m.dist = (1-m.dist), F.stat, desicion)
    outliers <- which((1-m.dist) > desicion)
    return(list(stats = stast, outliers = outliers))
}

wilks_outlier_remove(iris[,-5])


# trying to recreate the matlab code
wilks_outlier_og <- function(data, alpha = NULL){
    require(pracma)
    n <- dim(data)[1] # number of observations
    p <- dim(data)[2] # number of features
    sds <- apply(data, 2, sd)
    desicion <- t(3*sds)%*%pinv(cov(data))%*%(3*sds)
    distance <- cbind(data, critical = desicion)
    outliers <- which(data > desicion)
    return(list(stats = stast, outliers = outliers))
}

ACR <- function(p, n, alpha = .05){
    #'a function for fingind the upper percentile critical value for the test of a single multivariate outlier.  Wilks (1963) and approaching f distributions function by Yang and Lee (1987)
    #'@param p number of independent variables (features)
    #'@param n sample size
    #'@param alpha the significance level
    #'@return the critical value for the maximum squared mahalanobis distance
    f_inv <- qf(1-alpha/n, p, n-p-1) # p and n-p-1 DOF
    f_crit <- (p*(n-1)^2*f_inv)/(n*(n-p-1)+(n*p*f_inv))
    # equivalent to 
    # ((-1*(1/(1+(f_inv*p/(n-p-1))))-1) * ((n-1)^2))/n
    return(f_crit)
}