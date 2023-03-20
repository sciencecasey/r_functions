
jackknife_t_dot_bar <- function(dat, group_size, theta_functions){
    r <- ceiling(length(dat)/group_size)
    t_min_j <- matrix(ncol=r, nrow=length(theta_functions))
    if(!is.null(names(theta_functions))){
        rownames(t_min_j) <- names(theta_functions)
    }
    no_group_end <- 1
    for(i in 1:ncol(t_min_j)){
        no_group <- no_group_end:(no_group_end+group_size-1)
        for(j in 1:length(theta_functions)){
            t_min_j[j, i] <- theta_functions[[j]](dat[-no_group])
        }
        no_group_end <- no_group[length(no_group)]+1
    }
    t_dot_bar <- rowMeans(t_min_j)
    return(list(t_min_j =t_min_j, t_dot_bar = t_dot_bar))
}
# dat <- '1.3574290 2.6322438 2.6095764 3.3271900 2.5347809 2.1883210 2.5305415 2.3900173 2.3419647 1.9397951 2.0917835 1.8135587 1.3802648 2.0513314 0.9792986 2.7211900 1.6969646 2.5137291 2.1483714 1.8956554 1.7212816 1.3675655 1.0324651 2.3321129 2.6312402 2.1970480 2.3065229 1.9986555 2.4436408 1.7305614 1.4904656 2.3948154 2.0592621 2.2182509 2.0629388 1.9174036 1.2853690 1.6342195 2.0733385 1.8307672 1.5456211 2.2839314 1.4603212 1.5879199 2.3890512 2.0459956 2.6434013 1.7186567 1.5678296 0.9930886 1.7914020 2.0696211 1.8695956 2.3381967 2.0460529 2.3906329 2.9427357 2.0090586 1.5037948 1.9296335 2.1639874 1.7911893 1.1570131 2.1397387 1.3755089 1.8713208 2.0893267 2.2886844 1.1056039 2.0786053 2.4481403 1.2996872 1.9007019 2.0786233 2.1578095 1.8466725 1.7684522 2.3732928 0.9962648 2.2350401 1.5111431 1.3004200 2.6429312 1.3266264 1.7407923 2.3311205 2.1786840 1.8406327 2.1415052 1.5633681 2.5143516 0.7743915 1.9294857 1.3849407 1.7597190 2.2631869 2.4100753 2.8267039 1.3508010 2.2391369'
# dat <- str_split(dat, ' ')[[1]]
# dat <- as.double(dat)
# jackknife_t(dat, 5, list(mu = mean, sigma2 = var))

#jackknife_t_dot_bar(dat = x, group_size = 1, theta_functions = list(b2))

jackknife_psuedovalues <- function(t_minus_j, t){
    #'@param t_minus_j: a vector of estimates for t without the jth group
    #'@param t: the original estimate for the theta parameter from full dataset.
    r <- length(t_minus_j)
    return(r*t - (r-1)*t_minus_j)
    
}

jackknife_t <- function(pseud){
    #'@param pseud: a vector of pseudovalues
    #'@return the mean of the psuedovalues for jackknifed t
    return(mean(pseud))
}


jackknife_variance <- function(pseud, t=NULL){
    #'@param t_minus_j: a vector of estimates for t without the jth group
    #'@param t: the original estimate for the variance from full dataset. assumed NULL in which case the mean of t_minus_j is used
    #'@return the jackknife variance calculated as sum((t_min_j - mean(t-min_j)ORt)^2)/r(r-1)
    if(!is.null(t)){
        # use original t estimate
        jack_t <- t
    }else{
        jack_t <- mean(pseud)   
    }
    var <- sum((pseud - jack_t)^2)
    var <- var/(length(pseud)*(length(pseud)-1))
    return(var)
}

jackknife_bias <- function(t_min_j, t, n){
    #'@param t_min_j a vector of t minus j estimated for T (where original groupsize is 1)
    #'@param t: original (full dataset) estimate for theta
    #'@param n: the size of original dataset
    return((n-1)*(mean(t_min_j)-t))
}

jackknife_bias_corrected_estimator <- function(t_dot_var, t, n){
    #'@param t_dot_bar: the t-j mean of estimators where groupsize was 1
    #'@param t: original (full dataset) estimate for theta
    #'@param n: the size of original dataset
    n*t-(n-1)*t_dot_bar
}