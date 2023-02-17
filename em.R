## haven't finished writing out mixture portion or bayes portion

# source('~/Learn_R/functions/bayesClassify.R')
em <- function(data, clusters, p0 = uniform, mixture = FALSE, bayes = FALSE, epsilon = .00001){
    #'a generic function to classify into expectation maximizations
    #' @param data a matrix or df of numeric values over which to conduct the EM algorithm. Any features need to be columns and observations rows
    #' @param clusters the number of clusters to organize the data into
    #' @param p0 a vector of initial probabilities in order of the columns in data. Otherwise, uniform probability initially used
    #' @param mixture if TRUE uses covariance matrix to calculate across features rather than individual feature SD for the gaussian models
    #' @param bayes adds an optional bayesian classifying step after the EM
    #' @param epsilon to create a custom convergence value (must be greater than zero)
    #' @return a list with the first item a data frame of expectation probabilities and second item the maximization parameter estimates for those estimates and third item the difference between the two values (which was less than epsilon)

    if(!mixture & !bayes){
        return(em.simple(...))
    }else if (!mixture){
        e = em.simple(...)
        return(bayes_classifier(e, data))
    }else if (mixture & bayes){
        e = em.mixture(...)
        return(bayes_classifier(e, data))
    }else{
        return(em.mixture(...))
    }
}
em.simple <- function(data, clusters, p0 = "uniform", epsilon = .00001){
    #' @param data a matrix or df of numeric values over which to conduct the EM algorithm. Any features need to be columns and observations rows
    #' @param clusters the number of clusters to organize the data into
    #' @param p0 a vector of initial probabilities in order of the columns in data. Otherwise, uniform probability initially used
    #' @param epsilon to create a custom convergence value (must be greater than zero)
    #' @return a list with the first item a data frame of expectation probabilities and second item the maximization parameter estimates for those estimates and third item the difference between the two values (which was less than epsilon)
    # check inputs
    if(round(clusters) <= 1 | round(clusters) != clusters){
        errorCondition("invalid cluster size")
    }
    if(p0 == "uniform"){
        # uniform initial probs
        p0 <- 1/clusters
    }else if(!is.numeric(p0) | length(p0) != clusters){
        errorCondition("Initial probability should be a numeric vector with same amount of items as columns in data")
    }
    if(epsilon <=0 ){
        warning("invalid epsilon input: ignored")
        epsilon <- .00001
    }
    # initilize starting points
    mu0 <- data.frame(colMeans(data)) # D dimension X 1 col df
    sd0 <- data.frame(apply(data, 2, sd)) # D dimension X 1 col df
    # expand to be D x K dimensions
    kdims <- matrix(rep(1, clusters)) # 1 X K matrix
    mu0 <- tcrossprod(as.matrix(mu0), kdims) # D X K matrix !
    p0 <- tcrossprod(as.matrix(p0), kdims) # D X K matrix !
    rand0 <- runif(clusters) # 1 X K
    # our starting cluster points
    k0 <- tcrossprod(as.matrix(sd0), as.matrix(rand0)) + mu0 # D (rows) X K (cols) matrix !
    sd0 <- rep(colMeans(sd0), clusters) # aggregate the sds to one value
    dif <- TRUE # as long as bigger than epsilon
    max_iter <- 1000
    runs <- 0
    while(dif && runs < max_iter){
        # E step
        e <- estep(data, k0, sd0, p0)
        # M step
        m <- mstep(data, e)
        # calculate change for stopping case
        delta <- c(abs(k0 - m[[2]]), abs(sd0 - m[[3]]))
        if(all(delta<epsilon)){
            # stopping case reached!
            dif = FALSE
        }
        # save new priors
        p0 <- m[[1]]
        k0 <- m[[2]]
        sd0 <- m[[3]]
        runs <- runs + 1
    }
    # return the expectations and their associated statistics
    return(list(expectations = e, clusters_m = m, runs = runs, delta = delta))
}

estep <- function(data, k0, sd0, p0){
    #'@param data frame with columns the obs X D values
    #'@param k0 initial latent D X K cluster values?
    #'@param sd0 initial sd of every K cluster (1 X K vector)
    #'@param p0 initial probabilities for every k cluster
    #'@return an obs X K cluster matrix of membership probabilitiesan obs X K cluster matrix of membership probabilities
    e <- c() # initialize probability vector
    dimensions <- dim(data)[2]
    clusters <- dim(k0)[2]
    # calculate the gaussian function for the cluster
    pre <- 1/(sqrt(2*pi)*sd0)^dimensions # gives a 1 X K pre
    for(cluster in seq(1, clusters)){
        post <- exp(-.5*(rowSums(t(t(data) - k0[,cluster])^2))/(sd0)^2) # gives obs X D post
        # above: subtract each data row from k0 column and square it, and add all those dimensional distances together
        g <- pre*post # 1 X obs gaussians
        pg <- t(crossprod(p0[cluster], g))
        e <- cbind(e, pg) # creates a observation X K prob matrix
    }
    # calculate the membership probability numerator
    denom <- rowSums(e)
    e <- e/denom # calculate the full conditional probability for each point in each cluster
    return(e) # an obs X K cluster matrix of membership probabilities
}

mstep <- function(data, e){
    #' @param data an observation X dimension data frame or matrix
    #' @param e an observation X K (cluster) matrix of membership probabilities
    new_mu <- t(crossprod(e, as.matrix(data))/colSums(e)) # D dimension X K cluster matrix
    new_sd <- c()
    new_prior <- colMeans(e)
    for(cluster in seq(1, dim(new_mu)[2])){
        norm <- colSums((t(data)-new_mu[,cluster])^2)
        new_sd <- c(new_sd, sqrt((sum(e[,cluster]*norm))/(2*sum(e[,cluster]))))
    }
    return(list(pi = new_prior, mean = new_mu, sd = new_sd))
}

# ifthe data iisn't invertible: (not a full rank matirx)
# means we have 2 features with the same variance
# if two features are highly correlated need to get rid of one of them
# feature rank the data and keep the higher classifier between the two
# another opeion could be single value decomposition but taht doesn't actually get rid of the issue with the original data
# then re-calculate the covariance matrix and it should be invertible
#
# Bayes Classifier vs. EM
# EM: we pre-define the number of clusters and then we assign to the clusters by the convergence
# after the EM is completed, we need to re-run the mean and covariance of each cluster that we use
# Bayes Classifier: the only thing we need is the probability of belonging to each class, the mean and covariance of that class (not the number of clusters, etc.) We get these either from the output of the EM or from the original dataset (if we want to use Naive Bayes)
