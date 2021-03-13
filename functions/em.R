em <- function(data, clusters, p0 = uniform, mixture = FALSE, bayes = FALSE){
    if(!mixture & !bayes){
        em.simple(...)
    }
}
em.simple <- function(data, clusters, p0 = uniform){
    #' @param data a matrix or df of numeric values over which to conduct the EM algorithm. Any features need to be columns and observations rows
    #' @param clusters the number of clusters to organize the data into
    #' @param p0 a vector of initial probabilities in order of the columns in data. Otherwise, uniform probability initially used
    mu0 <- colMeans(data)
    sd0 <- sapply(data, sd)
    # our starting cluster points
    k0 <- runif(clusters)*sd0 + mu0 
    sd0 <- mean(sd0) # aggregate the sds to one value
    # uniform initial probs
    if(p0 == "uniform"){
        p0 <- 1/cols    
    }else if(!is.numeric(p0) | length(p0) != cols){
        errorCondition("Initial probability should bea numeric vector with same amount of items as columns in data")
    }
    epsilon <- .00001
    diff <- 1 # as long as bigger than epsilon
    while(diff > epsilon){
        # E step
        e <- estep(data, mu0, k0, sd0)
        # M step
        m <- mstep(data, e)
        # calculate change for stopping case
        dif <- c(mu0 - m[[2]], sd0 - m[[3]])
        # save new priors
        p0 <- m[[1]]
        mu0 <- m[[2]]
        sd0 <- m[[3]]
    }
    # return the expectations and their associated statistics
    return(list(e, m))
}

estep <- function(data, mu0, k0, sd0){
    e <- c() # initialize probability vector
    for(d in seq(1, length(k0))){
        # calculate the gaussian function for the cluster
        cols <- dim(data)[2]
        pre <- 1/(sqrt(2*pi)*sd0)^cols
        post <- exp(-.5*((data[,d] - mu0[d])/(sd0[d]))^2)
        g <- pre*post 
        # note, these 3 steps can be simplified using dnorm(data, mu0, sd0)
        e <- cbind(e, p0*g) # make a data frame for each cluster
    }
    # calculate the membership probability numerator
    e$memb <- rowSums(e)
    e$prob <- e/memb # calculate the full conditional probability for each point in each cluster
    return(e)
}

mstep <- function(data, e){
    clus <- length(e[1,])-1 # get number of clusters
    new_prior <- colSums(e[,-length(e[1,])])/length(e[,1]) # mean of each cluster's probabilities
    new_cluster_mu <- c()
    new_sd <- c()
    for(i in seq(1, clus)){
        new_cluster_mu <- c(new_cluster_mu, (sum(e[,i])*data[,i])/sum(e[,i]))
        # append the sd of each cluster together
        newsd <- c(new_sd, sum(e[,i]*(data[,i] - new_cluster_mu[i])^2) / sum(e[,i]))
    }
    return(list(pi = new_prior,mean = new_cluster_mu, sd =new_sd))
}