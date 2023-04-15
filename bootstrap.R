bootstrap <- function(dat, num_draws=1000, estimator=mean){
    #' nonparametric bootstrap function
    #'@param dat single vector of input data
    #'@param num_draws the number of draws to take for the nonparametric bootstrapped sample
    #'@param estimator default mean of data
    #'@return a list of t_hat estimators from the bootstrapped samples, the observed number of times each t_hat estimator was returned and the probability of each t_hat based on the observations. 
    t_hat <- c()
    n <- 1
    while(n<num_draws){
        new_dat <- sample(dat, length(dat), replace=TRUE)
        t_hat <- append(t_hat, estimator(new_dat))
        n <- n+1
    }
    # put list in order of t_hat
    t_hat <- t_hat[order(t_hat)]
    # get estimated probabilities
    uniq_t <- unique(t_hat)
    obs_uniq_t <- sapply(uniq_t, function(t) sum(t_hat==t))
    bias <- 2*estimator(dat)-mean(t_hat)
    return(list(boot_est = mean(t_hat), 
                t_hat = t_hat,
                uniq_t = uniq_t,
                obs_uniq_t=obs_uniq_t,
                p_uniq_t = obs_uniq_t/length(uniq_t),
                theta_bias=bias))
}
#dat <- c(1,2,6)
#bootstrap(dat)

bootstrap.parametric <- function(dat, 
                                 f_dist=function(x) rnorm(x, mean=mean(dat), sd=1), 
                                 num_draws=1000, 
                                 estimator=mean){
    t_hat <- c()
    n <- 1
    while(n<num_draws){
        new_dat <- f_dist(length(dat))
        t_hat <- append(t_hat, estimator(new_dat))
        n <- n+1
    }
    # put list in order of t_hat
    t_hat <- t_hat[order(t_hat)]
    # get estimated probabilities
    uniq_t <- unique(t_hat)
    obs_uniq_t <- sapply(uniq_t, function(t) sum(t_hat==t))
    bias <- 2*estimator(dat)-mean(t_hat)
    return(list(boot_est = mean(t_hat), 
                t_hat = t_hat,
                uniq_t = uniq_t,
                obs_uniq_t=obs_uniq_t,
                p_uniq_t = obs_uniq_t/length(uniq_t),
                theta_bias=bias))
    
    
}
#dat <- rnorm(100, mean=3, sd=1)
#mean(bootstrap.parametric(dat)$uniq_t)
#mean(dat)
#

bootstrap_variance <- function(t_estimate){
    #'@param t_estimate a vector of estimates for theta created using bootstrapping
    #'@return the variance of theta
    v <- sum((t_estimate-mean(t_estimate))^2)
    return(1/(length(t_estimate)-1)*v)
}

#bootstrap_variance(boot$t_hat)
#var(boot$t_hat)

bootstrap_lm <- function(dat, 
                         mod=function(d) lm(y~., data=d),
                         estimator=function(beta) beta[2]/beta[1], 
                         num_draws = 1000,
                         return_betas = FALSE){
    #'@param dat: a dataframe with the predictor and respons variables
    #'@param mod: a linear or glm models from which to draw coefficients
    #'@param estimator: a function of the coefficients returned from mod
    #'@param num_draws: number of bootstrapped samples to draw
    #'@return a list with the overall bootstrap estimate, all t_hats, rounded t_hat observations and probabilities, and the estimator's bias
    beta <- mod(dat)$coefficients
    t_0 <- estimator(beta)
    t_hat <- c()
    n <- 1
    if(return_betas){
        b <- c(beta)
        while(n<num_draws){
            indx <- sample(1:nrow(dat), replace = TRUE)
            new_dat <- dat[indx,]
            beta <- mod(new_dat)$coefficients
            b <- rbind(b, beta)
            t_hat <- append(t_hat, estimator(beta))
            n <- n+1
        }
        # put list in order of t_hat
        t_hat <- t_hat[order(t_hat)]
        return(list(boot_est = mean(t_hat), 
                    t_hat = t_hat,
                    beta=b))
    }
    
    while(n<num_draws){
        indx <- sample(1:nrow(dat), replace = TRUE)
        new_dat <- dat[indx,]
        beta <- mod(new_dat)$coefficients
        t_hat <- append(t_hat, estimator(beta))
        n <- n+1
    }   
    # put list in order of t_hat
    t_hat <- t_hat[order(t_hat)]
    # get estimated probabilities
    uniq_t <- unique(t_hat)
    obs_uniq_t <- sapply(uniq_t, function(t) sum(t_hat==t))
    bias <- 2*t_0-mean(t_hat)
    return(list(boot_est = mean(t_hat), 
                t_hat = t_hat,
                uniq_t = uniq_t,
                obs_uniq_t=obs_uniq_t,
                p_uniq_t = obs_uniq_t/length(uniq_t),
                theta_bias=bias))
    
}

#dat <-  read_delim('~/OneDrive/JHU/computational_statistics/textbook_resources/datasets/alloy.dat')
#boot <- bootstrap_lm(dat, mod=function(d) lm(corrosionloss~ironcontent, d))



bootstrap_ci <- function(bootstrap_obj, alpha=.05){
    # get the .05 value
    p <- cumsum(bootstrap_obj$p_uniq_t)
    s_indx <- which(p>=alpha/2)[1]
    e_indx <- which(p<=(1-alpha/2))
    e_indx <- e_indx[length(e_indx)]
    return(c(bootstrap_obj$uniq_t[s_indx], bootstrap_obj$uniq_t[e_indx]))
}

#bootstrap_ci(boot)
#' bootstrap_variance.two_vars <- function(betas){
#'     #'@param t_estimate a vector of estimates for theta created using bootstrapping
#'     #'@return the variance of theta
#'     beta_0 <- betas[1,]
#'     betas <- betas[-1,]
#'     (beta_0[2]/beta_0[1])^2*(var(betas[,2])/beta_0[2]^2+var(betas[,1])/beta_0[1]^2-2*cov(betas)[2,2]/(beta_0[1]*beta_0[2]))
#' }

#boot <- bootstrap_lm(dat, mod=function(d) lm(corrosionloss~ironcontent, d), return_betas = TRUE)
#sqrt(bootstrap_variance.two_vars(boot$beta))

# bootstrap_ci(boot)
# 
bootstrap_studentsT <- function(bootstrap_call, t_hat, alpha=.05){
    # bootstrap a variance
    v <- eval(bootstrap_call)
    v <- bootstrap_variance(v$t_hat)
    #v <- var(v$t_hat)
    # now bootstrap the bootstrap stat
    t <- eval(bootstrap_call)
    # now get the boot T stat
    boot_t_est <- (t$boot_est-t_hat)/sqrt(v)
    # get the top and bottom of CI
    ci <- bootstrap_ci(t, alpha)
    return(list(confidence_int =c(t_hat-sqrt(v)*ci[1], t_hat+sqrt(v)*ci[2]),
           variance = v,
           boot_t_est = boot_t_est))
}
#sqrt(bootstrap_variance(boot$t_hat))
t_0 <- lm(corrosionloss~ironcontent, dat)$coefficients[2]/lm(corrosionloss~ironcontent, dat)$coefficients[1]
bootstrap_studentsT(bootstrap_lm(dat, 
                                 mod=function(d) lm(corrosionloss~ironcontent, d)),
                    t_0) #/sqrt(bootstrap_variance(boot$t_hat))*.00273


bootstrap_balanced <- function(dat, num_samples=1000, estimator=mean){
    # make sure that there is a representation of the data each time
    permuted <- as.data.frame(modelr::permute(data.frame(dat), num_samples, dat)$perm)
    # concatenate into a vector
    temp <- c(t(permuted))
    # randomly separate back into samples
    for(i in 1:num_samples){
        indx <- sample(1:length(temp), length(dat))
        permuted[,i] <- temp[indx]
        temp <- temp[-indx]
    }
    t_hat <- as.vector(apply(permuted, 2, estimator))
    # get estimated probabilities
    uniq_t <- unique(t_hat)
    obs_uniq_t <- sapply(uniq_t, function(t) sum(t_hat==t))
    bias <- 2*estimator(dat)-mean(t_hat)
    return(list(boot_est = mean(t_hat), 
                t_hat = t_hat,
                uniq_t = uniq_t,
                obs_uniq_t=obs_uniq_t,
                p_uniq_t = obs_uniq_t/length(uniq_t),
                theta_bias=bias))
    
}
dat <- rnorm(100)
boot <- bootstrap_balanced(dat, num_samples = 10)
mean(dat)
