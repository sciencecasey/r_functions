effective_sample_size <- function(dat, truncated=FALSE, burn_in=FALSE){
    #'@param dat: a vector of estimated sampled from MCMC
    #'@param truncated: should the estimate remove acf < .1
    #'@param burn_in: numeric of how many samples to skip. Defaults false assuming burn-in already removed
    #'@return the estimated sample size after burn-in removed.  For fixed iterations, larger sample size suggests quicker convergence
    if(burn_in>0){
        dat <- dat[(burn_in+1):length(dat)]
    }
    if(truncated){
        indx <- which(acf(out, plot=FALSE)$acf>=.1)
        autocorr_time <- 1+2*sum(acf(dat, plot=FALSE)$acf[indx])
        return(length(dat)/autocorr_time)
    }
    autocorr_time <- 1+2*sum(acf(dat, plot=FALSE)$acf)
    return(length(dat)/autocorr_time)
    
}
#effective_sample_size(out, burn_in = 200)
#effective_sample_size(out, truncated = TRUE, burn_in = 200)
