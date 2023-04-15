burn_in_run_assessment <- function(runs, burn_in = 1000, length_runs=nrow(runs)){
    #'@param runs: a single-parameter from MCMC run over multiple starting positions, each run concatenated colwise
    #'@param burn_in: the number of rows (observations) to remove as burn-in
    #'@return the statistics comparing the between and within variance of each MCMC run as well as the estimated overall variance across runs
    if(burn_in){
        runs <- runs[(burn_in+1):nrow(runs),]
    }
    if(is.data.frame(runs)){
        # chains should be in colums
        num_runs <- ncol(runs)
    }else{
        # assumed in list form
        errorCondition('Please make runs a dataframe with each run a column')
    }
    if(length_runs<nrow(runs)){
        # separate each into multiple chains
        num_per <- ceiling(nrow(runs)/length_runs)
        out_frame <- as.data.frame(matrix(ncol=(num_runs*num_per), nrow=2))
        run_frame <- as.data.frame(matrix(ncol=(num_runs*num_per), nrow=length_runs))
        cnames <- rep(sapply(1:num_runs, function(i) paste0('chain_number_', i)), each=num_per)
        temp <- rep(letters[1:num_per], num_runs)
        cnames <- sapply(1:length(temp), function(i) paste0(cnames[i], '_', temp[i]))
        colnames(out_frame) <- cnames
        colnames(run_frame) <- cnames
        # populate the run-frame with values
        for(j in 1:num_per){
            temp <- runs[(j*(1:length_runs)),]
            col_indx <- seq(j, ncol(run_frame), num_per)    
            run_frame[,col_indx] <- temp
        }
        runs <- run_frame
        # update number of runs
        num_runs <- ncol(runs)
    }else{
        out_frame <- as.data.frame(matrix(nrow=2, ncol=num_runs))    
        colnames(out_frame) <- sapply(1:num_runs, function(i) paste0('chain_number_', i))
    }
    rownames(out_frame) <- c('run_mean', 'within_variance')
    out_frame['run_mean',] <- unname(colMeans(runs))
    overall_mean <- mean(as.numeric(out_frame['run_mean',]))
    between_var <- (length_runs)/(num_runs-1)*sum((out_frame['run_mean',]-overall_mean)^2)
    out_frame['within_variance',] <- apply(runs, 2, var)
    overall_var <- mean(as.numeric(out_frame['within_variance',]))
    r <- ((length_runs-1)/length_runs*overall_var+between_var/length_runs)/overall_var
    r_hat <- r*(num_runs+1)/num_runs-(length_runs-1)/(num_runs*length_runs)
    return(list(run_stats = out_frame, 
                overall_mean = overall_mean,
                between_var = between_var,
                overall_var = overall_var,
                sqrt_r = sqrt(r),
                sqrt_r_hat = sqrt(r_hat)))
    
}
#dat <- as.data.frame(cbind(out_10, out_30, out_60, out_100, out_500))
#burn_in_run_assessment(dat)
burn_in_run_assessment(runs= dat[1:1000,] , burn_in = 0, length_runs = 500)

