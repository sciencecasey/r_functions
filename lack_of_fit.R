lack_of_fit_anova <- function(X, y, intercept = T){
    #'@X a matrix of input data using the centered or uncentered information with observationsXfeatures
    #'@y a single vector of observed y values (same length as rows of X)
    #'@intercept adjusts the calculation of sum of squares based on estimating intercept or not
    #'@return a data frame of unique data across regressors and

    # source("~/opt/anaconda3/envs/m1_r/r_master_repo-main/functions/anova_statastyle.R")
    source("~/Learn_R/functions/anova_statastyle.R")

    # make initial anova table based on desired results
    mod <- anova_stata(X, y, intercept)

    # find where all regressors match
    match_rows <- apply(X, 1, function(x) paste0(x, collapse = ' '))
    unique_rows <- unique(match_rows)
    matching_rows <- list()
    for(item in 1:length(unique_rows)){
        # corresponding rows in the overall data
        data_rows <- which(match_rows==unique_rows[item])
        matching_rows[[item]] <- data_rows
    }

    # now the matching rows has lists of unique across rows data
    dat_compiled_full <- data.frame(x = unique_rows,
                                    SS = rep(NA, length(unique_rows)),
                                    DOF = rep(NA, length(unique_rows)))
    for(i in 1:length(matching_rows)){
        n <- length(matching_rows[[i]])
        mean <- mean(y[matching_rows[[i]]])
        SS <- sum((y[matching_rows[[i]]]-mean)^2)
        DOF <- n-1
        dat_compiled_full[i,-1] <- c(SS, DOF)
    }
    # calculate SS
    dat_compiled <- dat_compiled_full[dat_compiled_full$DOF != 0,]
    if(length(dat_compiled)==0){
        # no matching rows, can't do test
        warningCondition("No matching full rows of data- can't perform test")
        return(data = dat_compiled_full, model = mod)
    }

    SS_PE <- sum(dat_compiled$SS)
    DOF_SSPe <- sum(dat_compiled$DOF)
    LOF <- c(mod$anova$SS[2]-SS_PE,mod$anova$DOF[2]-DOF_SSPe,
             (mod$anova$SS[2]-SS_PE)/(mod$anova$DOF[2]-DOF_SSPe),
             NA, NA)
    mod$anova <- rbind(mod$anova[1,],
                       mod$anova[2,],
                       "lack of fit"=LOF,
                       "pure error"=c(SS_PE, DOF_SSPe, SS_PE/DOF_SSPe, NA, NA),
                       mod$anova[3,])
    mod$anova$F0[3] <- mod$anova$MS[3]/mod$anova$MS[4]
    mod$anova$p[3] <- pf(mod$anova$F0[3], mod$anova$DOF[3], mod$anova$DOF[4], lower.tail = F)

    return(list(unique_data = dat_compiled_full, model = mod))
}




lack_of_fit_anova_each_var <- function(X, y, intercept = T, regressor_names = NULL){
    #'@param X a matrix of input data using the centered or uncentered information with observationsXfeatures
    #'@param y a single vector of observed y values (same length as rows of X)
    #'@param intercept adjusts the calculation of sum of squares based on estimating intercept or not
    #'@param regressor_names a vector of regressor names for easy list navigation
    #'@return a nested list containing list of full model(with extended anova, intercepts, var-covar matrix, errors, standard errors, and estimated variance) and list of compiled data frame for each regressor unique values with repeat observations

    # source("~/opt/anaconda3/envs/m1_r/r_master_repo-main/functions/anova_statastyle.R")
    source("~/Learn_R/functions/anova_statastyle.R")

    SS_PE <- 0
    DOF_SSPe <- 0
    # across each regressor
    compiled_data_list <- list()
    if(intercept){
        start = 2
    }else{
        start = 1
    }

    # make initial anova table based on desired results
    mod_o <- anova_stata(X, y, intercept)

    for(regressor in start:ncol(X)){
        # fit single regressor model
        if(intercept){
            mod <- anova_stata(X[,c(1,regressor)], y)
        }else{
            mod <- anova_stata(X[,regressor], y)
        }
        x_j <- X[,regressor] # the regressor we're on
        itr <- unique(x_j)
        dat_compiled_full <- data.frame(x = itr,
                                        SS = rep(NA, length(itr)),
                                        DOF = rep(NA, length(itr)))
        for(value in itr){
            placement <- which(itr==value)
            n <- length(x_j[x_j==value]) # number of obs at this level
            mean <- mean(y[x_j==value]) # mean y at level of x == value
            SS <- sum((y[x_j==value]-mean)^2)
            DOF <- n-1
            dat_compiled_full[placement,-1] <- c(SS, DOF)
        }
        # calculate SS
        dat_compiled <- dat_compiled_full[dat_compiled_full$DOF != 0,]
        SS_PE <- SS_PE + sum(dat_compiled$SS)
        DOF_SSPe <- DOF_SSPe + sum(dat_compiled$DOF)
        if(length(itr)-length(mod$coeffs))
            LOF <- c(mod$anova$SS[2]-SS_PE,length(itr)-length(mod$coeffs),
                     (mod$anova$SS[2]-SS_PE)/(mod$anova$DOF[2]-DOF_SSPe),
                     NA, NA)
        mod$anova <- rbind(mod$anova[1,],
                           mod$anova[2,],
                           "lack of fit"=LOF,
                           "pure error"=c(SS_PE, DOF_SSPe, SS_PE/DOF_SSPe, NA, NA),
                           mod$anova[3,])
        mod$anova$F0[3] <- mod$anova$MS[3]/mod$anova$MS[4]
        mod$anova$p[3] <- pf(mod$anova$F0[3], mod$anova$DOF[3], mod$anova$DOF[4], lower.tail = F)

        compiled_data_list[[regressor-start+1]] <- list(compiled_data=dat_compiled, model=mod)
        # compiled_data_list$regressor <- list(dat_compiled, mod)
    }

    if(!is.null(regressor_names)){
        if(length(compiled_data_list)==length(regressor_names)){
            names(compiled_data_list) = regressor_names
        }
    }
    return(list(each_regressor_list = compiled_data_list, original_model=mod_o))

}

ss_pureError <- function(X, y, intercept = T, include_regressor_tables = F, model = NA){
    #'@X a matrix of input data using the centered or uncentered information with observationsXfeatures
    #'@y a single vector of observed y values (same length as rows of X)
    #'@param intercept adjusts DOF appropriately and SS calculations for regression if using include regressor tables
    #'@param include_regressor_tables if true calls lack_of_fit_anova.exhaustive
    #'@param model if included and include_regressor_tables is false will also include the model with the appended table (it is expected that this model was created using same X, y, corrected, and intercept)
    #'@return if not including regressor tables: returns the SS_pure_error, DOF_PE, OR if model included as argument, returns the model with SS_PE and SS_LOF, their mean squares, and F and p statistics appended to the anova table

    if(include_regressor_tables){
        lack_of_fit_anova_exhaustive(X, y, intercept)
    }

    if(intercept){
        start = 2
    }else{
        start = 1
    }
    SS_PE <- 0
    DOF_SSPe <- 0
    for(regressor in start:ncol(X)){
        x_j <- X[,regressor] # the regressor we're on
        itr <- unique(x_j)
        dat_compiled_full <- data.frame(x = itr,
                                        SS = rep(NA, length(itr)),
                                        DOF = rep(NA, length(itr)))
        for(value in itr){
            placement <- which(itr==value)
            n <- length(x_j[x_j==value]) # number of obs at this level
            mean <- mean(y[x_j==value]) # mean y at level of x == value
            SS <- sum((y[x_j==value]-mean)^2)
            DOF <- n-1
            dat_compiled_full[placement,-1] <- c(SS, DOF)
        }
        # calculate SS
        SS_PE <- SS_PE + sum(dat_compiled_full$SS)
        DOF_SSPe <- DOF_SSPe + sum(dat_compiled_full$DOF)
    }


    if(!is.na(model)){
        model$anova <- rbind(model$anova[1,],
                             model$anova[2,],
                             "lack of fit" = c(model$anova$SS[2]-SS_PE,
                                               model$anova$DOF[2]-DOF_SSPe,
                                               (model$anova$SS[2]-SS_PE)/(model$anova$DOF[2]-DOF_SSPe),
                                               NA, NA),
                             "pure error"=c(SS_PE, DOF_SSPe, SS_PE/DOF_SSPe, NA, NA),
                             model$anova[3,])
        model$anova$F0[3] <- model$anova$MS[3]/model$anova$MS[4]
        model$anova$p[3] <- pf(model$anova$F0[3], model$anova$DOF[3], model$anova$DOF[4], lower.tail = F)
        return(model)
    }
    return(list(SS_pure_error=SS_PE, DOF_pe=DOF_SSPe))
}



# mod <- lack_of_fit_anova)_exhaustive(matrix(c(rep(1, nrow(dat)), dat$x), nrow = nrow(dat)), dat$y)
#mod$compiled_data_frames
#ss_pureError(matrix(c(rep(1, nrow(dat)), dat$x), nrow = nrow(dat)), dat$y)
#mod$full_model$anova