anova_stata <- function(X, y, corrected=TRUE, intercept = TRUE){
    #'@X a matrix of input data using the centered or uncentered information with observationsXfeatures
    #'@y a single vector of observed y values (same length as rows of X)
    #'@alpha the level of
    #'@corrected if true uses the corrected total sum of squares, else uses uncorrected total
    #'@intercept adjusts the calculation of sum of squares based on estimating intercept or not
    #'@return a list containing the anova table, intercepts, var-covar matrix, errors, standard errors, and estimated variance

    X <- as.matrix(X)
    XtXi <- pracma::pinv(t(X) %*% X)  # (X'X)^-1
    beta_hat <- XtXi %*% t(X) %*% y # Hy
    yhat <- X %*% beta_hat
    errors <- y - yhat
    ssres <- sum(errors^2)

    if(corrected){
        # use correct sums of squares
        if(intercept){
            sst <- sum(y^2)-length(y)*mean(y)^2
            ssreg <- t(yhat)%*%y - length(y)*mean(y)^2
            DOF <- c(length(beta_hat)-1, length(y)-1-(length(beta_hat)-1),length(y)-1)
        }else{
            # DOF different if no intercept
            sst <- sum(y^2)-length(y)*mean(y)^2
            ssreg <- t(yhat)%*%y - length(y)*mean(y)^2
            DOF <- c(length(beta_hat), length(y)-1-(length(beta_hat)),length(y)-1)
        }
    }else{
        # use uncorrected sums of squares
        sst <- sum(y^2)
        ssreg <- t(yhat)%*%y
        DOF <- c(length(beta_hat), length(y)-(length(beta_hat)),length(y))
    }
    SS <- c(ssreg, ssres, sst) # vector of SS
    MS <- c(SS/DOF) # divide by DOF
    F0 <- c(MS[1]/MS[2]) # claculate F0
    p <- c(pf(F0[1], DOF[1], DOF[2], lower.tail = FALSE), NA, NA) # p-value
    sighat <- ssres/(length(y)-length(beta_hat)) # same as MSE
    b_std_errors <- sqrt(diag(XtXi)*sighat) # calculate coefficient standard errors

    # put the above into a table
    MS[3] <- NA
    F0 <- c(F0, NA, NA)
    at <- data.frame(SS, DOF, MS, F0, p, row.names = c('regression', 'residual', 'total'))

    return(list(anova=at, coeffs=beta_hat, predicted=yhat, resids=errors, var_covar=XtXi, coef_stderrors= b_std_errors, sighat = sighat))

}


