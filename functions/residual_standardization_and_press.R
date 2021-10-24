resid_variance <- function(X, e=NULL, yhat=NULL, y=NULL){
  #'takes either a vector of errors or a vector of predicted and observed values and returns the variance of associated errors
  #'@param X a matrix of input regressors (with 1s if intercept in model) from which to compute the hat matrix
  #'@param e a vector of residuals
  #'@param yhat a vector of predicted y values
  #'@param y a vector of observed y values
  #'@return a vector of associated variances
  if(is.null(e)){
    if(is.null(yhat) | is.null(y)){
      errorCondition("Must include either y and yhat or error vector")
    }else{
      e <- y-yhat
    }
  }
  hat <- X%*%solve(t(X)%*%X)%*%t(X)
  return((1/length(e)*sum(e^2)*(1-diag(hat))))
}

resid_cov <- function(X, e=NULL, yhat=NULL, y=NULL){
  #'takes either a vector of errors or a vector of predicted and observed values and returns the covariance of associated errors
  #'@param X a matrix of input regressors (with 1s if intercept in model) from which to compute the hat matrix
  #'@param e a vector of residuals
  #'@param yhat a vector of predicted y values
  #'@param y a vector of observed y values
  #'@return a matrix of covariances 
  if(is.null(e)){
    if(is.null(yhat) | is.null(y)){
      errorCondition("Must include either y and yhat or error vector")
    }else{
      e <- y-yhat
    }
  }
  hat <- X%*%solve(t(X)%*%X)%*%t(X)
  cov <- -1/length(e)*sum(e^2)*hat
  diag(cov) <- 1
  return(cov)
}

resid_standard <- function(e=NULL, yhat=NULL, y=NULL){
  #'@param e a vector of residuals
  #'@param yhat a vector of predicted y values
  #'@param y a vector of observed y values
  #'@return a vector of standardized residuals by error/sqrt(MS_res)
  if(is.null(e)){
    if(is.null(yhat) | is.null(y)){
      errorCondition("Must include either y and yhat or error vector")
    }else{
      e <- y-yhat
    }
  }
  msres <- 1/length(e)*sum(e^2)
  return(e/sqrt(msres))
} 

resid_student <- function(X, e=NULL, yhat=NULL, y=NULL){
  #' should yield the same result as rstandard()
  #'@param X a matrix of regressors (including 1's as constant for intercept if included)
  #'@param e a vector of residuals
  #'@param yhat a vector of predicted y values
  #'@param y a vector of observed y values
  #'@return a vector of studentized residuals by error/sqrt(MS_res(1-h_ii)) where every residual has a constant variance = 1
  
  if(is.null(e)){
    if(is.null(yhat) | is.null(y)){
      errorCondition("Must include either y and yhat or error vector")
    }else{
      e <- y-yhat
    }
  }
  hat <- X%*%solve(t(X)%*%X)%*%t(X)
  msres <- 1/length(e)*sum(e^2)
  return(e/sqrt(msres*(1-diag(hat))))
} 

prediction_error_resid <- function(X,e=NULL, yhat=NULL, y=NULL, standardize = F){
  #'@param X a matrix of regressors (including 1's as constant for intercept if included)
  #'@param e a vector of residuals
  #'@param yhat a vector of predicted y values
  #'@param standardize default F: do you want to standardize the errors as well? 
  #'@param y a vector of observed y values
  #'@return a vector of prediction errors, note that if standardize is set to true this returns the same value as the studentized function
  if(standardize){
    resid_student(X, e, yhat, y)
  }
  if(is.null(e)){
    if(is.null(yhat) | is.null(y)){
      errorCondition("Must include either y and yhat or error vector")
    }else{
      e <- y-yhat
    }
  }
  hat <- X%*%solve(t(X)%*%X)%*%t(X)
  return(e/(1-diag(hat)))
}

press_stat <- function(press_resids = null){
  #'@param press_resids a vector of already press residuals (function names prediction error residuals)
  #'@return a statistic for the PRESS sum of squares
  
  return(sum(press_resids^2))
}
r2_prediction_stat <- function(PRESS, y, corrected = T){
  #'@param PRESS the press statistic calculated from SS of predicted error residuals
  #'@param y a vector of observed y values (from which to calculate total corrected SS)
  #'@param corrected if true, calculate corrected SS total 
  #'@return the prediction r squared stat for the predictive capability of model
  
  if(!corrected){
    SS_T <- sum(y^2)
  }else{
    SS_T <- sum(y^2)-length(y)*mean(y)^2
  }
  return(1 - (PRESS/SS_T))
}

resid_rstudent <- function(X,e=NULL, yhat=NULL, y=NULL){
  #' an externally studentized residual using estimated variance as though each ith error term isn't included in the model
  #' should yield same result as rstudent() of MASS::studres()
  #'@param X a matrix of regressors (including 1's as constant for intercept if included)
  #'@param e a vector of residuals
  #'@param yhat a vector of predicted y values
  #'@param y a vector of observed y values using leave one out MSE estimator
  if(is.null(e)){
    if(is.null(yhat) | is.null(y)){
      errorCondition("Must include either y and yhat or error vector")
    }else{
      e <- y-yhat
    }
  }
  hat <- X%*%solve(t(X)%*%X)%*%t(X)
  msres <- 1/length(e)*sum(e^2)
  s2 <- ((nrow(X)-ncol(X))*msres-e^2)/(1-diag(hat))
  s2 <- s2/(nrow(X)-ncol(X)-1)
  return(e/sqrt(s2*(1-diag(hat))))
}
