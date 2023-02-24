newtons_method <- function(gx, respect_to, inputs, sum=FALSE, check_individual = FALSE){
  #'@param gx: an expression object with only one variable, x
  #'@param respect_to: variable name for the gradient to be taken with respect to if single variate.  If multivariate, a character vector with each variable of interest inside
  #'@param inputs: a named vector of inputs and their starting or range of values
  #'@param sum: should the gradient be summed at x_t? (yes for more than one value as in MLE)
  #'@param check_individual: should each parameter be checked separately? Defaults false for frobenius norm (euclidian distance) check
  #'@return: a list with x_hat estimated value of x* based on newton's method and the number of iterations taken to converge
  #'@author: Casey Jayne
  
  if(check_individual){
      return(newtons_method.indv)
  }
  dxdg <- deriv3(gx, respect_to, function.arg=names(inputs))
  if(sum){
    h <- do.call(dxdg,inputs) 
    h_t <- matrix(-colSums(attr(h, 'gradient')) %*% pracma::pinv(colSums(attr(h, 'hessian'))))
    rownames(h_t) <- respect_to
  }else{
    h <- do.call(dxdg,inputs) 
    h_t <- matrix(-attr(h, 'gradient') %*% pracma::pinv(colSums(attr(h, 'hessian'))))
    rownames(h_t) <- respect_to
  }
  iter <- 1
  while(TRUE){
    x_next <- as.numeric(inputs[respect_to]) + h_t
    # traditional convergence check
    if(sum((as.numeric(x_next)-as.numeric(inputs[respect_to]))^2)<.0001){
        return(list(theta_hat = inputs[respect_to], convergence_steps = iter))
    # if(norm(as.matrix(as.numeric(x_next)-as.numeric(inputs[respect_to])), 'F')<.0001){
    #     return(list(theta_hat = inputs[respect_to], convergence_steps = iter))
    }else if(iter > 10000){
        return('Failed to converge')
    }
    inputs[respect_to] <- unname(x_next)
    if(sum){
        h <- do.call(dxdg,inputs)
        h_t <- matrix(-colSums(attr(h, 'gradient')) %*% pracma::pinv(colSums(attr(h, 'hessian'))))
        rownames(h_t) <- names(colSums(attr(h, 'gradient')))
        h_t <- h_t[respect_to,] # only grab items of interest at this iteration
    }else{
         h <- do.call(dxdg,inputs)
         h_t <- matrix(-attr(h, 'gradient') %*% pracma::pinv(colSums(attr(h, 'hessian'))))
         rownames(h_t) <- names(colSums(attr(h, 'gradient')))
         h_t <- h_t[respect_to,] # only grab items of interest at this iteration
    }
    iter <- iter + 1
  }

}

newtons_method.individual <- function(gx, respect_to, inputs, sum=FALSE, check_individual = TRUE){
    #'@param gx: an expression object with only one variable, x
    #'@param respect_to: variable name for the gradient to be taken with respect to if single variate.  If multivariate, a character vector with each variable of interest inside
    #'@param inputs: a named vector of inputs and their starting or range of values
    #'@param sum: should the gradient be summed at x_t? (yes for more than one value as in MLE)
    #'@param check_individual: should each parameter be checked separately? Defaults false for frobenius norm (euclidian distance) check
    #'@return: a list with x_hat estimated value of x* based on newton's method and the number of iterations taken to converge
    #'@author: Casey Jayne
    final_out <- matrix(-999, nrow=length(respect_to))
    rownames(final_out) <- respect_to
    final_it <- matrix(0, nrow=length(respect_to))
    rownames(final_it) <- respect_to
    dxdg <- deriv3(gx, respect_to, function.arg=names(inputs))
    if(sum){
        h <- do.call(dxdg,inputs) # h <- eval(dxdg, as.list(inputs))
        #h_t <- matrix(-colSums(attr(h, 'gradient'))/rowSums(colSums(attr(h, 'hessian'))))
        h_t <- matrix(-colSums(attr(h, 'gradient')) %*% pracma::pinv(colSums(attr(h, 'hessian'))))
        rownames(h_t) <- respect_to
    }else{
        h <- do.call(dxdg,inputs) # h <- eval(dxdg, as.list(inputs))
        h_t <- matrix(-attr(h, 'gradient') %*% pracma::pinv(colSums(attr(h, 'hessian'))))
        #h_t <- matrix(-attr(h, 'gradient')/rowSums(attr(h, 'hessian')))
        rownames(h_t) <- respect_to
    }
    iter <- 1
    while(TRUE){
        x_next <- as.numeric(inputs[respect_to]) + h_t
        
        if(all(is.nan(x_next))){
            warningCondition(paste0('Fail to converge on ', respect_to))
            final_it[which(final_it==0)] <- iter
            return(list(estimates = final_out, iterations = final_it))
        }
        for(i in respect_to){
            if(tryCatch(abs(x_next[i,] - as.numeric(inputs[i])) < .0001, error=function(e) abs(x_next[i] - as.numeric(inputs[i])) < .0001)){
                # append to final
                final_out[i,] <- tryCatch(x_next[i,], error=function(e) x_next[i])
                final_it[i,] <- iter
                # set constant value to input
                inputs[i] <- tryCatch(x_next[i,], error=function(e) x_next[i])
                # rename the final_out column
                rownames(final_out)[which(rownames(final_out)==i)] <- paste0(i, '_hat')
                # remove from lists
                x_next <- tryCatch(x_next[-which(rownames(x_next)==i),], error=function(e) x_next[-which(rownames(x_next)==i)])
                respect_to <- respect_to[-which(respect_to==i)]
                if(length(respect_to)<1){
                    # all items have been finalized
                    final_out <- apply(final_out, 2, function(x) round(x, digits=2))
                    return(list(estimates = final_out, iterations = final_it))
                }
            }
        }
        inputs[respect_to] <- unname(x_next)
        if(sum){
            h <- do.call(dxdg,inputs)
            #h <- dxdg(inputs) # h <- eval(dxdg, as.list(inputs))
            h_t <- matrix(-colSums(attr(h, 'gradient')) %*% pracma::pinv(colSums(attr(h, 'hessian'))))
            #h_t <- matrix(-colSums(attr(h, 'gradient'))/rowSums(colSums(attr(h, 'hessian'))))
            rownames(h_t) <- names(colSums(attr(h, 'gradient')))
            h_t <- h_t[respect_to,] # only grab items of interest at this iteration
        }else{
            h <- do.call(dxdg,inputs)
            h_t <- matrix(-attr(h, 'gradient') %*% pracma::pinv(colSums(attr(h, 'hessian'))))
            #h_t <- matrix(-attr(h, 'gradient')/rowSums(attr(h, 'hessian')))
            rownames(h_t) <- names(colSums(attr(h, 'gradient')))
            h_t <- h_t[respect_to,] # only grab items of interest at this iteration
            #rownames(h_t) <- respect_to
        }
        iter <- iter + 1
    }
    
}


eq <- expression(log(x)/(1+x))
#newtons_method(eq, .5)

newtons_method(ll, respect_to=c('alpha1', 'alpha2'), inputs =list(y=oil_dat$spills, n=26, b1=oil_dat$importexport, b2=oil_dat$domestic, alpha1=1, alpha2=1), sum=TRUE)

newtons_method(ll, inputs = list(mu=mean(dat), sigma=sd(dat), x=dat), sum = TRUE, respect_to = c('mu', 'sigma'))

newtons_method(ll, inputs = list(mu=mean(dat), sigma=1, x=dat), sum = TRUE, respect_to = c('mu', 'sigma'))


newtons_method(ll, inputs = list(mu=mean(dat), sigma=25, x=dat), sum = TRUE, respect_to = c('mu', 'sigma'))



