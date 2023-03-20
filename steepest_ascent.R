newton_forced_ascent <- function(gx, respect_to, inputs, sum=FALSE){
    #'@param gx: an expression object with only one variable, x
    #'@param respect_to: variable name for the gradient to be taken with respect to if single variate.  If multivariate, a character vector with each variable of interest inside
    #'@param inputs: a named vector of inputs and their starting or range of values
    #'@param sum: should the gradient be summed at x_t? (yes for more than one value as in MLE)
    #'@return: a list with x_hat estimated value of x* based on newton's method and the number of iterations taken to converge
    #'@author: Casey Jayne
    
    
    dxdg <- deriv3(gx, respect_to, function.arg=names(inputs))
    if(sum){
        h <- do.call(dxdg,inputs) # h <- eval(dxdg, as.list(inputs))
        h_t <- matrix(-colSums(attr(h, 'gradient'))%*% pracma::pinv(colSums(attr(h, 'hessian'))))
        rownames(h_t) <- respect_to
    }else{
        h <- do.call(dxdg,inputs) # h <- eval(dxdg, as.list(inputs))
        h_t <- matrix(-attr(h, 'gradient')%*% pracma::pinv(colSums(attr(h, 'hessian'))))
        rownames(h_t) <- respect_to
    }
    iter <- 1
    while(TRUE){
        x_next <- as.numeric(inputs[respect_to]) + h_t
        alpha=1
        # traditional convergence check
        # calculate the ll values to check
        inputs_now <- inputs
        inputs_now[respect_to] <- unname(x_next)
        if(sum){
            ll_now <- sum(na.omit(eval(gx, inputs_now)))
            ll_prev <- sum(na.omit(eval(gx, inputs)))
        }else{
            ll_now <- eval(gx, inputs_now)
            ll_prev <- eval(gx, inputs)
        }
        while(ll_now-ll_prev<=0){
            # didn't move in negative direction
            alpha = alpha/2
            x_next <- as.numeric(inputs[respect_to]) + alpha*h_t
            inputs_now <- inputs
            inputs_now[respect_to] <- unname(x_next)
            if(sum){
                ll_now <- sum(na.omit(eval(gx, inputs_now)))
                ll_prev <- sum(na.omit(eval(gx, inputs)))
            }else{
                ll_now <- eval(gx, inputs_now)
                ll_prev <- eval(gx, inputs)
            }
            if(norm(as.matrix(as.numeric(x_next)-as.numeric(inputs[respect_to])), 'F')<.0001){
                return(list(theta_hat = x_next, convergence_steps = iter))
            }
            if(alpha == alpha/2^5000){
                return('No more descent')
            }
        }

        if(norm(as.matrix(as.numeric(x_next)-as.numeric(inputs[respect_to])), 'F')<.0001){
            return(list(theta_hat = inputs[respect_to], convergence_steps = iter))
        }else if(iter > 10000){
            return('Failed to converge')
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


steepest_ascent <- function(gx, respect_to, inputs, sum=FALSE){
    #'@param gx: an expression object with only one variable, x
    #'@param respect_to: variable name for the gradient to be taken with respect to if single variate.  If multivariate, a character vector with each variable of interest inside
    #'@param inputs: a named vector of inputs and their starting or range of values
    #'@param sum: should the gradient be summed at x_t? (yes for more than one value as in MLE)
    #'@return: a list with x_hat estimated value of x* based on newton's method with steepest ascent assurance and the number of iterations taken to converge
    #'@author: Casey Jayne
    
    
    dxdg <- deriv3(gx, respect_to, function.arg=names(inputs))
    iter <- 1
    while(TRUE){
        if(sum){
            h <- do.call(dxdg,inputs) # h <- eval(dxdg, as.list(inputs))
            h_t <- matrix(colSums(attr(h, 'gradient')))
            rownames(h_t) <- respect_to
        }else{
            h <- do.call(dxdg,inputs) # h <- eval(dxdg, as.list(inputs))
            h_t <- matrix(attr(h, 'gradient'))
            rownames(h_t) <- respect_to
        }
        x_next <- as.numeric(inputs[respect_to]) + h_t
        print(paste0('At iteration ', iter, ' x_next is: ', x_next[1], ', ', x_next[2]))
        alpha=1
        # traditional convergence check
        # calculate the ll values to check
        inputs_now <- inputs
        inputs_now[respect_to] <- unname(x_next)
        if(sum){
            ll_now <- sum(na.omit(eval(gx, inputs_now)))
            ll_prev <- sum(na.omit(eval(gx, inputs)))
            ascent <- ll_now-ll_prev
        }else{
            ll_now <- eval(gx, inputs_now)
            ll_prev <- eval(gx, inputs)
            ascent <- ll_now-ll_prev
        }
        while(ascent<=0){
            print(paste0('Difference is: ', ascent))
            # didn't move in negative direction
            alpha = alpha/2
            print('Halving alpha ')
            x_next <- as.numeric(inputs[respect_to]) + alpha*h_t
            inputs_now <- inputs
            inputs_now[respect_to] <- unname(x_next)
            print(paste0('x_next: ', x_next[1], ', ', x_next[2]))
            if(sum){
                ll_now <- sum(na.omit(eval(gx, inputs_now)))
                ll_prev <- sum(na.omit(eval(gx, inputs)))
                ascent <- ll_now-ll_prev
            }else{
                ll_now <- eval(gx, inputs_now)
                ll_prev <- eval(gx, inputs)
                ascent <- ll_now-ll_prev
            }
            if(norm(as.matrix(as.numeric(x_next)-as.numeric(inputs[respect_to])), 'F')<.0001){
                return(list(theta_hat = x_next, convergence_steps = iter))
            }
            if(alpha == alpha/2^5000){
                return('No more descent')
            }
        }
        if(norm(as.matrix(as.numeric(x_next)-as.numeric(inputs[respect_to])), 'F')<.0001){
            return(list(theta_hat = x_next, convergence_steps = iter))
        }else if(iter > 10000){
            return('Failed to converge')
        }
        print(paste0('Difference is: ', ascent))
        inputs[respect_to] <- unname(x_next)
        # if(sum){
        #     h <- do.call(dxdg,inputs)
        #     h_t <- matrix(colSums(attr(h, 'gradient')))
        #     rownames(h_t) <- names(colSums(attr(h, 'gradient')))
        #     h_t <- h_t[respect_to,] # only grab items of interest at this iteration
        # }else{
        #     h <- do.call(dxdg,inputs)
        #     h_t <- matrix(attr(h, 'gradient'))
        #     rownames(h_t) <- names(attr(h, 'gradient'))
        #     h_t <- h_t[respect_to,] # only grab items of interest at this iteration
        # }
        iter <- iter + 1
    }
}

#steepest_ascent(ll, respect_to = c('alpha1', 'alpha2'), inputs = list(n=oil_dat$spills, b1=oil_dat$importexport, b2=oil_dat$domestic, alpha1=1, alpha2=1), sum=TRUE)

#steepest_ascent(ll, respect_to = c('alpha1', 'alpha2'), inputs = list(n=oil_dat$spills, b1=oil_dat$importexport, b2=oil_dat$domestic, alpha1=2, alpha2=2), sum=TRUE)

steepest_ascent(ll, respect_to = c('alpha1', 'alpha2'), inputs = list(n=oil_dat$spills, b1=oil_dat$importexport, b2=oil_dat$domestic, alpha1=.1, alpha2=.1), sum=TRUE)
