secant_method <- function(x_0, x_1, gx, respect_to='x', sum=FALSE){
  #'@param x_0: the starting point to iterate from
  #'@param x_1: point 1 to iterate from
  #'@param gx: an expression object with only one variable, x
  #'@param sum: should the gradient be summed at x_t? (yes for more than one value as in MLE)
  #'@param respect_to: variable name for the gradient to be taken with respect to
  #'@return: a list with x_hat estimated value of x* based on secane method and the number of iterations taken to converge
  #'@author: Casey Jayne
  dxdg <- deriv(gx, respect_to, function.arg=TRUE)
  iter <- 1
  if(sum){
    while(TRUE){
      x_2 <- x_1 - sum(attr(dxdg(x_1), 'gradient'))*(x_1-x_0)/(sum(attr(dxdg(x_1), 'gradient'))-sum(attr(dxdg(x_0), 'gradient')))
      if(abs(x_2 - x_1) < .0001){
        return(list(x_hat=x_2, convergence_steps=iter))
      }
      # increment values
      x_0 <- x_1
      x_1 <- x_2
      iter <- iter+1
    }
  }else{
    while(TRUE){
      x_2 <- x_1 - attr(dxdg(x_1), 'gradient')*(x_1-x_0)/(attr(dxdg(x_1), 'gradient')-attr(dxdg(x_0), 'gradient'))
      if(abs(x_2 - x_1) < .0001){
        return(list(x_hat=x_2, convergence_steps=iter))
      }
      # increment values
      x_0 <- x_1
      x_1 <- x_2
      iter <- iter+1
    }
  }
}
eq <- expression(log(x)/(1+x))
#newtons_method(eq, .5, 2)


fixed_point_method <- function(g, respect_to, inputs, M=NULL, sum=FALSE){
    
    d <- deriv3(g, respect_to, names(inputs))
    h <- do.call(d, as.list(inputs))
    if(sum){
        val <- colSums(attr(h, 'gradient')) # should be length of respect_to
    }else{
        val <- attr(h, 'gradient')
    }
    
    if(is.null(M)){
        E <- matrix(0, nrow=length(respect_to), ncol=length(respect_to))
        diag(E) <- 1
        
    }else if(M=='hessian'){
        # use negative hessian as M
        M <- matrix(-colSums(attr(h, 'hessian')), nrow=length(respect_to))
        if(!pracma::isposdef(M, psd=TRUE)){
            # not positive definite
            M <- (M+t(M)) %*% diag(1/2, nrow = nrow(M), ncol=ncol(M))
            if(!pracma::isposdef(M, psd=TRUE)){
                errorCondition('M fails to be positive semi-definite')
            }
        }
        #M <- Matrix::nearPD(M, ensureSymmetry = TRUE)
    }
    iter <- 1
    while(TRUE){
        x_prev <- as.numeric(inputs[respect_to])
        inputs[respect_to] <- x_prev - pracma::pinv(M)%*%as.matrix(val)
        if(norm(as.matrix(as.numeric(inputs[respect_to])-as.numeric(x_prev)), 'F')<.0001){
            return(list(theta_hat = inputs[repect_to], convergence_steps = iter))
        }else if(iter > 10000){
            return('Failed to converge')
        }
        iter <- iter + 1
    }
}

fixed_point_method(eq, 'x', inputs=list(x=2), M='hessian')

ll <- expression(-1/2*log(2*pi) -1/2*log(sigma^2)-1/2*sigma^2*(x-mu)^2)
fixed_point_method(ll, inputs = list(mu=mean(dat), sigma=25, x=dat), sum = TRUE, respect_to = c('mu', 'sigma'), M='hessian')
