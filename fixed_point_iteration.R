fixed_point_iteration <- function(gx, x_start, respect_to='x', sum=FALSE, alpha=1){
  #'@param gx: an expression object with only one variable, x
  #'@param x_start: the starting point to iterate from
  #'@param sum: should the gradient be summed at x_t? (yes for more than one value as in MLE).default FALSE
  #'@param respect_to: variable name for the gradient to be taken with respect to
  #'@return: a list with x_hat estimated value of x* based on fixed point iteration where the G function is G(x)=g'(x)+x and the number of iterations taken to converge
  #'@author: Casey Jayne
  dxdg <- deriv(gx, respect_to, function.arg=TRUE)
  iter <- 1
  if(sum){
    while(TRUE){
      x_next <- alpha*sum(attr(dxdg(x_start), 'gradient'))+x_start
      if(abs(x_next-x_start) < .001){
        return(list(x_hat = round(x_next, 2), convergence_steps=iter))
      }
      x_start <- x_next
      iter <- iter+1
    }
  }else{
    while(TRUE){
      x_next <- alpha*attr(dxdg(x_start), 'gradient')+x_start
      if(abs(x_next-x_start) < .001){
        return(list(x_hat = x_next, convergence_steps=iter))
      }
      x_start <- x_next
      iter <- iter+1
    }
  }
}
eq <- expression(log(x)/(1+x))
newtons_method(eq, .5)
