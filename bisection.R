check_bisection <- function(a_0, b_0, dxdg, sum=FALSE){
  #'@param a_0: starting left boundary for iteration
  #'@param b_0: starting right boundary for iteration
  #'@param dxdg: first derivative of the equation of interest g(x) (where can be evaluated as function)
  #'@param sum: should the values be summed (as the case for MLE). default false
  #'@return: boolean whether or not meets constraint that g'(a_0)*g'(b_0) \leq 0
  #'@author Casey Jayne
  v1 <- dxdg(a_0)
  v2 <- dxdg(b_0)
  if(sum){
    result <- (sum(attributes(v1)[[1]][1])*sum(attributes(v2)[[1]][1]))
  }else{
    result <- (attributes(v1)[[1]][1]*attributes(v2)[[1]][1])
  }

  rm(v1, v2)
  return(result<=0)
}

# original function
x <- seq(1, 5, .01)
g <- expression((log(x))/(1+x))
# get first derivative
(dxdg <- D(g, 'x'))
plot(eval(g), main='log(x))/(1+x) and its derivative',col='red', xlab = 'x', ylab='y', xaxt='n')
lines(eval(dxdg), col='green')
axis(at=0:400, labels = x, side = 1, line = 1)

# now need it to be function to use the test
dxdg <- deriv(g, 'x', function.arg=TRUE)
#out <- dxdg(a_0)
#attributes(out)
# one way to grab it
#attributes(out)[[1]][1]
#attr(out, 'gradient')
check_bisection(3, 5, dxdg)


bisection_method <- function(a_in, b_in, dxdtheta, prev_x = NULL, iter = 1, sum=FALSE){
  #'@param a_in: starting left boundary for iteration
  #'@param b_in: starting right boundary for iteration
  #'@param dxdtheta: first derivative of the equation of interest g(x) (where can be evaluated as function)
  #'@param prev_x: prev x used as stopping point. should be null on first iteration
  #'@param iter: iteration count to return the number of iterations to stopping point
  #'@param sum: should the values be summed (as the case for MLE). default false
  #'@return: list with the result theta hat estimation and number of recursions called to converge
  #'@author Casey Jayne
  x_new <- (a_in+b_in)/2
  a_out <- dxdtheta(a_in)
  b_out <- dxdtheta(b_in)
  x_out <- dxdtheta(x_new)
  if(sum){
    a_out <- sum(attr(a_out, 'gradient'))
    b_out <- sum(attr(b_out, 'gradient'))
    x_out <- sum(attr(x_out, 'gradient'))
  }else{
    a_out <- attr(a_out, 'gradient')
    b_out <- attr(b_out, 'gradient')
    x_out <- attr(x_out, 'gradient')
  }
  if(!is.null(prev_x)){
    if(abs(prev_x - x_new) < .001){
      return(list(theta_hat = round(x_new, 2), convergence_steps = iter)) # this is final answer
    }
    iter <- iter + 1
  }
  if(a_out*x_out<=0){
    return(bisection_method(a_in, x_new, dxdtheta, prev_x=x_new, iter, sum))
  }else{
    return(bisection_method(x_new, b_in, dxdtheta, prev_x=x_new, iter, sum))
  }
}

bisection_method(25, 35, dxdtheta, sum=TRUE)
