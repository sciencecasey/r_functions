normal_dist <- function(x, mu=0, sigma=1, log=FALSE){
    #'@param x: vector of inputs from uniform(0,1) distribution
    #'@param mu: mean of distribution
    #'@param sigma: sd of distribution
    #'@param log: should be lognormal? 
    #'@return a vector of numbers from N(mu, sigma) distribution
    u1 <- x[1:floor(length(x)/2)]
    u2 <- x[(length(u1)+1):length(x)]
    x1 <- mu+sigma*sqrt(-2*log(u1))*cos(2*pi*u2)
    x2 <- mu+sigma*sqrt(-2*log(u1))*sin(2*pi*u2)
    if(log){
        return(exp(c(x1,x2)))
    }
    return(c(x1,x2))
    
}
x <- runif(100)
normal_dist(x)


