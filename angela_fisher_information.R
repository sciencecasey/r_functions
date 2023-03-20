fishers_MLE <- function(theta_0, gprime_theta, fishers_theta, 
                        epsilon, max_iter = 1000) {
    #' Fishers Scoring MLE calculation for a poison process with parameters theta 1 and 2
    #'
    #' Calculates the MLE estimates for parameters theta 1 and 2 in a Poisson process
    #'
    #' @param theta_0 Starting value for the theta vector
    #' @param gprime_theta Gradient of the log likelihood function (first order derivatives 
    #' for log likelihood function)
    #' @param fishers_theta Fishers information matrix
    #' @param epsilon Convergence precision/tolerance
    #' @param max_iter Maximum numbers of iterations (when to give up)
    
    theta_t <- theta_0
    iteration <- 1
    repeat{
        print(paste("At Iteration ", iteration, "Theta 1 is:", theta_t[1], "Theta 2 is:", theta_t[2]))
        
        # Evaluate gradient and fishers information matrix at the current points
        Gprime_t <- matrix(c(gprime_theta[[1]](theta_t), gprime_theta[[2]](theta_t)), ncol = 1)
        Fishers_t <- matrix(c(fishers_theta[1,1][[1]](theta_t),fishers_theta[1,2][[1]](theta_t),
                              fishers_theta[2,1][[1]](theta_t),fishers_theta[2,2][[1]](theta_t)), nrow=2)
        theta_next <- theta_t + (solve(Fishers_t)%*%Gprime_t)
        
        # Check for convergence (absolute) 
        dist <- sqrt(sum((theta_next - theta_t)^2))/(sqrt(sum((theta_t - 0)^2))+epsilon)
        if( !is.nan(dist) && (dist < epsilon) ){
            # The algorithm converged
            break 
        } else if (iteration < max_iter) {
            # Continue to next iteration after updating x_t
            theta_t <- theta_next
            iteration <- iteration + 1
        } else {
            stop('Algorithm failed to converge before max_iter reached')
        }
    }
    return(theta_t)
}

fishers_MLE(theta_0 = matrix(c(0.1, 0.1), ncol=1), gprime_theta, fishers_info, epsilon = 0.0001)