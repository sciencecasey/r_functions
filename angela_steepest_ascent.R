steepest_ascent_MLE <- function(theta_0, g_theta, gprime_theta, 
                                epsilon, max_iter = 1000) {
    #' Steepest Ascent MLE calculation for a poison process with parameters theta 1 and 2
    #'
    #' Calculates the MLE estimates for parameters theta 1 and 2 in a Poisson process
    #'
    #' @param theta_0 Starting value for the theta vector
    #' @param g_theta Log likelihood function to calculate direction of the ascent
    #' @param gprime_theta Gradient of the log likelihood function (first order derivatives 
    #' for log likelihood function)
    #' @param epsilon Convergence precision/tolerance
    #' @param max_iter Maximum numbers of iterations (when to give up)
    
    theta_t <- theta_0
    theta_next <- theta_t
    iteration <- 1
    total_iter <- 1
    repeat{
        print(paste("At Main Iteration ", iteration, "Theta 1 is:", theta_t[1], "Theta 2 is:", theta_t[2]))
        
        alpha <- 1
        # Loop checking for ascent
        repeat{
            # Evaluate gradient the current points
            Gprime_t <- matrix(c(gprime_theta[[1]](theta_t), gprime_theta[[2]](theta_t)), ncol = 1)
            theta_next <- theta_t + alpha*Gprime_t
            
            ascent <- g_theta(theta_next) - g_theta(theta_t)
            print(paste("At Iteration ", total_iter, "Alpha is:", alpha, ", diff is:", ascent))
            
            # Check for ascent and adjust alpha accordingly
            if ( !is.nan(ascent) && ascent > 0){
                break
            } else {
                print(paste("At Iteration ", total_iter, "halving alpha for ascent."))
                alpha <- alpha/2
                total_iter <- total_iter + 1
            }
        }
        
        # Check for convergence (absolute) 
        if((sqrt(sum((theta_next - theta_t)^2))/(sqrt(sum((theta_t - 0)^2))+epsilon) < epsilon)){
            # The algorithm converged
            break 
        } else if (iteration < max_iter) {
            # Continue to next iteration after updating x_t
            theta_t <- theta_next
            iteration <- iteration + 1
            total_iter <- total_iter + 1
        } else {
            stop('Algorithm failed to converge before max_iter reached')
        }
    }
    return(theta_t)
}
#steepest_ascent_MLE(theta_0 = matrix(c(0.01, 0.01), ncol=1), g_theta=ll_theta, gprime_theta, epsilon = 0.0001)
#steepest_ascent_MLE(theta_0 = matrix(c(1, 1), ncol=1), g_theta=ll_theta, gprime_theta, epsilon = 0.0001)
steepest_ascent_MLE(theta_0 = matrix(c(.1, .1), ncol=1), g_theta=ll_theta, gprime_theta, epsilon = 0.0001)
steepest_ascent_MLE(theta_0 = matrix(c(2, 2), ncol=1), g_theta=ll_theta, gprime_theta, epsilon = 0.0001)
steepest_ascent_MLE(theta_0 = matrix(c(10, 10), ncol=1), g_theta=ll_theta, gprime_theta, epsilon = 0.0001)