# source('~/Learn_R/functions/hessian.R')
# 
fisher_information <- function(gx, respect_to, data, sum=TRUE){
  # recall that fisher information can be rewritten as variance of first derivative of the log liklihood summed over x
      if(sum){
          f <- eval(deriv3(gx, names(respect_to)), as.list(respect_to))
          out_mat <- matrix(sum(attr(f, 'gradient')), length(respect_to), dimnames = list(respect_to))
      }else{
          f <- eval(deriv3(gx, names(respect_to)), as.list(respect_to))
          out_mat <- matrix(attr(f, 'gradient'), length(respect_to), dimnames = list(respect_to))
      }
  return(var(out_mat))
}

g <- expression(x^3-2*x*y-y^6)
y=2
fisher_information(g, respect_to = c('x'=1, 'y'=2), sum=FALSE)
# 
# 
# 
# # make the information matrix from expected hessian functions derived analytically
# eh_11 <- function(b1, b2, a1, a2){
#     return(sum((b1^2)/(a1*b1+b2*b2)))
# }
# eh_12 <- function(b1, b2, a1, a2){
#     return(sum((b1*b2)/(a1*b1+b2*b2)))
# }
# eh_22 <- function(b1, b2, a1, a2){
#     return(sum((b2^2)/(a1*b1+b2*b2)))
# }
# oil_dat <- read.table('~/OneDrive/JHU/computational_statistics/datasets/oilspills.dat', header=TRUE, sep=' ')
# ll <- expression(n*log(alpha1*b1+alpha2*b2)-log(factorial(n))-alpha1*b1-alpha2*b2) # log liklihood (should wrap in a sum)
# add extra information directly into the process

fisher_process<- function(start_a1, start_a2){
    # initialize unchanging params
    b1=oil_dat$importexport
    b2=oil_dat$domestic
    n=oil_dat$spills
    # make the objects to compute each time
    alpha <- matrix(c(start_a1, start_a2), nrow=2)
    G <- deriv3(ll, c('alpha1', 'alpha2'), c('alpha1', 'alpha2', 'b1', 'b2', 'n'), hessian=FALSE)
    iter <- 1
    while(TRUE){
        # compute the fisher information
        HE <- matrix(c(eh_11(b1, b2, a1=alpha[1], a2=alpha[2]), 
                       rep(eh_12(b1, b2, a1=alpha[1], a2=alpha[2]), 2),
                       eh_22(b1, b2, a1=alpha[1], a2=alpha[2])), nrow=2, byrow=TRUE)
        # alpha next is alpha+I(alpha)^-1%*%gradient(alpha)
        grad <- matrix(colSums(attr(G(alpha[1], alpha[2], b1, b2, n), 'gradient')))
        alpha_next <- alpha + pinv(HE) %*% grad
        if(norm(alpha_next-alpha, 'F')<.0001){
            return(list(theta_hat = alpha_next, convergence_steps=iter))
        }
        iter <- iter + 1
        alpha <- alpha_next
        # recompute HE
        
    }
}
fisher_process(.1,.1)
fisher_process(1,1)
fisher_process(.1,.1)
