
# multivariate normal
gibbs <- function(start_x1, start_x2, cycles=10000){
    out <- data.frame(x1=c(rep(NA, cycles)),
                      x2=c(start_x2, rep(NA, cycles-1)))
    i <- 1
    while(i<cycles){
        fx1 <- rnorm(1, mean=(0+p*(out$x2[i])), sd=sqrt(1-p^2))
        out$x1[i] <- fx1
        fx2 <- rnorm(1, mean=(0+p*(out$x1[i])), sd=sqrt(1-p^2))
        out$x2[i+1] <- fx2
        i = i+1
    }
    # last iterate
    fx1 <- rnorm(1, mean=(0+p*(out$x2[i])), sd=sqrt(1-p^2))
    out$x1[i] <- fx1
    return(out)
}
p <- rho[1]
out <- gibbs(0, 0)
