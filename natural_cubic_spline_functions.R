nat_cubic_spline <- function(dat, seps){
    h <- rep(0, length(seps)-1)
    b <- rep(0, length(seps)-1)
    v <- rep(0, length(seps)-1)
    u <- rep(0, length(seps)-1)
    z <- rep(0, length(seps))
    written <- rep(NA, length(seps)-1)
    z[c(1, length(z))] <- 0
    for(i in 1:(length(seps)-1)){
        h[i] <- dat$x[(i+1)]-dat$x[i]
        b[i] <- 1/h[i]*(dat$y[(i+1)]-dat$y[(i)])
        if(i>1){
            v[i] <- 2*(h[(i-1)]+h[i])
            u[i] <- 6*(b[i]-b[i-1])
        }
    }
    # remove first two v, u values
    v <- v[-1]
    u <- u[-1]
    A <- diag(v)
    # fill the matrix
    for(i in 1:(ncol(A)-1)){
        A[i, (i+1)] <- h[i]
        A[(i+1), i] <- h[i]
    }
    # solve system
    z[c(2:(length(z)-1))] <- solve(A) %*% as.matrix(u)
    for(i in 1:(length(z)-1)){
        c1 <- z[(i+1)]/(6*h[i])
        c2 <- z[i]/(6*h[i])
        c3 <- dat$y[(i+1)]/h[i]-z[(i+1)]/6*h[i]
        c4 <- dat$y[i]/h[i]-h[i]/6*z[i]
        written[i] <- paste0(c1, '*(x-', dat$x[i], ')^3+', 
                             c2, '*(', dat$x[i+1], '-x)^3+',
                             c3, '*(x-', dat$x[i], ')+',
                             c4, '*(', dat$x[i+1], '-x)')
    }
    return(written)
}

dat <- list(x=c(.9, 1.3, 1.9, 2.1),
            y=c(1.3, 1.5, 1.85, 2.1))
nat_cubic_spline(dat, c(0,1,2,3))
