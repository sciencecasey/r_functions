auto_correlation <- function(input, max=40){
    outs <- matrix(nrow=1)
    overall_mean <- mean(input)
    for(dist_apart in 2:max){
        ci <- 0
        for(from in 1:(length(input)-dist_apart)){
            indx <- seq(from, length(input), dist_apart)
            ci <- ci + (input[from]-overall_mean)*(input[(from+dist_apart)]-overall_mean)
        }
        ci <- ci/length(input)
        r <- ci/var(input)
        outs <- as.data.frame(cbind(outs, r))
        names(outs)[ncol(outs)] <- dist_apart
    }    
    outs <- outs[,-1] # remove first NA column
    return(outs)
}

#auto_correlation(dat1)
#acf(dat1)$acf[-1]
