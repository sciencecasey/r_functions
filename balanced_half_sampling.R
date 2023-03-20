balanced_half_sample <- function(x, y, g, mean=NULL, sd=NULL){
    #'@param x: a vector of observed x values
    #'@param y: a vector of observed y values
    #'@param g: a function taking in x observations with static parameters already passed
    #'@param mean: should mean of x be passed as parameter? default NULL means no
    #'@param sd: should sd of x be passed as paramter? default NULL means no
    #'@return
    
    # grab first half and second half indeces
    first <- 1:floor(length(x)/2)
    sec <- (floor(length(x)/2)+1):ceiling(length(x/2))
    
    # split the data
    x1 <- x[first]
    y1 <- y[first]
    x2 <- x[sec]
    y2 <- y[sec]
    if(!is.null(mean) & !is.null(sd)){
        # use params of opposite
        g1 <- g(x1, mean(x2), sd(x2))
        g2<- g(x2, mean(x1), sd(x1))
    }else if(!is.null(mean)){
        g1 <- g(x1, mean(x2))
        g2<- g(x2, mean(x1))
    }else if(!is.null(sd)){
        g1 <- g(x1, sd(x2))
        g2<- g(x2, sd(x1))
    }else{
        g1 <- g(x1)
        g2<- g(x2)
    }
    er1 <- abs(y1-g1)
    er2 <- abs(y2-g2)
    return(1/length(x)*sum(er1,er2))
}

#balanced_half_sample(dat$x, dat$y, function(x, mu) dnorm(x, mu, sd=sqrt(2)), mean=TRUE)
