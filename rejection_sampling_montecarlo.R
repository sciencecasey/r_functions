rejection_sampling <- function(n, f_target, g_sampling, g_density, alpha=1, from=0, to=1, ...){
    #'@param n: number of samples to draw
    #'@param f_target: an expression with the target density from which to sample the x-values
    #'@param g_sampling: the distribution from which to draw values
    #'@param alpha: a number between 0 and 1 around which to make the envelope
    #'@return
    #'@author: Casey Jayne
    
    # make sure within the from-to bounds
    y <- g_sampling(n, ...)
    y <- y[which(y<=to & y>=from)]
    #u <- runif(length(y), from, to)
    u <- runif(length(y), 0, 1)
    fy <- tryCatch(sapply(y, f_target), error=function(e) f_target(y)) # needs to be within the from and to support range
    gy <- sapply(y, function(x) g_density(x, ...))
    ey <-  gy/alpha
    indx <- which(u<=fy/ey)
    not_indx <- which(u>fy/ey)
    return(list(keep=data.frame(x_keep = y[indx], envelopeX = ey[indx]/alpha, f_targetX=fy[indx], u=u[indx]), reject=data.frame(x_reject = y[not_indx], envelopeX = ey[not_indx]/alpha, f_targetX=fy[not_indx], u=u[not_indx])))
}

x <- rejection_sampling(1000, dnorm, rexp, dexp, alpha=.3, rate=2, from = -5, to = 5)
plot(x$keep$envelopeX~x$keep$x_keep, ylim=c(0, 5), xlim=c(-3, 3))
points(x$keep$f_targetX~x$keep$x_keep, col='red')

y <- rnorm(1000, 0, 4) # y drawn from g
f <- function(x) dnorm(x, 1, 3) # target function (black)
fy <- sapply(y, f) # evaluated target function
gy <- dnorm(y, 0, 4) # evaluated g function 
ey <- gy/.5 # evaluated envelope green

plot(ey~y, col='green', ylim=c(0, 1), main='target function (black), envelope(green), cutoff (red)', sub='Kept values and evaluation at target, Orange', ylab = 'Function at y')
points(fy~y) # (black)
u <- seq(-10, 10,length.out=length(y)) # random range on horizontal axis
points((fy/ey)~y, col='red') # the cutoff criteria to evaluate the random draw at (red)
v <- runif(length(u)) # random draws from uniform to use in evaluation
points(v~u, col='blue') # plotting the random draws randomly distributed about to show compared to comparison function fy/ey (red)
indx <- which(v <= fy/ey) # which kept
points(fy[indx]~y[indx], col='orange', cex=.25) # the kept values (orange)


rejection_sampling_envelope<- function(n, f, alpha, g_sample, g_density, from, to, ...){
    var <- c()
    density <- c()
    iterations <- 1
    while(length(var) < n)
    {
        y <- g_sample(1, ...) # Sample Y ~ g
        while(y>to | y<from){
            y <- g_sample(1, ...) # Sample Y ~ g
        }
        u <- runif(1, 0, 1) # Sample U ~ Unif(1,0)
        g_y <- g_density(y, ...)
        e <- g_y/alpha
        if(u <= f(y)/e) { # Reject if U > f(y)/e(y)
            # Accept y as random variable of pdf f
            var <- append(var, y)
            density <- append(density, f(y))
        }
        iterations <- iterations+1
    }
    print(paste("Total iterations to produce", n, "samples was", iterations))
    return(data.frame(var, density))
}

x <- rejection_sampling(1000, dnorm, g_sampling = rexp, g_density = dexp, alpha=.3, from=-5, to=5, rate=2)
# rej1 <- rejection_sampling(5000, f = function(x) 4*x, g_sampling=rnorm, g_density=dnorm, alpha=.6, from=0, to=.5, mean=.5, sd=.25)
# rej2 <- rejection_sampling(5000, f = function(x) 4-4*x, g_sampling = rnorm, g_density=dnorm, alpha=.6, from=.5, to=1, mean=.5, sd=.25)
# plot(c(rej1$keep$f_targetX, rej2$keep$f_targetX)~c(rej1$keep$x_keep, rej2$keep$x_keep))
# 
# rej1 <- rejection_sampling_envelope(5000, f = function(x) 4*x, g_sample=rnorm, g_density=dnorm, alpha=.6, from=0, to=.5, mean=.5, sd=.25)
# rej2 <- rejection_sampling_envelope(5000, f = function(x) 4-4*x, g_sample = rnorm, g_density=dnorm, alpha=.6, from=.5, to=1, mean=.5, sd=.25)
# plot(c(rej1$density, rej2$density)~c(rej1$var, rej2$var))


# dat <- rejection_sampling(1000, f_target = function(x) 4*sqrt(1-x^2), g_sampling=rnorm, e_function=dnorm)
# reject <- dat$reject
# dat <- dat$keep
# dat <- dat %>% 
#     mutate(f_over_e = f_targetX/envelopeX)

#dat <- rejection_sampling(1000, f_target = function(x) 4*sqrt(1-x^2), g_sampling=rnorm, e_function=dnorm, mean=1, sd=3)
#plot(density(x))

# plot(f_over_e~x_keep, dat, col='green', ylim=c(0, 10), main='Density of Target function (black)', sub=' Envelope (red), and Target/Envelope (green)', ylab='', xlab='kept inputs')
# points(envelopeX~x_keep, dat, col='red')
# points(f_targetX~x_keep, dat)
# points(rep(2, length(u))~u, dat)
# plot(f_targetX~x_reject, reject, col='pink')
#hist(dat$x_keep, add=TRUE, freq = TRUE, col = NULL)


