# https://youtu.be/Y8MyV4vOnXs
data <- rnorm(1000, mean = 25, sd = 5)
data.1 <- rnorm(1000, mean = 10, sd = 2)
data <- c(data, data.1)
rm(data.1)
hist(data)
hist(data, plot = FALSE) # this gives us bin size and the density
hist(data, breaks = seq(0,50, 2)) # breaks gives us not bin size 2 from range 0 to 50

# KDE
mean = 25
sd = 5
# pnorm gives us the normal probability dist. funciton
# probability that the values are +/- 2 sd of the funtion
1 - 2*pnorm(q = mean + (2*sd), mean = mean, sd = sd, lower.tail = FALSE)

# density function gives us the gaussian kernel
kde = density(data)
plot(kde)
kde$bw
kde

# change the bw
kde = density(data, bw = .5)
plot(kde)
kde = density(data, bw = .25)
plot(kde)


# https://rpubs.com/mcocam12/KDF_byHand
Gauss_K = function(Values, Range, h=1){
    library(dplyr) 
    
    #Result
    densities = data.frame()
    #Temporal variable
    Temp = data.frame()
    #One by one variable
    V = vector()
    
    for (i in 1:length(Values))
    {
        #The value
        V = Values[i]
        #Gaussian Function
        Temp =
            data.frame(
                Density = exp(-(Range-V)^2/(2*h^2))/(h*sqrt(2*pi)),
                Range = Range,
                Value = as.factor(paste0("X",i))
            )
        densities = rbind(densities, Temp)
    }
    Densities1 = densities
    Densities2 = densities%>%
        #Sum K by range value!
        group_by(Range)%>%
        summarise(Bell_sum = sum(Density))%>%
        #Normalization
        mutate(Kernel_Density = Bell_sum /length(Values))
    #Data Frame with points
    Points = Densities1%>%
        group_by(Value)%>%
        summarise(Y = max(Density))%>%
        mutate(Value = Values)
    
    
    return(
        list(Densities1, Densities2, Points)
    )
}

set.seed(1234)
S = sample(iris$Petal.Length, 5)
S
R = seq(min(S)-5, max(S)+5, 0.01)
Points5 = Gauss_K(S, R, h = 1)
densityPlot(S, bw = .25)

cl = colors()
densityPlot(iris$Petal.Length, 
            g = iris$Species, 
            bw = .25, 
            xlab = "Petal Length", 
            col = c(cl[10], cl[60], cl[150]))

densityPlot(iris$Petal.Length, 
            g = iris$Species, 
            bw = .1, 
            xlab = "Petal Length", 
            col = c(cl[10], cl[60], cl[150]))

densityPlot(iris$Petal.Length, 
            g = iris$Species, 
            bw = .5, 
            xlab = "Petal Length", 
            col = c(cl[10], cl[60], cl[150]))

library(ggplot2)
ir <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, 
                              color = Species)) +
                   geom_point() 
                    
ir + geom_density_2d(h = .5)

kde <- kde2d(iris$Petal.Length, iris$Petal.Width, h = .5)



bar<-matrix(runif(100), nrow = 10)
contour(bar)
str(bar)
contour(bar,levels = 0.2, lwd=3, add=TRUE)
contour(bar,lwd=c(2,1,1,1,1,5))

# Lunch break R, lynda
library(lattice)
str(ChickWeight)
levelplot(weight~Time*Diet, data = ChickWeight)
levelplot(weight~Time*Diet, data = ChickWeight, contour = TRUE)
levelplot(weight~Time*Diet, data = ChickWeight, contour = TRUE, cuts = 7) 
contourplot(weight~Time*Diet, data = ChickWeight)
contourplot(weight~Time*Diet, data = ChickWeight, region = TRUE, labels = FALSE)
contourplot(weight~Time*Diet, data = ChickWeight, region = TRUE, labels = FALSE, cuts = 30)



levelplot(Species~Petal.Length*Petal.Width, data = iris, contour = TRUE)
contourplot(kde$z~kde$x*kde$y)
contourplot(iris$Species~iris$Petal.Length*iris$Petal.Width, 
            data = iris, 
            pretty = FALSE, 
            col.regions = c(cl[20], cl[30], cl[100]), 
            contour = TRUE, 
            cuts = 30)
contourplot(iris$Species ~ iris$Petal.Length*iris$Petal.Width, 
            region = F, lwd  = 2, at = c(0, .5, 3))

levelplot(volcano, panel = panel.levelplot.raster,
          col.regions = topo.colors, cuts = 30, interpolate = TRUE)
levelplot(t(as.matrix(iris[,3:4])), panel = panel.levelplot.raster,
          col.regions = topo.colors, cuts = 30, interpolate = TRUE)

library(MASS) # for kde2d
s <- kde2d(iris[1:50,3], iris[1:50,4], h = .5)
ve <- kde2d(iris[51:100,3], iris[51:100,4], h = .5)
vi<- kde2d(iris[101:150,3], iris[101:150,4], h = .5)
str(i)
image(i, zlim = c(0,3))
persp(i, phi = 10, theta = 20)


#graphics man
## (1) The Obligatory Mathematical surface.
##     Rotated sinc function.
x <- seq(-10, 10, length.out = 50)
y <- x
rotsinc <- function(x,y){
    sinc <- function(x) { y <- sin(x)/x ; y[is.na(y)] <- 1; y }
    10 * sinc( sqrt(x^2+y^2) )}
sinc.exp <- expression(z == Sinc(sqrt(x^2 + y^2)))
z <- outer(x, y, rotsinc)
oldpar <- par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
title(sub=".")## work around persp+plotmath bug
title(main = sinc.exp)
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Z")
title(sub=".")## work around persp+plotmath bug
title(main = sinc.exp)
## (2) Visualizing a simple DEM model
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
persp(x, y, z, theta = 120, phi = 15, scale = FALSE, axes = FALSE)
## (3) Now something more complex
##     We border the surface, to make it more "slice like"
##     and color the top and sides of the surface differently.
z0 <- min(z) - 20
z <- rbind(z0, cbind(z0, z, z0), z0)
x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
y <- c(min(y) - 1e-10, y, max(y) + 1e-10)

fill <- matrix("green3", nrow = nrow(z)-1, ncol = ncol(z)-1)
fill[ , i2 <- c(1,ncol(fill))] <- "gray"
fill[i1 <- c(1,nrow(fill)) , ] <- "gray"
par(bg = "lightblue")
persp(x, y, z, theta = 120, phi = 15, col = fill, scale = FALSE, axes = FALSE)
title(main = "Maunga Whau\nOne of 50 Volcanoes in the Auckland Region.",
      font.main = 4)
par(bg = "slategray")
persp(x, y, z, theta = 135, phi = 30, col = fill, scale = FALSE,
      ltheta = -120, lphi = 15, shade = 0.65, axes = FALSE)
## Don't draw the grid lines :  border = NA
persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
      ltheta = -120, shade = 0.75, border = NA, box = FALSE)

## `color gradient in the soil' :
fcol <- fill ; fcol[] <- terrain.colors(nrow(fcol))
persp(x, y, z, theta = 135, phi = 30, col = fcol, scale = FALSE,
      ltheta = -120, shade = 0.3, border = NA, box = FALSE)

## `image like' colors on top :
fcol <- fill
zi <- volcano[ -1,-1] + volcano[ -1,-61] +
    volcano[-87,-1] + volcano[-87,-61]  ## / 4
fcol[-i1,-i2] <-
    terrain.colors(20)[cut(zi,
                           stats::quantile(zi, seq(0,1, length.out = 21)),
                           include.lowest = TRUE)]
persp(x, y, 2*z, theta = 110, phi = 40, col = fcol, scale = FALSE,
      ltheta = -120, shade = 0.4, border = NA, box = FALSE)


## reset par():
par(oldpar)






# built-in
contour(i, nlevels = 20)
contour(s, xlim = c(0, 7), ylim = c(0, 3), col = cl[10])
lines(contour(ve, xlim = c(0, 7), ylim = c(0, 3), col = cl[30]))
contour(vi, xlim = c(0, 7), ylim = c(0, 3), col = cl[50])
# this adds them together :/ 
contour(s, col = cl[10]) + contour(ve, col = cl[30]) + contour(vi, col = cl[50])

# man page, R for contour trying to understand how it works
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
lev <- pretty(range(volcano), 10)
par(bg = "lightcyan")
pin <- par("pin")
xdelta <- diff(range(x))
ydelta <- diff(range(y))
xscale <- pin[1]/xdelta
yscale <- pin[2]/ydelta
scale <- min(xscale, yscale)
xadd <- 0.5*(pin[1]/scale - xdelta)
yadd <- 0.5*(pin[2]/scale - ydelta)
plot(numeric(0), numeric(0),
     xlim = range(x)+c(-1,1)*xadd, ylim = range(y)+c(-1,1)*yadd,
     type = "n", ann = FALSE)
usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], col="green3")
contour(x, y, volcano, levels = lev, col="yellow", lty="solid", add=TRUE)
box()
title("A Topographic Map of Maunga Whau", font= 4)
title(xlab = "Meters North", ylab = "Meters West", font= 3)
mtext("10 Meter Contour Spacing", side=3, line=0.35, outer=FALSE,
      at = mean(par("usr")[1:2]), cex=0.7, font=3)


# lattice
levelplot(kde$z, panel = panel.levelplot.raster,
          col.regions = topo.colors, 
          cuts = 30, interpolate = TRUE)
contourplot(s$z) 
contourplot(ve$z)
contourplot(vi$z)


# 
## Given data
x  <- c(4.09, 4.46, 4.61, 4.30, 4.03, 5.22, 4.21, 4.07, 4.02, 4.58, 4.66, 4.05, 
        4.23, 5.51, 4.03, 4.72, 4.47, 4.50, 5.80, 4.30, 4.09, 4.78, 4.18, 4.45, 
        4.40, 5.60, 4.37, 4.42, 4.88, 4.20, 4.45, 4.10, 4.43, 4.58, 4.40, 4.38)
h  <- 0.1516 

# https://stackoverflow.com/questions/64235786/gaussian-kernel-density-estimation-in-r
DensityGraph <- function(x, h){
    n    <- length(x)
    xi   <- seq(min(x) - sd(x), max(x) + sd(x), length.out = 512)
    # gaus kernel function
    GK <- function(u) {dnorm(u)} 
    # fhat without sum since we are interest in the bell shaped curves
    fhat <- sapply(x, function(y){(1/(n*h))*GK((xi - y)/h)})
    # histogram of x
    hist (x, freq = FALSE, nclass = 15, main = "Kernel density with histogram",
          xlab = paste("N = ", n, "   ", "Bandwidth = ", h))
    # add fhat with sum
    lines(xi, rowSums(fhat), lwd = 2)
    # add the bell shaped curves
    apply(fhat, 2, function(j) lines(xi, j, col = 4))
    # show data points
    rug  (x, lwd = 2, col = 2)
}

DensityGraph(x = x, h = 0.05)
lines(density(x = x, bw = 0.1516), col = 3, lwd = 2)
DensityGraph(x = iris[1:50, 3], h = .05)
DensityGraph(x = iris[1:50, 3], h = .05)
# GaussianKernel
GK <- function(u) {(1/sqrt(2*pi))*exp(-(u^2)/2)} # or dnorm(u)


fhat <- function(x, h, specific_x){
    n    <- length(x)
    xi   <- seq(min(x) - sd(x), max(x) + sd(x), length.out = 512)
    GK <- function(u) {(1/sqrt(2*pi))*exp(-(u^2)/2)} # or dnorm(u)
    f    <- rowSums(sapply(x, function(y){(1/(n*h))*GK((xi - y)/h)}))
    kde  <- data.frame(xi, fhat = f)
    indx <- which.min(abs(xi - specific_x))
    fx   <- kde[indx, "fhat"]
    list(fx = fx, kde = kde)
}

KernelDensity <- fhat(x = x, h = 0.1516, specific_x = 4.09)
KernelDensity$fx
# [1] 0.9114677
plot(KernelDensity$kde, type  = "l", lwd = 2, xlab = "")
title(xlab = paste("N = ", length(x), "    Bandwidth = ", h))
rug(x, lwd = 2, col = 2)
lines(density(x, bw = 0.1516), col = 5) 

# graphics man pages
par(bg="white")
n <- 1000
x <- c(0,cumsum(rnorm(n)))
y <- c(0,cumsum(rnorm(n)))
xx <- c(0:n, n:0)
yy <- c(x, rev(y))
plot(xx, yy, type="n", xlab="Time", ylab="Distance")
polygon(xx, yy, col="gray")
title("Distance Between Brownian Motions")

# P is my created group
p1 <- parz_window_og(iris[, 3:4], iris[1:50, 3:4], spread = .1)
p2 <- parz_window_og(iris[, 3:4], iris[51:100, 3:4], spread = .1)
p3 <- parz_window_og(iris[, 3:4], iris[101:150, 3:4], spread = .1)
par(bg = "green")
n <- 50
x <- iris$Petal.Length
y <- iris$Petal.Width    
z <- 100+0.5*outer(iris$Petal.Length,iris$Petal.Width,FUN="+")
z <- scale(z)
x= seq(-3, 3, 1)
y= seq(-3, 3, 1)
contour(x,y,z)
xx <- c(0:n, n:0)
yy <- c(x, rev(y))
plot(x[1:50], y[1:50], col = "red")
contour(gaus_kernel(iris[1:50, 3:4], iris[,3:4], .1))
levelplot(gaus_kernel(iris[1:50, 3:4], iris[,3:4], .1))
heatmap(gaus_kernel(iris[1:50, 3:4], iris[,3:4], .1))
contourplot(iris$Petal.Width, data = rbind(p1, p2, p3))
length(xx)
length(yy)

x <- y <- 1:10
z <- 100+0.5*outer(x,y,FUN="+")
contour(x,y,z)
z[3:4,3:4] <- NA #putting some missing values
contour(x, y, z,nlevels=20)
x<-1:10
y<-1:10
z <- outer(x,y,function(x,y) 100 + 0.5*x + 0.5*y)
contour(x,y,z)
