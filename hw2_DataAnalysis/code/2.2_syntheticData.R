library(lattice)
library(latticeExtra)
library(car)
synthetic_data <- function(x, n, dist = NULL){
  #'@param x a feature vector from which to generate more data
  #'@param n the number of synthetic datas to create
  #'@return positive numbers normalized to fit the data
  #'@param dist if entered as "gaussian" or "normal" the random numbers will
  #' be generated from a normal distribution.  default uses uniformly distributed 
  #' random values
  cols = dim(x)[2]
  rows = dim(x)[1]
  minnie = sapply(x, min)
  maxie = sapply(x, max)
  cov = cov(x)
  mean = colMeans(x)
  if(is.null(dist)){
    #generate uniformly distributed numbers between 1 and 100 for each col
    syn = runif((n*cols), 0, 100)
    # match dimensions
    dim(syn)<- c(n, cols)
  }else if(dist == "normal" || dist == "gaussian"){
    #generate uniformly distributed numbers between 1 and 100
    syn = rnorm(n*cols)
    # match dimensions
    dim(syn)<- c(n, cols)
  }else{
    warning("invalid distribution type")
    break
  }
  syn = as.matrix(syn)
  syn = syn
  cov = as.matrix(cov)
  syn = syn %*% cov # multiply by covariance matrix to orient
  syn = as.data.frame(syn)
  syn_min = sapply(syn, min)
  syn_max = sapply(syn, max)
  syn = ((syn - syn_min)/(syn_max-syn_min))*(maxie - minnie) + minnie
  syn = syn # transpose
  #center around the mean
  mu = colMeans(syn)-colMeans(x)
  syn = syn - mu
  return(abs(syn))
}

synth_setosa <- synthetic_data(iris[(1:50), -5], 100)
summary(synth_setosa)
summary(iris[(1:50), -5])
synth_versi <- synthetic_data(iris[(51:100), -5], 100)
synth_virgi <- synthetic_data(iris[(101:150), -5], 100)
# synth_swidth <- c(synthetic_data(iris$Sepal.Width[1:50], 100), 
#                  synthetic_data(iris$Sepal.Width[51:100], 100),
#                  synthetic_data(iris$Sepal.Width[101:150], 100))
# summary(iris$Sepal.Width[1:50])
# summary(synth_swidth[1:100])
# sd(synth_swidth[1:100]) 
# sd(iris$Sepal.Width[1:50]) #SD is .3790644
# sd(c(iris$Sepal.Width, synth_swidth)) # the SD is .527
# 
# synth_slength <- c(synthetic_data(iris$Sepal.Length[1:50], 100), 
#                  synthetic_data(iris$Sepal.Length[51:100], 100),
#                  synthetic_data(iris$Sepal.Length[101:150], 100))
# summary(iris$Sepal.Length[1:50])
# summary(synth_slength[1:100])
# 
# synth_plength <- c(synthetic_data(iris$Petal.Length[1:50], 100), 
#                   synthetic_data(iris$Petal.Length[51:100], 100),
#                   synthetic_data(iris$Petal.Length[101:150], 100))
# summary(iris$Petal.Length[1:50])
# summary(synth_plength[1:100])
# summary(c(iris$Petal.Length[1:50], synth_plength[1:100]))
# 
# synth_pwidth <- c(synthetic_data(iris$Petal.Width[1:50], 100), 
#                   synthetic_data(iris$Petal.Width[51:100], 100),
#                   synthetic_data(iris$Petal.Width[101:150], 100))
# summary(iris$Petal.Width[1:50])
# summary(synth_pwidth[1:100])

#create matching dataframe
colnames(iris)
#synth <- cbind(synth_slength, synth_swidth, synth_plength, synth_pwidth)
synth <- rbind(synth_setosa, synth_versi, synth_virgi)
Species = c(rep("setosa", 100), rep("versicolor", 100), rep("virginica", 100))
dim(synth)
synth <- cbind(synth, Species)
colnames(synth) <- colnames(iris)
colnames(synth)
dim(synth)
synth <- as.data.frame(synth)

#combine
more_iris <- rbind(iris, synth)

# Plots
#scatter3d(more_iris$Petal.Width~more_iris$Sepal.Length|Species,
#          data = more_iris,
#          more_iris$Petal.Width, more_iris$Sepal.Length, 
#          ellipsoid = TRUE)

scatterplot(synth$Petal.Width~synth$Sepal.Length,
            groups = levels(synth$Species),
            boxplots = "",
            grid = FALSE,
            ellipse = TRUE)
scatterplot(more_iris$Petal.Width~more_iris$Sepal.Length,
            groups = levels(more_iris$Species),
            boxplots = "",
            grid = FALSE,
            ellipse = TRUE)

dataEllipse()
meany = colMeans(iris[-5])
iris_norm = iris[-5] - meany 
plot(iris$Petal.Width ~ iris$Sepal.Length, col = "red", cex = .2, ylim = c(0, 4), xlim = c(2, 8))
points(synth$Petal.Width ~ synth$Sepal.Length, col = "blue", cex = .2)

# turning on robust makes the ellipse drawn with covariance matrix
dataEllipse(outlier_iris$Petal.Width, outlier_iris$Petal.Length,
            robust = TRUE,
            groups = levels(outlier_iris$Species))
dataEllipse(outlier_iris$Petal.Length, outlier_iris$Sepal.Length, levels=0.1*1:9, 
            ellipse.label=0.1*1:9, lty=2, fill=TRUE, fill.alpha=0.1)
m = glm(outlier_iris$Petal.Length~outlier_iris$Species + outlier_iris$Petal.Width)
print(summary(m))
confidenceEllipse(m) # how to I separate by goup? 

