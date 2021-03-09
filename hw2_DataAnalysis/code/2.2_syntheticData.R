library(lattice)
library(latticeExtra)
library(car)
synthetic_data <- function(x, n, dist = NULL, centered = TRUE){
  #'@param x a feature vector from which to generate more data
  #'@param n the number of synthetic datas to create
  #'@return positive numbers normalized to fit the data
  #'@param dist if entered as "gaussian" or "normal" the random numbers will
  #'@param centered if the data is centered around the mean
  #' be generated from a normal distribution.  default uses uniformly distributed
  #' random values
  if(!centered){
    # center the data
    x <- sapply(x, normalize)
  }
  cols = dim(x)[2]
  rows = dim(x)[1]
  minnie = sapply(x, min)
  maxie = sapply(x, max)
  cov = cov(x)
  mean = colMeans(x)
  if(is.null(dist)){
    #generate uniformly distributed numbers between 1 and 100 for each col
    syn = runif((n*cols))
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
  cov = as.matrix(cov)
  syn = syn %*% cov # multiply by covariance matrix to orient
  syn = as.data.frame(syn)
  syn_min = sapply(syn, min)
  syn_max = sapply(syn, max)
  syn = ((syn - syn_min)/(syn_max-syn_min))*(maxie - minnie) + minnie
  #center around the mean
  mu = colMeans(syn)-colMeans(x)
  syn = syn - mu
  return(abs(syn))
}
iris_centerd <- sapply(iris[-5], normalize)
synth_setosa <- synthetic_data(iris_centerd[(1:50), -5], 100)
summary(synth_setosa)
summary(iris_centerd[(1:50), -5])
synth_versi <- synthetic_data(iris_centerd[(51:100), -5], 100)
synth_virgi <- synthetic_data(iris_centerd[(101:150), -5], 100)

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
temp <- cbind(iris_centerd, Species = iris$Species)
more_iris <- rbind(temp, synth)

# synthetic data from testy analysis
iris_centerd <- as.data.frame(temp)
pdf("output/synthetic_data_testyFunc.pdf")
plot(iris_centerd$Petal.Width ~ iris_centerd$Sepal.Length,
     col = "red", cex = .2, ylim = c(-1, 2), xlim = c(-1, 2),
     xlab = "Sepal Length", ylab = "Petal Width", main = "Testy Function Synthetic Data")
points(synth$Petal.Width ~ synth$Sepal.Length, col = "blue", cex = .2)
summary(iris_centerd)
summary(synth[-5])
dev.off()

# compare with built in functions
# make synthetic
iris_normed <- cbind(runif(300), runif(300), runif(300), runif(300))
# multiply by the covariance of centered iris data
covy = cov(iris_centerd[1:50,-5])
iris_normed[1:100,] <- as.matrix(iris_normed[1:100,]) %*% covy
covy = cov(iris_centerd[51:100,-5])
iris_normed[101:200,] <- as.matrix(iris_normed[101:200,]) %*% covy
covy = cov(iris_centerd[101:150,-5])
iris_normed[201:300,] <- as.matrix(iris_normed[201:300,]) %*% covy
# min max normalizaion
iris_normed <- cbind(normalize(iris_normed[,1], min = min(iris_centerd[,1]), max = max(iris_centerd[,1])),
                     normalize(iris_normed[,2], min = min(iris_centerd[,2]), max = max(iris_centerd[,2])),
                     normalize(iris_normed[,3], min = min(iris_centerd[,3]), max = max(iris_centerd[,3])),
                     normalize(iris_normed[,4], min = min(iris_centerd[,4]), max = max(iris_centerd[,4])))
# center around the iris_centerd mean
colMeans(iris_centerd[-5])
colMeans(iris_normed)
diff <- colMeans(iris_normed) - colMeans(iris_centerd[-5])
iris_normed <- iris_normed - diff

# This one worked!!
Species = c(rep("setosa", 100), rep("versicolor", 100), rep("virginica", 100))
dim(iris_normed)
iris_normed <- cbind(iris_normed, Species)
colnames(iris_normed) <- colnames(iris)
colnames(iris_normed)
dim(iris_normed)
iris_normed <- as.data.frame(iris_normed)
iris_centerd <- as.data.frame(iris_centerd)
#pdf("output/synthetic_data_prebuildFunc.pdf")
plot(iris_centerd$Petal.Width ~ iris_centerd$Sepal.Length, col = "red", cex = .2,
     ylim = c(-1, 2), xlim = c(-1, 2), xlab = "Sepal Length", ylab = "Petal Width",
     main = "Pre-Built Function Synthetic Data")
points(iris_normed$Petal.Width ~ iris_normed$Sepal.Length, col = "blue", cex = .2)
#dev.off()


# Plots
more_iris$type <- c(rep("original_centered", 150), rep("synthetic", 300))
str(synth)
synth$Species <- as.factor(synth$Species)
iris_centerd$Species <- iris$Species
#pdf = ("output/Testy_ellipse.pdf")
scatterplot(iris_centerd$Petal.Width~iris_centerd$Sepal.Length,
            groups = levels(iris_centerd$Species),
            xlim = c(-1, 3),
            ylim = c(-1, 2),
            xlab = "Sepal Length",
            ylab = "Petal Width",
            main = "Testy Synthetic overlaid on Iris",
            boxplots = "",
            #col = "red", # carPalette()[5],
            grid = FALSE,
            ellipse = list(TRUE, fill = FALSE, robust = TRUE))
points(synth$Petal.Width ~ synth$Sepal.Length, col = "blue", cex = .2)
#dev.off()
str(iris_centerd)
iris_normed$Species <- as.factor(iris_normed$Species)
iris_normed$Sepal.Length <- as.numeric(iris_normed$Sepal.Length)
iris_normed$Sepal.Width <- as.numeric(iris_normed$Sepal.Width)
iris_normed$Petal.Length <- as.numeric(iris_normed$Petal.Length)
iris_normed$Petal.Width <- as.numeric(iris_normed$Petal.Width)
scatterplot(iris_normed$Petal.Width~iris_normed$Sepal.Length,
            groups = levels(iris_normed$Species),
            xlim = c(-1, 3),
            ylim = c(-1, 2),
            xlab = "Sepal Length",
            ylab = "Petal Width",
            main = "Testy Synthetic overlaid on Normalized Iris",
            boxplots = "",
            grid = FALSE,
            ellipse = list(TRUE, fill = FALSE, robust = TRUE))
points(synth$Petal.Width ~ synth$Sepal.Length, col = "blue", cex = .2)


# turning on robust makes the ellipse drawn with covariance matrix
dataEllipse(iris_centerd$Petal.Width, iris_centerd$Petal.Length,
            robust = TRUE,
            groups = iris_centerd$Species)
#dataEllipse(outlier_iris$Petal.Length, outlier_iris$Sepal.Length, levels=0.1*1:9,
#            ellipse.label=0.1*1:9, lty=2, fill=TRUE, fill.alpha=0.1)
#m = glm(outlier_iris$Petal.Length~outlier_iris$Species + outlier_iris$Petal.Width)
#print(summary(m))
#confidenceEllipse(m) # how to I separate by goup?

