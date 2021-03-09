# mahalanobis methodology described in wrap up Data Processing Module
library(dplyr)
library(here)
library(latticeExtra)
library(car) # for scatterplot ellipse
# import self-made function to return 
source(here("../functions/testy_testStats.R"))

outlier_iris = as_tibble(iris)
str(outlier_iris)
# remove the Species for the calculation
# calculate by each class
sepal_mahal <- mahalanobis(outlier_iris[1:50,-5], 
                           center = colMeans(outlier_iris[1:50, -5]), 
                           cov = cov(outlier_iris[1:50, -5]))
versi_mahal <- mahalanobis(outlier_iris[51:100,-5], 
                           center = colMeans(outlier_iris[51: 100, -5]), 
                           cov = cov(outlier_iris[51: 100, -5]))
virgi_mahal <- mahalanobis(outlier_iris[51:100,-5], 
                           center = colMeans(outlier_iris[51: 100, -5]), 
                           cov = cov(outlier_iris[51: 100, -5]))
outlier_iris$allMahal <- c(sepal_mahal, versi_mahal, virgi_mahal)

# check if any reach sig. by 
# Check if any reach significance by the Chi squared critical value for dist
# Check if any reach sign. by wilks' outlier method


# Make function for Mahl distance for each value by the value's mean
one_mahl <- function(x){
  #'@param x a vector of type numeric
  #'uses variance & mean to calculate mahalanobis distance
  #'@return vector of distances
  dist = (x - mean(x))^2*var(x)
  return(dist)
}
outlier_iris[1:50,-5]

# add as columns to the tibble
outlier_iris$SLength.Mahl<-one_mahl(outlier_iris$Sepal.Length)
outlier_iris$SWidth.Mahl <- one_mahl(outlier_iris$Sepal.Width)                          
outlier_iris$PLength.Mahl <- one_mahl(outlier_iris$Petal.Length)
outlier_iris$PWidth.Mahl <- one_mahl(outlier_iris$Petal.Width) 


# Plot to see as graph
xyplot(outlier_iris$PWidth.Mahl ~ outlier_iris$PLength.Mahl, 
       groups= outlier_iris$Species, 
       auto.key = TRUE,
       xlab = "Mahal. Petal Width",
       ylab = "Mahal. Petal Length",
       type = "p")

xyplot(outlier_iris$PWidth.Mahl ~ outlier_iris$SWidth.Mahl,
       groups= outlier_iris$Species, #col= c("blue", "black", "red"),
       auto.key = TRUE,
       xlab = "Mahal. Petal Length",
       ylab = "Mahal. Sepal Width",
       type = "p")

scatterplot(outlier_iris$Petal.Width ~ outlier_iris$Petal.Length,
            groups = levels(outlier_iris$Species),
            boxplots = "",  # Disable boxplots
            grid = FALSE,   # Disable plot grid
            ellipse = TRUE)#,
            #robust = TRUE) # Draw ellipses

# turning on robust makes the ellipse drawn with covariance matrix
dataEllipse(outlier_iris$Petal.Width, outlier_iris$Petal.Length,
            robust = TRUE,
            groups = levels(outlier_iris$Species))
dataEllipse(outlier_iris$Petal.Length, outlier_iris$Sepal.Length, levels=0.1*1:9, 
            ellipse.label=0.1*1:9, lty=2, fill=TRUE, fill.alpha=0.1)
m = glm(outlier_iris$Petal.Length~outlier_iris$Species + outlier_iris$Petal.Width)
print(summary(m))
confidenceEllipse(m) # how to I separate by goup? 
# Using boxplot code suggestions from: 
# https://r-coder.com/boxplot-r/
boxplot(outlier_iris$PLength.Mahl ~ outlier_iris$Species, horizontal = TRUE)
stripchart(outlier_iris$PLength.Mahl ~ outlier_iris$Species, method = "jitter",
           pch = 5, cex = .5, add = TRUE, col = 1:length(levels(outlier_iris$Species)))
# potential outlier in virginica

boxplot(outlier_iris$PWidth.Mahl~ outlier_iris$Species, horizontal = TRUE)
stripchart(outlier_iris$PWidth.Mahl~ outlier_iris$Species, method = "jitter",
           pch = 5, cex = .5, add = TRUE, col = 1:length(levels(outlier_iris$Species)))
# too close to suspect outliers - calculate

boxplot(outlier_iris$SLength.Mahl~ outlier_iris$Species, horizontal = TRUE)
stripchart(outlier_iris$SLength.Mahl~ outlier_iris$Species, method = "jitter",
           pch = 5, cex = .5, add = TRUE, col = 1:length(levels(outlier_iris$Species)))
# potential outlier in virginica

boxplot(outlier_iris$SWidth.Mahl~ outlier_iris$Species, horizontal = TRUE)
stripchart(outlier_iris$SWidth.Mahl ~ outlier_iris$Species, method = "jitter",
           pch = 5, cex = .5, add = TRUE, col = 1:length(levels(outlier_iris$Species)))
# potential outlier in setosa

# Sort full Table: 
# save the descending Max order
## the smallest difference = the last in the order of distances
classifying_iris <- classifying_iris[order(classifying_iris$PLength.Mahl, decreasing = TRUE),]
# add a column for the sorted order to compare across classes
classifying_iris$MaxDist_PLength <- seq(1, length(classifying_iris$Sepal.Length))
# repeat across features
classifying_iris <- classifying_iris[order(classifying_iris$PWidth.Mahl, decreasing = TRUE),]
classifying_iris$MaxDist_PWidth <- seq(1, length(classifying_iris$Sepal.Length))
classifying_iris <- classifying_iris[order(classifying_iris$SLength.Mahl, decreasing = TRUE),]
classifying_iris$MaxDist_SLength <- seq(1, length(classifying_iris$Sepal.Length))
classifying_iris <- classifying_iris[order(classifying_iris$SWidth.Mahl),]
classifying_iris$MaxDist_SWidth <- seq(1, length(classifying_iris$Sepal.Length))


# Summary Stats
stats <- data.frame(SLength = testy(classifying_iris$SLength.Mahl,p = .8),
                    SWidth = testy(classifying_iris$SWidth.Mahl, p = .8),
                    PLength = testy(classifying_iris$PLength.Mahl, p = .8),
                    PWidth = testy(classifying_iris$PWidth.Mahl, p = .8),
                    Overall = testy(classifying_iris$allMahl, p = .8))
str(stats)
summary(stats)