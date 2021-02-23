# mahalanobis methodology described in wrap up Data Processing Module
library(dplyr)
library(here)
# import self-made function to return 
source(here("../functions/testy_testStats.R"))

classifying_iris = as_tibble(iris)
str(classifying_iris)
# remove the Species for the calculation
classifying_iris$allMahl = mahalanobis(classifying_iris[-5], 
                                      center = colMeans(classifying_iris[-5]), 
                                      cov = cov(classifying_iris[-5]))


# Make function for Mahl distance for each value by the value's mean
one_mahl <- function(x){
  #'@param x a vector of type numeric
  #'uses variance & mean to calculate mahalanobis distance
  #'@return vector of distances
  dist = (x - mean(x))^2*var(x)
  return(dist)
}

#############We want outliers of classes so we need to do this by class!
# add as columns to the tibble
classifying_iris$SLength.Mahl<-one_mahl(classifying_iris$Sepal.Length)
classifying_iris$SWidth.Mahl <- one_mahl(classifying_iris$Sepal.Width)                          
classifying_iris$PLength.Mahl <- one_mahl(classifying_iris$Petal.Length)
classifying_iris$PWidth.Mahl <- one_mahl(classifying_iris$Petal.Width)  

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
