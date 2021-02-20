# mahalanobis methodology described in wrap up Data Processing Module
library(dplyr)
library(here)
# import self-made function to return 
source(here("functions/testy_testStats.R"))

classifying_iris = as_tibble(iris)
str(classifying_iris)
# remove the Species
classifying_iris = subset(classifying_iris, select = -Species)
classifying_iris$allMahl = mahalanobis(classifying_iris, 
                                      center = colMeans(classifying_iris), 
                                      cov = cov(classifying_iris))


# just making sure cov has vars on diagonal
cov(classifying_iris)
var(classifying_iris$Sepal.Length)
var(classifying_iris$Sepal.Width)


one_mahl <- function(x){
  #'@param x a vector of type numeric
  #'uses variance & mean to calculate mahalanobis distance
  #'@return vector of distances
  dist = (x - mean(x))^2*var(x)
  return(dist)
}

classifying_iris$SLength.Mahl<-one_mahl(classifying_iris$Sepal.Length)
classifying_iris$SWidth.Mahl <- one_mahl(classifying_iris$Sepal.Width)                          
classifying_iris$PLength.Mahl <- one_mahl(classifying_iris$Petal.Length)
classifying_iris$PWidth.Mahl <- one_mahl(classifying_iris$Petal.Width)                          

stats <- data.frame(testy(classifying_iris$SLength.Mahl,p = .8),
                      testy(classifying_iris$SWidth.Mahl, p = .8),
                      testy(classifying_iris$PLength.Mahl, p = .8),
                      testy(classifying_iris$PWidth.Mahl, p = .8),
                      testy(classifying_iris$allMahl, p = .8))
str(stats)
summary(stats)
