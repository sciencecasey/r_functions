# mahalanobis methodology described in wrap up Data Processing Module
library(dplyr)
library(lattice)
library(here)
# import self-made function to return 
source(here("../functions/testy_testStats.R"))

# segregate table by class 
classifying_iris <- as_tibble(iris[1:50,])
classifying_iris <- cbind(classifying_iris, iris[51:100,], iris[101:150,])
str(classifying_iris)
# remove class columns
classifying_iris <- classifying_iris[-5]
classifying_iris <- classifying_iris[-9]
classifying_iris <- classifying_iris[-13]
str(classifying_iris)
# rename columns by class
colnames(classifying_iris) <- c("Setosa_SepLength", "Setosa_SepWidth", "Setosa_PetLength", "Setosa_PetWidth",
                                "Versi_SepLength", "Versi_SepWidth", "Versi_PetLength", "Versi_PetWidth",
                                "Virgi_SepLength", "Virgi_SepWidth", "Virgi_PetLength", "Virgi_PetWidth")
str(classifying_iris)
# Calculate the Mahal dist overall
classifying_iris$allMahl = mahalanobis(classifying_iris, 
                                      center = colMeans(classifying_iris), 
                                      cov = cov(classifying_iris))


#' # Make function for Mahl distance for each value by the value's mean
#' one_mahl <- function(x){
#'   #'@param x a vector of type numeric
#'   #'uses variance & mean to calculate mahalanobis distance
#'   #'@return vector of distances
#'   dist = (x - mean(x))^2*var(x)
#'   return(dist)
#' }
#' 
#' 
#' # add as columns to the tibble
#' classifying_iris$Set_SLength.Mahl<-one_mahl(classifying_iris$Setosa_SepLength)
#' classifying_iris$Versi_SLength.Mahl <- one_mahl(classifying_iris$Versi_SepLength)
#' classifying_iris$Virgi_SLength.Mahl <- one_mahl(classifying_iris$Virgi_SepLength)
#' classifying_iris$Set_SWidth.Mahl <- one_mahl(classifying_iris$Setosa_SepWidth)
#' classifying_iris$Versi_SWidth.Mahl <- one_mahl(classifying_iris$Versi_SepWidth)
#' classifying_iris$Virgi_SWidth.Mahl <- one_mahl(classifying_iris$Virgi_SepWidth)
#' classifying_iris$Set_PLength.Mahl <- one_mahl(classifying_iris$Setosa_PetLength)
#' classifying_iris$Versi_PLength.Mahl <- one_mahl(classifying_iris$Versi_PetLength)
#' classifying_iris$Virgi_PLength.Mahl <- one_mahl(classifying_iris$Virgi_PetLength)
#' classifying_iris$Set_PWidth.Mahl <- one_mahl(classifying_iris$Setosa_PetWidth)  
#' classifying_iris$Versi_PWidth.Mahl <- one_mahl(classifying_iris$Versi_PetWidth)
#' classifying_iris$Virgi_PWidth.Mahl <- one_mahl(classifying_iris$Virgi_PetWidth)

# Sort full Table: 
# organize by class (as in original table, we remove by class across all features)
# in other words, we are looking for class outliers within a feature, that won't remove key info about other features
setosa <- classifying_iris %>% select(starts_with("Set"))
virginica <- classifying_iris %>% select(starts_with("Virg"))
versicolor <- classifying_iris %>% select(starts_with("Versi"))

# save SD and Mean to watch changes after removals
setosa_stats <- summaryStats(setosa)
versicolor_stats <- summaryStats(versicolor)
virginica_stats <- summaryStats(virginica)

# plot distances to explore
boxplot(setosa[-(1:4)])
boxplot(versicolor[-(1:4)])
boxplot(virginica[-(1:4)])

# Mahalan
# # save the descending Max order
# setosa <- setosa[order(setosa$Set_PLength.Mahl, decreasing = TRUE),]
# 
# # add a column for the sorted order to compare across classes
# setosa <- 
# classifying_iris$MaxDist_PLength <- seq(1, length(classifying_iris$Sepal.Length))
# # repeat across features
# classifying_iris <- classifying_iris[order(classifying_iris$PWidth.Mahl, decreasing = TRUE),]
# classifying_iris$MaxDist_PWidth <- seq(1, length(classifying_iris$Sepal.Length))
# classifying_iris <- classifying_iris[order(classifying_iris$SLength.Mahl, decreasing = TRUE),]
# classifying_iris$MaxDist_SLength <- seq(1, length(classifying_iris$Sepal.Length))
# classifying_iris <- classifying_iris[order(classifying_iris$SWidth.Mahl),]
# classifying_iris$MaxDist_SWidth <- seq(1, length(classifying_iris$Sepal.Length))


# Summary Stats
stats <- data.frame(SLength = testy(classifying_iris$SLength.Mahl,p = .8),
                      SWidth = testy(classifying_iris$SWidth.Mahl, p = .8),
                      PLength = testy(classifying_iris$PLength.Mahl, p = .8),
                      PWidth = testy(classifying_iris$PWidth.Mahl, p = .8),
                      Overall = testy(classifying_iris$allMahl, p = .8))
str(stats)
summary(stats)
