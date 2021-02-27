library(EnvStats)
library(dplyr)
library(formattable)
# kurtosis(iris$Sepal.Length[1:50,])


testy = function(x, p = NULL){
  #' @param x a vector of values from which to return the statistics
  #' @param p the number of values to remove from the end(s) of x 
  #' @return a vector of named statistics including the mean
  #' maximum, minimum, trimmed mean, SD, skew, and kurtosis
  #' calculated using base R and EnvStats package
  (n = length(x))
  (min = min(x))
  (max = max(x))
  sd = sd(x)
  mean = mean(x)
  skew = skewness(x, na.rm = TRUE)
  kurty = kurtosis(x, na.rm = TRUE)
  if(!is.null(p)){
    if(p<0 || p>length(x)){
      cat(paste("Error in the number of items to remove"))
      trimmed_mean = NA
      break
    }
    p = round(p) # make sure no decimal passed
    while(p > 1){
      x = sort(x)
      x = x[-1] # remove first item
      x = x[-length(x)] # remove last item
      p = p-2 # 2 removed
    }
    if(p == 1){
      x = sort(x)
      x = x[-length(x)]
      p = 0
    }
    trimmed_mean = mean(x) # the mean less the removed values
    return(c(min = min, max = max, sd = sd, mean = mean, 
             trimmed_mean = trimmed_mean, 
             skew = skew, kurtosis = kurty)) 
  }
  return(c(min = min, max = max, sd = sd, mean = mean, 
           skew = skew, kurtosis = kurty))
}


# Sepal Length calculation & place in Data Frame
sepal_Length <- data.frame(testy(iris$Sepal.Length[1:50], 1),
                           testy(iris$Sepal.Length[51:100], 1),
                           testy(iris$Sepal.Length[101:150],  1))
# rename the columns
sepal_Length = cbind(rownames(sepal_Length), sepal_Length)
types <- c("Stat", "Setosa", "Versicolor", "Virginica")
colnames(sepal_Length) <- types
# add column for feature (used when combining)
sepal_Length <- cbind(sepal_Length, Feature = rep("Sepal Length", 7))


# repeat with Sepal Width 
sepal_width <- data.frame(testy(iris$Sepal.Width[1:50], 1),
                          testy(iris$Sepal.Width[51:100], 1),
                          testy(iris$Sepal.Width[101:150], 1))
sepal_width <- cbind(rownames(sepal_width), sepal_width)
colnames(sepal_width) <- types
sepal_width <- cbind(sepal_width, Feature = rep("Sepal Width", 7))

# repeat with petal length
petal_Length <- data.frame(testy(iris$Petal.Length[1:50], 1),
                           testy(iris$Petal.Length[51:100], 1),
                           testy(iris$Petal.Length[101:150], 1))

petal_Length = cbind(rownames(petal_Length), petal_Length)
types <- c("Stat", "Setosa", "Versicolor", "Virginica")
colnames(petal_Length) <- types
petal_Length <- cbind(petal_Length, Feature = rep("Petal Length", 7))

# repeat with petal width
petal_width <- data.frame(testy(iris$Petal.Width[1:50], 1),
                          testy(iris$Petal.Width[51:100], 1),
                          testy(iris$Petal.Width[101:150], 1))
petal_width <- cbind(rownames(petal_width), petal_width)
colnames(petal_width) <- types
petal_width <- cbind(petal_width, Feature = rep("Petal Width", 7))

# combine features
output <- rbind(sepal_Length, sepal_width, petal_Length, petal_width)
# sort by the statistic then Feature for ease when reading
output <- group_by(output, Stat, Feature) %>% 
  arrange(output, .by_group = TRUE) 


pdf("output/statistic_table_Iris.pdf")
formattable(output, align = c("l", rep("r", NCOL(output))))
# dev.flush()
dev.off()      
