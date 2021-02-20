library(EnvStats)
library(dplyr)
library(formattable)



testy = function(x, p = NULL){
  #' @param x a vector of values from which to return the statistics
  #' @param p a number between 1 and 0 for percent to trim from mean
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
    if(p<1 && p>0){
      trimmed_mean = 1/(n-2*p) * sum(x[(p+1):(n-p)])  
    }else{
      trimmed_mean = NA
    }
    return(c(min = min, max = max, sd = sd, mean = mean, 
             trimmed_mean = trimmed_mean, 
             skew = skew, kurtosis = kurty)) 
  }
  return(c(min = min, max = max, sd = sd, mean = mean, 
           skew = skew, kurtosis = kurty))
}
summaryFull(iris$Sepal.Length)
# Sepal Length calculation & place in Data Frame
sepal_Length <- data.frame(testy(iris$Sepal.Length[1:50]),
                           testy(iris$Sepal.Length[51:100]),
                           testy(iris$Sepal.Length[101:150]))
# rename the columns
sepal_Length = cbind(rownames(sepal_Length), sepal_Length)
types <- c("Stat", "Setosa", "Versicolor", "Virginica")
colnames(sepal_Length) <- types
# add column for feature (used when combining)
sepal_Length <- cbind(sepal_Length, Feature = rep("Sepal Length", 6))


# repeat with Sepal Width 
sepal_width <- data.frame(testy(iris$Sepal.Width[1:50]),
                          testy(iris$Sepal.Width[51:100]),
                          testy(iris$Sepal.Width[101:150]))
sepal_width <- cbind(rownames(sepal_width), sepal_width)
colnames(sepal_width) <- types
sepal_width <- cbind(sepal_width, Feature = rep("Sepal Width", 6))


# combine features
output <- rbind(sepal_Length, sepal_width)
# sort by the statistic then Feature for ease when reading
output <- group_by(output, Stat, Feature) %>% 
  arrange(output, .by_group = TRUE) 


pdf("statistic_table_Iris.pdf")
formattable(output, align = c("l", rep("r", NCOL(output))))
dev.off()      
