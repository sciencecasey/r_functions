library(EnvStats)
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
    }
  }else{
      trimmed_mean = NA
  }
  return(c(min = min, max = max, sd = sd, mean = mean,
             trimmed_mean = trimmed_mean,
             skew = skew, kurtosis = kurty))
}


