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