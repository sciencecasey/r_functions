NPDCT = function(image){
  #'@param image an N*N matrix
  #'@return the DCT coefficients
  #'@author Casey Jayne Richards
  #'relies on the getdct helper function to calculate the DCT2 coefficents
  #'credit to K.R. Rao for the Math behind this function & 
  #'Dr. Ben Rodriguez for assistance with structure and debugging
  npdct = rep(0, (dim(image)[1]*dim(image)[2]))
  dim(npdct) <- c(dim(image)[2], dim(image)[1]) 
  npdct <- as.matrix(npdct) #make same size matrix of zeros
  k = 0
  n = 0
  N = dim(image)[1]
  for (k in seq(0, N-1)){
    for(n in seq(0, N-1)){
      if (k == 0){
        # first pass
        npdct[(k+1), (n+1)] = sqrt(1/(N)) # normalizing the data to create to first vector
      } else {
        temp = cos((pi*(2*n+1)*k)/(2*N))
        npdct[(k+1), (n+1)] = sqrt(2/N)*temp # 1/sqrt(image) is equivalent to sqrt(2/image)
      }
    }
  }
  transformed = getdct(npdct, image)
  return(transformed)
}

getdct = function(npdct, image){
  #'@param npdct the result of an np-dct calculation
  #'@param the matrix from with the np-dct was computed
  #'@return the DCT2 coefficients
  #'@author Casey Jayne Richards
  #'credit to K.R. Rao for the Math behind this function & 
  #'Dr. Ben Rodriguez for assistance with structure and debugging
  npdct <- as.matrix(npdct)
  image <- as.matrix(image)
  transforms = npdct%*%image%*%t(npdct)
  return(transforms)
}


