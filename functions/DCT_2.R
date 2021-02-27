DCT_2 = function(B){
  #'@param B takes in a matrix 
  #'@return the DCT2 coefficients
  #'@author Casey Jayne Richards 
  #'credit to K.R. Rao for the Math behind this function & 
  #'Dr. Ben Rodriguez for assistance with structure
  N1 <- dim(B)[1]
  N2 <- dim(B)[2]
  B_DCT = rep(0, N1*N2)
  dim(B_DCT) <- c(N1, N2)
  B_DCT = as.matrix(B_DCT)
  for (k1 in seq(0, N1-1)){
    for(k2 in seq(0, N2-1)){
      tmp = 0
      for(n1 in seq(0, N1-1)){
        for(n2 in seq(0, N2-1)){
          tmp = tmp + B[(n1+1), (n2+1)]*
            cos((pi*(2*n1+1)*k1)/(2*N1))*
            cos((pi*(2*n2+1)*k2)/(2*N2))        
        }
      }
      b1 = get_b(k1,N1);
      b2 = get_b(k2,N2);
      B_DCT[k1+1,k2+1] = b1*b2*tmp;
    }
  }
  return(B_DCT)
}

get_b = function(k, N){
  if (k ==0){
    b = 1/sqrt(N)
  }else{
    b = sqrt(2/N)
  }
}  