example_matrix1 <- c(rep(0, 32), rep(255, 32))
dim(example_matrix1) <- c(8,8)
example_matrix1 <- as.matrix(example_matrix1)
library(dtt)
exampleDCT <- mvdct(example_matrix1)

# Modeled exactly from DCT_2.m written by Dr. Rodriguez
DCT_2 = function(B){
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

tesyMat <- matrix(c(maxi, meany, minni, 1,2,3,4), ncol = 4, nrow = 4)  
DCT_2(tesyMat)


NPDCT = function(image){
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
        # npdct[(k+1), (n+1)] = sqrt(solve(image)) # inverse used in matlab code but gives NAs here
        npdct[(k+1), (n+1)] = sqrt(2/(N)) # using same method as below instead ? is this okay?
        # cat(paste(npdct[(k+1), (n+1)]))
      } else {
        temp = cos((pi*(2*n+1)*k)/(2*N))
        npdct[(k+1), (n+1)] = sqrt(2/N)*temp # 1/sqrt(image) is equivalent to sqrt(2/image)
        # cat(paste(npdct[(k+1), (n+1)]))
      }
    }
  }
  transformed = getdct(npdct, image)
  return(transformed)
}

getdct = function(npdct, image){
  npdct <- as.matrix(npdct)
  image <- as.matrix(image)
  transforms = npdct%*%image%*%npdct
  return(transforms)
}

# same matrix from Annie's example
out1 = NPDCT(example_matrix1)
out2 = DCT_2(example_matrix1)
out1 == out2
