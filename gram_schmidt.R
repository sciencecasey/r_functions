# Note not computationally stable: created for HW Matrix Theory
gs <- function(vectors){
    #'@param vectors a list of vectors
    #'@return a list of orthogonal vectors
    #'@author Casey Jayne

    vecs = list()
    for(i in 1:length(vectors)){
        amount = 0
        for(vals in 1:(i-1)){
            if(length(vecs)>=1){
                t = (t(vecs[[vals]]) %*% vectors[[i]])/crossprod(vecs[[vals]])
                t = t[[1]]*vecs[[vals]]
                amount = amount+t
            }
        }
        vecs[[i]] = vectors[[i]] - amount
    }

    return(vecs)
}

vec_normalize <- function(vectors){
    #'@param vectors a list of vectors
    #'@return a list of normalized vectors by 2norm
    #'@author Casey Jayne
    for(item in 1:length(vectors)){
        vectors[[item]] = vectors[[item]]/sqrt(sum(vectors[[item]]^2)) # standard 2 norm
    }

    return(vectors)
}




## this doesn't work, make a polynomial function
# example from comp stats lecture 10D
# polynomials, 1, x, x^2, x^3, ...
m <- list(matrix(c(1, 0, 0, 0)), matrix(c(0, 1, 0, 0)), matrix(c(0, 0, 1, 0)), matrix(c(0, 0, 0, 1)))
v <- gs(m)
q <- vec_normalize(v)
r = matrix(0, 4,4)
r[1,1] = sqrt(sum(v[[1]]^2))
r[2,2] = sqrt(sum(v[[2]]^2))
r[3,3] = sqrt(sum(v[[3]]^2))
r[4,4] = sqrt(sum(v[[4]]^2))
r[1,2] = crossprod(q[[1]], m[[2]])
r[1,3] = crossprod(q[[1]], m[[3]])
r[2,3] = crossprod(q[[2]], m[[3]])
r[3,4] = crossprod(q[[3]], m[[4]])
Q = matrix(unlist(q), nrow = 4)
R = matrix(unlist(r), nrow = 4)
round(Q %*% R, 2) # equal to original vectors!

# m = list(matrix(c(1,0,1)), matrix(c(0,1,1)), matrix(c(1,4,6)))
# v <- gs(m)
# # Normalize GS to get the q matrix
# q <- vec_normalize(v)
# # Create R matrix with norms of GS on diagonal
# r = matrix(0, 3,3)
# r[1,1] = sqrt(sum(v[[1]]^2))
# r[2,2] = sqrt(sum(v[[2]]^2))
# r[3,3] = sqrt(sum(v[[3]]^2))
# # Fill in R's upper triangle with original vectors cross the normalized GS
# r[1,2] = crossprod(q[[1]], m[[2]])
# r[1,3] = crossprod(q[[1]], m[[3]])
# r[2,3] = crossprod(q[[2]], m[[3]])
# # Put in matrix form to check calculations
# Q = matrix(unlist(q), nrow = 3)
# R = matrix(unlist(r), nrow = 3)
# round(Q %*% R, 2) # equal to original vectors!!
# # rounded the calculation to remove the "almost zero" in position [1,2]
#
# # Example 4.24
# v = list(matrix(c(1,1,1,-1)), matrix(c(2,-1,-1,1)), matrix(c(0,3,3,-3)), matrix(c(-1,2,2,1)))
# v = list(v[[1]], v[[2]], v[[4]])
# v = gs(v)
# v = vec_normalize(v)
# v
#
# # Example 4.26
# w = list(matrix(c(1,1,1,-1)), matrix(c(2,-1,-1,1)), matrix(c(0,3,3,-3)), matrix(c(-1,2,2,1)))
# w = list(w[[1]], w[[2]], w[[4]])
# v = gs(w)
# # Normalize GS to get the Q
# q = vec_normalize(v)
# # Create R matrix with norms of GS on diagonal
# r = matrix(0, 3,3)
# r[1,1] = sqrt(sum(v[[1]]^2))
# r[2,2] = sqrt(sum(v[[2]]^2))
# r[3,3] = sqrt(sum(v[[3]]^2))
# # Fill in R's upper triangle with original vectors cross the normalized GS
# r[1,2] = crossprod(q[[1]], w[[2]])
# r[1,3] = crossprod(q[[1]], w[[3]])
# r[2,3] = crossprod(q[[2]], w[[3]])
#
# # Put in matrix form to check calculations
# Q = matrix(unlist(q), nrow = 4)
# R = matrix(unlist(r), nrow = 3)
# Q %*% R # equal to original vectors!!
# 
# 
#################### Gram Schmidth for orthogonal polynomials
####################  This doesn't work yet!
library(pracma)

gs <- function(func_list, range, w=rep(1, length(functions))){
    #'@param func_list a list of orignal functions
    #'@param range: the range to integrate over
    #'@param w: a vector of weights
    #'@return a list of orthogonal polynomials
    #'@author Casey Jayne
    
    q_1 <- func_list[[1]]
    # make a matrix of inner products as a lookup table
    # ip <- matrix(nrow=length(func_list), ncol=length(func_list))
    # for(i in 1:nrow(ip)){
    #     for(j in 1:ncol(ip)){
    #         # integration is the inner product of polynomials over the range
    #         ip[i, j] <- round(integral(as.function(wrap_funcs(func_list[[i]], func_list[[j]])), range[1], range[2]), 4)
    #     }
    # }
    out_funcs <- as.list(rep(NA, length(func_list)))
    out_written <- as.list(rep(NA, length(func_list)))
    out_funcs[[1]] <- q_1
    names(out_funcs)[1] <- 'q_1'
    out_written[[1]] <- body(q_1)
    names(out_written)[1] <- 'q_1'
    for(i in 2:length(func_list)){
       others <- seq(i-1, 1, -1)
       q <- func_list[[i]]
       q_printed <- format(body(func_list[[i]]))
       for(j in others){
           # get the norms we need using the 
           c_num <- integral(as.function(wrap_funcs(eval(out_funcs[[j]], environment(out_funcs[[j]])), func_list[[i]])), range[1], range[2])
           c_den <- integral(as.function(wrap_funcs(eval(out_funcs[[j]], environment(out_funcs[[j]])), eval(out_funcs[[j]], environment(out_funcs[[j]])))), range[1], range[2])
           c <- ifelse(is.nan(c_num/c_den), 0, c_num/c_den, 4)
           #c <- ip[others[j], i]/ip[others[j], others[j]]
           if(c==0){
               temp <- function(x) 0
               temp_printed <- 0
           }else{
               #temp <- func_times_constant(round(c, 4), eval(out_funcs[[j]], environment(out_funcs[[j]])))    
               #temp_printed <- paste0(round(c, 4), format(body(eval(out_funcs[[j]], environment(out_funcs[[j]])))))
               temp <- func_times_constant(c, func_list[[j]])    
               temp_printed <- paste0(c, format(body(func_list[[j]])))
           }
           q <- subtract_funcs(q, temp)
           q_printed <- paste0(q_printed, '-', temp_printed)
       }
       #q <-  paste(func_list[[i]], '-', ip[others, i]/ip[others, others],'*', func_list[[others]])
       out_funcs[[i]] <- q
       out_written[[i]] <- q_printed
       names(out_funcs)[i] <- paste0('q_', i) 
       names(out_written)[i] <- paste0('q_', i) 
    }
    return(list(funcs = out_funcs, 
                written = out_written))
}

func_times_constant <- function(c, f1){
    #'@param c: a constant
    #'@param f1: a function with one argument
    #return(function(y) c*eval(f1, environment(f1)))
    return(function(y) c*do.call(f1, list(y)))
}

subtract_funcs <- function(f1, f2){
    #'@param f1: a function with one argument
    #'@param f2: a function with one arguement
    #'@return f1-f2 as a function
    return(function(y) do.call(f1, list(y)) - do.call(f2, list(y)))
}

multiply_funcs <- function(f1, f2){
    #'@param f1: a function with one argument
    #'@param f2: a function with one arguement
    return(function(y) do.call(f1, list(y))*do.call(f2, list(y)))
}

g <- gs(func_list = list(function(x) 1, function(x) x, function(x) x^2, function(x) x^3, function(x) x^4), range = c(-1, 1))
g$funcs
g$written


vec_normalize <- function(vectors){
    #'@param vectors a list of vectors
    #'@return a list of normalized vectors by 2norm
    #'@author Casey Jayne
    for(item in 1:length(vectors)){
        vectors[[item]] = vectors[[item]]/sqrt(sum(vectors[[item]]^2)) # standard 2 norm
    }
    
    return(vectors)
}

