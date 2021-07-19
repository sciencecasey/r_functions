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
