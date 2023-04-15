row_reduced<- function(data){
    #'@param data a data frame or matrix of numeric data
    #'@return a matrix in row reduced form
    #'@author Casey Jayne
    num_rows <- dim(data)[1]
    num_cols <- dim(data)[2]
    p = 1
    q = 1
    while(p<=num_rows & q<=num_cols){
        i = p
        while(data[i, q] == 0){
            i = i+1
            if(i>num_rows){
                i = 1
                q = q+1
                if(q>num_cols){
                    # index out of bounds! no more reduction
                    return(data)
                }
            }
        }
        # indx != 0
        if(i!=p){
            # we've moved
            temp1 = data[i,] # grab the row corresponding to leading entry
            temp2 = data[p,]
            data[p,] = temp1 # switch with the pth row
            data[i,] = temp2
        }
        if(data[p, q] != 1){
            # make pivot = 1
            data[p,] = data[p,]*(1/data[p,q])
        }
        # check for non-zero rows above & below
        items = which(data[,q] != 0)
        for(row in items){
            if(row != p){
                # zero out rows above and below
                vals = data[p,]*(-data[row,q])
                data[row,] = data[row,] + vals
            }
        }
        p = p+1
        q = q+1
    }
    return(data)
}

# # From example on pg 30
# attempt = matrix(c(1,2,4,
#                    1,2,6,
#                    1,4,8,
#                    4,11,24), nrow = 3) # fills columnwise
# attempt
# row_reduced(attempt)
#
# attempt = matrix(c(0,2,4,
#                    1,2,6,
#                    1,0,8,
#                    4,11,24), nrow = 3) # fills columnwise
# attempt
# row_reduced(attempt)
#
# attempt = matrix(c(0,2,4,
#                    1,0,6,
#                    1,0,8,
#                    4,11,24), nrow = 3) # fills columnwise
# row_reduced(attempt)

#attempt <- matrix(c(4,-1,3,-1,5,4,2,-1,1,6,-3,3), nrow = 3)
#attempt[1,] <- attempt[1,] + attempt[2,]*4
#attempt[3,] <- attempt[2,]*3 + attempt[3,]
#attempt[3,] <- -1*attempt[1,] + attempt[3,]
#attempt[2,] <- attempt[2,]*-1
#t <- attempt[1,]
#attempt[1,] <- attempt[2,]
#attempt[2,] <- t
#attempt
