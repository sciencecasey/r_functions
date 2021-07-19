e_lincomb <- function(data, y, x, scalar){
    #'@param data a numeric dataframe or matrix
    #'@param y the row index to act on
    #'@param x the row index to get scalar multiple of
    #'@param scalar the amount to scale row x by
    #'@return the data with y = y + scalary*x
    data[y,] <- data[y,] + scalar*data[x,]
    return(data)
}

e_scale <- function(data, y, scalar){
    #'@param data a numeric dataframe or matrix
    #'@param x the row index to scale
    #'@param scalar the amount to scale row y by
    #'@return the data with y = scalar*y

    data[y,] <- scalar*data[y,]
    return(data)
}

e_switch <- function(data, y, x){
    #'@param data a numeric dataframe or matrix
    #'@param y the row to switch with x
    #'@param x the row to switch with y
    #'@return the data with y = x & x = y

    tx <- data[x,]
    ty <- data[y,]
    data[x,] <- ty
    data[y,] <- tx
    return(data)
}
