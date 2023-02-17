hessian_matrix <- function(gx, respect_to){
    #'@param gx: a function to calculate second derivs of
    #'@param respect_to: a character vector of parameters to calc. parital derivatives over
    out_mat <- matrix("", nrow=length(respect_to), ncol=length(respect_to))
    for(i in 1:length(respect_to)){
        for(j in 1:length(respect_to)){
            dthetad2x <- D(D(gx, respect_to[i]), respect_to[j])
            out_mat[i,j] <- deparse(dthetad2x)
        }
    }
    return(out_mat)
}

# g <- expression(x^3-2*x*y-y^6)
# h_g <- hessian_matrix(g, respect_to = c('x', 'y'))
# h_g

solve_hessian_matrix <- function(h, values){
    #'@param h: a hessian matrix where the expressions are saved as strings, as created using hessian_matrix
    #'@param values: a list of named values to put into the hessian matrix
    #'@return 
    out <- apply(h,  1:2, \(x) eval(str2lang(x), values))
    rownames(out) <- names(values)
    colnames(out) <- names(values)
    return(out)
}

# solve_hessian_matrix(h_g, list(x=1, y=2))
# 
# # alternative
# g <- expression(x^3 - 2 * x * y - y^6)
# attr(eval(deriv3(g, c('x','y')),list(x=1,y=2)), 'hessian')

hessian <- function(g,values){
    #'@param g: an expression on which to calculate the hessian matrix
    #'@param values: a named list of parameters and their values to calculated the hessian at
    #'@return a hessian matrix
    nms <- names(values)
    f <- eval(deriv3(g, nms),as.list(values))
    matrix(attr(f, 'hessian'), length(values), dimnames = list(nms,nms))
}
# 
# hessian(g, c(x=1,y=2))
# 
# g <- expression(x^3-2*x*y-y^6)
# h_g <- hessian_env(g, respect_to = c('x', 'y'))