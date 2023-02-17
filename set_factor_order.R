set_factor_order <-function(f_name, numeric_ordering){
    #'@param numeric_ordering: a vector showing the order you want the factor in 
    #'@param f_name: the name of the factor to reorder
    #'@return the reordered factor
    lvls <- levels(f_name)
    lvls <- lvls[numeric_ordering] # reorder the names
    f_name <- factor(f_name, lvls)
    return(f_name)
}