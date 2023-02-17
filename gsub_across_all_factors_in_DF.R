match_levels <- function(column, replace_this, replacement){
    #'use this with a lapply function as in: new_df <- as.data.frame(lapply(old_df[],match_levels))
    #'@param column a factor column to change some levels in 
    #'@param replace_this the regular expression to replace the first match of
    #'@param replacement the string to replace the expression with 
    #'@return the column in its original form if not a factor or with the regular expression replacement if it is a factor
    
    if(is.factor(column)){
        #levels(column) <- sapply(column, tolower)
        #levels(column) <- sapply(column,function(x) sub('\\d: |\\d : ', '', levels(x)))
        lvls <- gsub('\\d: |\\d : ', '', tolower(levels(column)))
        levels(column) <- lvls
        return(column)
    }else{
        return(column)
    }
}

# use this