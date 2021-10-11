trun <- function(string, from_pos, replace_with=""){
    #'@param string a string from which to remove the end
    #'@param from_pos a positional integer less than length of string after which all characters will be removed
    #'@param replace_with an optional string to change the ending to.
    #'@return the string containing all values up to and including the index and ending with string in replace_with
    t <- paste(read.fwf(textConnection(string),
                        c(from_pos, nchar(string)),
                        as.is=T), collapse=replace_with)
    return(t)
}