reverse_numeric <- function(num){
  #'@param num an integer number at most 22 digits long
  #'@return the number in reverse order
  if(nchar(num)>22){
    return(warningCondition('Only works for 22 digits'))
  }
  num <- strtoi(paste0(rev(unlist(strsplit(as.character(num), ""))), collapse=''))
  return(num)
}

as.numeric(reverse_numeric(12345))
