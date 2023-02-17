 is_pangram <- function(s){
  str <- tolower(s)
  str <- strsplit(str, '')[[1]]
  abc <- letters
  for(i in str){
    if(i %in% abc){
      indx <- which(abc==i)
      abc <- abc[-indx]
    }
  }
  if(length(abc)>0){
    return(FALSE)
  }else{
    return(TRUE)
  }
 }

 is_pangram <- function(s){
   chars <- tolower(s)
   chars <- strsplit(chars, '')[[1]]
   pang <- all(letters %in% chars)
   pang
 }


pangram <- "The quick, brown fox jumps over the lazy dog!"
is_pangram(pangram)


