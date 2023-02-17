write_matex <- function(x) {
    #'@param x a matrix object
    #'@param author: Blended via Stack Exchange
    #' source: https://stackoverflow.com/questions/45591286/for-r-markdown-how-do-i-display-a-matrix-from-r-variable
    #' write a matrix in Latex Format
    begin <- "$$\\begin{bmatrix}"
    end <- "\\end{bmatrix}$$"
    X <-
        apply(x, 1, function(x) {
            paste(
                paste(x, collapse = "&"),
                "\\\\"
            )
        })
    writeLines(c(begin, X, end))
}
write_vectex <- function(x){
    #'@param x: a vector
    write_matex(as.matrix(x))
}