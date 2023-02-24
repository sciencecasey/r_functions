expression_to_function <- function(expr){
    vars <- gsub('expression', '', deparse(expr))
    vars <- gsub('exp', '', vars)
    vars <- gsub('ln', '', vars)
    vars <- strsplit(vars, '')[[1]]
    vars <- vars[which(vars != '*' & vars != '+'& vars != '-' & vars != '/'& vars != ' ' & vars != ''& vars != '(' & vars != ')' & vars != '^')]
    # remove any numbers and replace with spaces
    vars <- gsub('[1-9]', ' ', vars)
    seps <- which(vars == ' ')
    v <- list() # output variables
    if(length(seps<1)){
        # only one variable
        vars <- str_c(vars, collapse = '')
    }else {
        if(!(1 %in% seps)){
            # the first value starts a variable
            v <- append(v, str_c(vars[1:i-1], collapse=''))
            vars <- vars[i-1:length(vars)]
        }
        for(i in 1:length(seps)){
            if(length(vars>seps[i])){
                tryCatch(
                v <- append(v, str_c(vars[seps[i]:(seps[i+1]-1)], collapse='')),
                v <- append(v, str_c(vars[seps[i]:length(vars)], collapse=''))
                )
            }
        }
        
    }
    return(v)
    f <- function(x) { }
    body(f) <- eval(expr)
    return(f)
}
expression_to_function(expression(4*x))
