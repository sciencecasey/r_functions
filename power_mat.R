power_mat <- function(p, p1=p, n=2){
    n = n-1
    if(n==1){
        return(p%*%p1)
    }else{
        temp <- p%*%p1
        return(power_mat(p=temp, p1, n))
    }
}