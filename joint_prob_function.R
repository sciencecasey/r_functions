observed_join_prob <- function(inputs){
    #'@param inputs: a dataframe of input variables to take the discrete probability of 
    
    inputs <- as.data.frame(inputs)
    # grab initial values in first col
    var <- names(inputs)[1]
    vals <- unique(inputs[,var])
    # make output dataframe
    ots <- data.frame()
    for(value in vals){
        indx <- which(inputs[,var]==value)
        others <- inputs[indx, -1] # grab values in other columns
        prob <- rep(1/nrow(inputs), length(indx))
        if(nrow(ots)<1){
            ots <- cbind(var=value, others, prob)    
        }else{
            temp <- cbind(var=value, others, prob)    
            ots <- rbind(ots, temp)
        }
        
    }
    temp <- which(duplicated(ots))
    for(i in temp){
        indx <- which(apply(df, 1,function(x) all(df[i,]==x))) #find duplicted rows
        ots[indx[1],3] <- sum(ots[indx, 3]) # add probabilities
        ots <- ots[-indx[-1],] # remove duplicates
    }
    return(ots)
}
df <- data.frame(x=c(1,1,1,0,2,3,3), y=c(1,2,3,4,5,6,6))
#observed_join_prob(df)
observed_join_prob(oil_dat[,2:4])
