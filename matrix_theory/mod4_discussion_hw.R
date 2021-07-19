all_bin_mats <- function(){
    mat <- matrix(0, nrow = 4, ncol = 4)
    all <- list(mat)
    not_at_end <- TRUE
    round <- 0
    while(not_at_end){
        prevmat <- mat
        for (i in 1:4){
            for(j in 1:4){
                if(mat[i,j] == 0){
                    mat[i,j] <- 1 # add a one
                    all[[length(all)+1]] <- mat
                }
                if(i == 4 & j ==4){ # finished this round
                    # add one more one
                    total = sum(mat) # number of ones
                    if(total == 16){
                        not_at_end = FALSE
                    }else{
                        round <- round + 1
                        col <- total %% 4
                        if(col == 0){
                            col = 4
                        }
                        row <- floor((total-col)/ 4)+1
                        prevmat[row,col] <- 1
                        # all[[length(all)+1]] <- prevmat
                    }
                }
                mat <- prevmat
            }
        }
    }
    return(all)
}
r = all_bin_mats()
ranks <- c()
library(pracma)
for(item in 1:length(r)){
    ranks[item]<- Rank(r[[item]])
}
hist(ranks)
barplot(table(ranks))

leall_012_mats <- function(list_mats){
    compiled_list <- list_mats
    start_here <- 1
    end_of_list <- length(compiled_list)
    not_at_end <- TRUE
    while(not_at_end){
        round <- 1
        for(item in start_here:end_of_list){
            # for each item, add one two
            matty <- compiled_list[[item]]
            reset_mat <- matty
            for (i in 1:4){
                for(j in 1:4){
                    if(matty[i,j]!=2){
                        if(item<length(list_mats)){
                            # still in original list
                            t <- matty
                            matty[i,j] <- 0
                            if(sum(list_mats[[item]]) > (sum(matty))){
                                # not enough ones,
                                # we already have this matrix in the list
                                matty <-t
                            }else{
                                matty[i,j] <- 2
                                compiled_list[[length(compiled_list)+1]] <- matty
                                matty <- reset_mat
                            }
                        }else{
                            matty[i,j] <- 2
                            compiled_list[[length(compiled_list)+1]] <- matty
                            matty <- reset_mat
                        }
                    }
                }
            }
        }
        # now go through and add another 2 until at 16 2s
        start_here <- end_of_list+1
        end_of_list <- length(compiled_list)
        if(sum(compiled_list[[end_of_list]]) == 32){
            not_at_end <- FALSE
        }
    }
    return(compiled_list)
}
r2 = all_012_mats(r)
ranks <- c()
library(pracma)
for(item in 1:length(r2)){
    ranks[item]<- Rank(r2[[item]])
}
hist(ranks)