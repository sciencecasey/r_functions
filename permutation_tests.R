permutation_test <- function(x, y, test_stat= function(x, y) mean(x)-mean(y), permutations=choose(length(x)+length(y), length(y)), alpha=.05){
    #'permutation test as discussed in 9.8 Givens and 12.1 Gentle
    #'@param x: 
    #'@param y:
    #'@param test_stat: the null hypothesis (equivalency). assuymption that under the null mean(x)-mean(y)=0
    #'@return
    
    # starting stat
    t0 <- do.call(test_stat, list(x, y))
    t_hat <- c()
    for(i in 1:permutations){
        indX <- sample(1:length(x), size = 1)
        indY <- sample(1:length(y), size = 1)
        x_new <- c(x[-indX], y[indY])
        y_new <- c(y[-indY], x[indX])
        t_hat <- append(t_hat, do.call(test_stat, list(x=x_new, y=y_new)))
    }
    # order the tj statistics
    t_hat <- t_hat[order(t_hat)]
    rank_t0 <- (which(t_hat > t0)[1])-1
    # find the confidence interval t0 should exist in to reject null
    p_rank = rank_t0/length(t_hat)
    p_rank2 = rank_t0/rank(t_hat)[length(t_hat)]
    return(list(test_0 = t0,
                test_perms = t_hat, 
                rank_t0 = rank_t0,
                p_rank = p_rank,
                p_rank2 = p_rank2))
    
}
perm <- permutation_test(dat$stomach, as.vector(na.omit(dat$breast)), permutations = 10000)
perm$test_0
perm$rank_t0
perm$p_rank
perm$p_rank2
