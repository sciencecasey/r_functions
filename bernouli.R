bernouli <- function(num_trials, total_possible_per_trial, p){
    out <- c()
    for(i in 1:num_trials){
        out <- append(out, sum(rbinom(total_possible_per_trial, 1, p)))
    } 
    return(out)}
