importance_sampling <- function(target_function, orig_density_function, imp_sample_function, imp_density_function, n_samples=1000){
    # sample from g
    y <- imp_sample_function(n_samples) # Y~g
    # calculate the w* weights
    # original density function
    fy <- tryCatch(orig_density_function(y), error=function(e) sapply(y, orig_density_function))
    # weighted variance function
    gy <- tryCatch(imp_density_function(y), error=function(e) sapply(y, imp_density_function))
    # correction for weighting
    wy <- fy/gy
    # external function 
    hy <- tryCatch(target_function(y), error=function(e) sapply(y, target_function))
    return(list(mc_mean=mean((hy*wy)),
                mc_var = (1/n_samples*var(hy*wy)),
                mc_sample = y, 
                mc_weight = wy, 
                mc_target = hy))
}

# from  lecture-- target function is network failure when at least 4 total failures, original pdf is bernouli with 20 trials and p=.05, importance function has p=.25
bernouli <- function(num_trials, total_possible_per_trial, p){
    out <- c()
    for(i in 1:num_trials){
        out <- append(out, sum(rbinom(total_possible_per_trial, 1, p)))
    } 
    return(out)}
out <- importance_sampling(target_function = function(num_broken) {num_broken[which(num_broken<4)]=0; num_broken[which(num_broken!=0)]=1; return(num_broken)}, 
                    orig_density_function = function(num_broken) .05^num_broken*(1-.05)^(20-num_broken),
                    imp_sample_function = function(num) bernouli(num, 20, .25), 
                    n_samples = 10000,
                    imp_density_function = function(num_broken) .25^(num_broken)*(1-.25)^(20-num_broken))
                    #orig_density_function = function(x) dbinom(x, 20, .05), 
                    #imp_density_function = function(y) dbinom(y, 20, .25))
                    
                    
out$mc_mean
out$mc_var

# same as above
# out <- importance_sampling(target_function = function(j) {j[which(j<4)]=0; j[which(j!=0)]=1; return(j)}, 
#                            orig_density_function = function(b) .05^b*(1-.05)^(20-b),
#                            imp_sample_function = function(y) rbinom(y, 20, .25), 
#                            n_samples = 10000,
#                            imp_density_function = function(b) .25^(b)*(1-.25)^(20-b))

# from example here: https://www.statlect.com/asymptotic-theory/importance-sampling
out <- importance_sampling(target_function = function(x)10*exp(-5*(x-3)^4),
                    orig_density_function = function(x) dnorm(x, 0, 1),
                    imp_sample_function = function(x) rnorm(x, 3, 1),
                    imp_density_function = function(x) dnorm(x, 3, 1),
                    n_samples = 10000)
out$mc_mean
out$mc_var

coefficient_variation <- function(dat){
    return(var(dat)/mean(dat))
}