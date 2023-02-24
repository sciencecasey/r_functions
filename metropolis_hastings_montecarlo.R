metrop_hastings <- function(f_target, g_propsal_sample, g_propsal_compute, g_condition_name, support_from=0, support_to=1, n_draws=1000, ...){
    #'@param f_target: the target function to draw from
    #'@param g_propsal_sample: the proposal markov chain conditional function to draw from
    #'@param g_proposal_compute: the function to calculate the density of proposal g using drawn sample from g_proposal_sample
    #'@param g_condition_name: the name of the parameter passed to g_proposal_sample and g_proposal_compute which the proposed item is conditioned on
    #'@param support_from, support_to: the domain from which we can draw the first value x0 with a positive return from f_target evaluated at it
    #'@param n_draws: how many samples to draw from f_target
    #'@param ...: additional parameters to pass to g_proposal_sample and g_proposal_compute
    #'@return: a vector of values from f_target
    # draw x_0 from support
    x0 <- sample(seq(support_from, support_to, by=.1), size = 1)
    if(f_target(x0) <= 0){
        errorCondition('Support region inappropriately named.  Please make support only positive region of F')
    }
    out_list <- c()
    while(length(out_list)<n_draws){
        g_condition_prev <- list(x0)
        names(g_condition_prev) <- g_condition_name
        x_prop <- g_propsal_sample(1, unlist(g_condition_prev), ...)
        g_condition_prop <- list(x_prop)
        names(g_condition_prop) <- g_condition_name
        r <- f(x_prop)*g_propsal_compute(x0, unlist(g_condition_prop), ...)/(f(x0))*g_propsal_compute(x_prop, unlist(g_condition_prev), ...)
        if(r>=1){
            out_list <- append(out_list, x_prop)
        }else if(runif(1, 0, 1)<r){
            out_list <- append(out_list, x_prop)
        }else{
            out_list <- append(out_list, x0)
        }
        x0 <- out_list[length(out_list)] # grab most recent value
    }
    return(out_list)
        
}
out <- metrop_hastings(function(x) exp(-x), g_propsal_sample = rnorm, g_propsal_compute = dnorm, g_condition_name = 'mean', sd=.1)
out <- metrop_hastings(function(x) exp(-x), g_propsal_sample = rnorm, g_propsal_compute = dnorm, g_condition_name = 'mean', sd=.01)
out <- metrop_hastings(function(x) exp(-x), g_propsal_sample = rnorm, g_propsal_compute = dnorm, g_condition_name = 'mean', sd=.001)

unconditioned_metrop_hastings <- function(f_target, g_propsal_sample, g_propsal_compute, support_from=0, support_to=1, n_draws=1000, ...){
    #'@param f_target: the target function to draw from
    #'@param g_propsal_sample: the proposal markov chain conditional function to draw from
    #'@param g_proposal_compute: the function to calculate the density of proposal g using drawn sample from g_proposal_sample
    #'@param g_condition_name: the name of the parameter passed to g_proposal_sample and g_proposal_compute which the proposed item is conditioned on
    #'@param support_from, support_to: the domain from which we can draw the first value x0 with a positive return from f_target evaluated at it
    #'@param n_draws: how many samples to draw from f_target
    #'@param ...: additional parameters to pass to g_proposal_sample and g_proposal_compute
    #'@return: a vector of values from f_target
    # draw x_0 from support
    x0 <- sample(seq(support_from, support_to, by=.1), size = 1)
    if(f_target(x0) <= 0){
        errorCondition('Support region inappropriately named.  Please make support only positive region of F')
    }
    out_list <- c()
    while(length(out_list)<n_draws){
        x_prop <- g_propsal_sample(1, ...)
        r <- f(x_prop)*g_propsal_compute(x0, ...)/(f(x0))*g_propsal_compute(x_prop, ...)
        if(r>=1){
            out_list <- append(out_list, x_prop)
        }else if(runif(1, 0, 1)<r){
            out_list <- append(out_list, x_prop)
        }else{
            out_list <- append(out_list, x0)
        }
        x0 <- out_list[length(out_list)] # grab most recent value
    }
    return(out_list)
    
}
unconditioned_metrop_hastings(function(x) exp(-x), g_propsal_sample = runif, g_propsal_compute = dunif, min=0, max=5)
