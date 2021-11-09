aitken_r2_cutoff.lm <- function(mod, alpha = .05){
    #'@param mod an lm or glm object
    #'@param a cutoff criteria for the F distribution

    r2 <- 1-summary(mod)$r.squared
    F_0 <- qf(alpha, length(mod$coefficients)-1, mod$df.residual, lower.tail = F)
    d <- ((length(mod$coefficients)-1)*F_0)/mod$df.residual
    r2_0 <- 1-r2-r2*d

    return(r2_0)
}

aitken_r2_cutoff.ols <- function(mod, alpha = .05){
    #'@param mod an anova_stata object
    #'@param a cutoff criteria for the F distribution
    #'@return the r2_0 for the assumption population's true model is represented in the model passed
    r2 <- 1-mod$anova$SS[1]/mod$anova$SS[3]
    F_0 <- qf(alpha, mod$anova$DOF[1], mod$anova$DOF[2], lower.tail = F)
    d <- mod$anova$DOF[1]*F_0/mod$anova$DOF[2]

    r2_0 <- 1-r2-r2*d

    return(r2_0)
}
aitken_r2_cutoff <- function(mod, alpha = .05){
    #'@param mod an lm, glm, or anova_stata object
    #'@param a cutoff criteria for the F distribution
    ifelse(class(mod) == 'lm', aitken_r2_cutoff.lm(mod, alpha), aitken_r2_cutoff.ols(mod, alpha))
}