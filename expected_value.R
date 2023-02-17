#' integrate_numerically <- function(f, from, to, sub_ints = 200) {
#'   #'@param f: an pdf expression which can be computed.  Should have a single input variable, x
#'   #'@param from: integrate from
#'   #'@param to: integrate to
#'   #'@param sub_ints: the number of sub intervals to integrate over. Default 200.
#'   #'@return sum of the integral
#'   sub_size <- (to - from) / sub_ints
#'   x <- seq(from, to - sub_size, sub_size)
#'   sum(f(x) * sub_size)
#' }
#'
#' integrate_numerically(dnorm, -1.96, 1.96)
#' integrate_numerically(dcauchy, -1.96, 1.96)
#' integrate_numerically(dchisq, -1.96, 1.96) # can't no param df
#' # show comparison with built-in
#' integrate(dnorm, -1.96, 1.96)

expected_value <- function(f, params, from=-Inf, to=Inf){
  #'@param f: an expression with one input value
  #'@param params: a list of additional parameters to pass to f
  #'@param from/to: range to integrate over. Defaults -Inf, Inf
  #'@return expected value calculated by integrating x*f(x) over range
  integrate(function(x) x * f(x, unlist(params)), from, to)
}

#expected_value(dnorm, params=list(mean=5))
#expected_value(dchisq, params=list(df=2))
