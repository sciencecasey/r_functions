#https://rstudio-education.github.io/hopr/speed.html
long = rep(c(-1, 1), 5000000)
abs_loop <- function(vec){
  for (i in 1:length(vec)) {
    if (vec[i] < 0) {
      vec[i] <- -vec[i]
    }
  }
  vec
}
abs_sets <- function(vec){
  negs <- vec < 0
  vec[negs] <- vec[negs] * -1
  vec
}

system.time(abs_loop(long))
system.time(abs_sets(long))

#built in absolute value
system.time(abs(long))

#assignment operators are weird here
x = 1
x <- 3
2 -> x
