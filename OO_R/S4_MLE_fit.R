#Start with S3 Generics
foo = structure(list(), class = "foo")
class(foo)
inherits(foo, "foo")
#create generic method type
f = function(x){
  UseMethod("f")
}
typeof(f)
isS3stdGeneric(f)
f.a = function(x){
  cat("Called the function using a \"Class a\" type")
}
a = structure(list(), class= "a")
f(a)
f.default = function(x){
  cat("Uknown class type")
}
f(structure(list, class = c("b")))

library(stats4)
#http://adv-r.had.co.nz/OO-essentials.html#s4
y = c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
nLL = function(lambda) - sum(dpois(y, lambda, log = TRUE))
fit = mle(nLL, start = list(lambda = 5), nobs = length(y))

isS4(fit)
attributes(fit)
typeof(fit)

isS4(nobs)
typeof(nobs)
attributes(nobs)

mle_nobs = method_from_call(nobs(fit))
isS4(mle_nobs)
methods("t.test")
methods("nLL")

library(rlang)
#infix functions
`%+%` <- function(a, b) paste(a, b)
"new" %+% " string"

#replacement function
`second<-` <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:10
second(x) <- 5
x     
second(x) = 3
x
#second(x, 2)  ##error could not find function second
library("pryr")
address(x)
second(x) = 3
pryr::address(x)

#modifying by position
`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
modify(x, 2) <- 10
x

#Restore on Exiting
getwd()
in_dir = function(tempDir){
  orig = getwd()
  on.exit(setwd(orig))
  
  setwd(tempDir)
}
in_dir("~")
getwd()
