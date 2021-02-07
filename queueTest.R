environment()
typeof(q)
typeof(Queue)
typeof(x)
ls()
myQ = Queue$new("v", 7, 9)
myQ$getHead()
myQ$add("this")
myj = myQ$clone()
myj$getHead()
q <- Queue$new(5, 6, "foo")
q$getHead()
myQ = Queue$new("v", 7, 9)
myQ$getHead()
myQ$add("this")
myj = myQ$clone()
myj$getHead()
# Add and remove items
q$add("something")
q$add("another thing")
q$add(17)
q$remove()
#> [1] 5
q$remove()
#> [1] 6

# Private members can't be accessed directly
q$queue
#> NULL
# q$length()
#> Error: attempt to apply non-function

# add() returns self, so it can be chained
q$add(10)$add(11)$add(12)

# remove() returns the value removed, so it's not chainable
q$remove()
#> [1] "foo"
q$remove()
#> [1] "something"
q$remove()
#> [1] "another thing"
q$remove()
#> [1] 17
