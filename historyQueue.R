# Inheritance -----------------------------------------------------
# Note that this isn't very efficient - it's just for illustrating inheritance.
HistoryQueue <- R6Class("HistoryQueue",
                        inherit = Queue,
                        public = list(
                          show = function() {
                            cat("Next item is at index", private$head_idx + 1, "\n")
                            for (i in seq_along(private$queue)) {
                              cat(i, ": ", private$queue[[i]], "\n", sep = "")
                            }
                          },
                          remove = function() {
                            if (private$length() - private$head_idx == 0) return(NULL)
                            private$head_idx <<- private$head_idx + 1
                            private$queue[[private$head_idx]]
                          }
                        ),
                        private = list(
                          head_idx = 0
                        )
)

hq <- HistoryQueue$new(5, 6, "foo")
hq$show()
#> Next item is at index 1
#> 1: 5
#> 2: 6
#> 3: foo
hq$remove()
#> [1] 5
hq$show()
#> Next item is at index 2
#> 1: 5
#> 2: 6
#> 3: foo
hq$remove()
#> [1] 6



# Calling superclass methods with super$ --------------------------
CountingQueue <- R6Class("CountingQueue",
                         inherit = Queue,
                         public = list(
                           add = function(x) {
                             private$total <<- private$total + 1
                             super$add(x)
                           },
                           get_total = function() private$total
                         ),
                         private = list(
                           total = 0
                         )
)

cq <- CountingQueue$new("x", "y")
cq$get_total()
#> [1] 2
cq$add("z")
cq$remove()
#> [1] "x"
cq$remove()
#> [1] "y"
cq$get_total()
#> [1] 3
