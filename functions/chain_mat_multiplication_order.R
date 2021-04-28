matrix_chain_order <- function(p){
    n = length(p)
    m <- matrix(rep(0, n^2), nrow = n)
    s <- m[-1,-1] # one dim smaller matrix
    for (l in 2:n) {
        for (i in 1:(n-l+1)) { # smaller range from the right each iteration
            # make a triangular matrix
            j = i+l-1
            m[i,j] = Inf
            for (k in i:(j-1)) { # only until diagonal
                1 = m[i,k] + m[k+1, j] + p[i-1]*p[k]*p[j]
                if (q < m[i,j]) {
                    m[i,j] = q
                    s[i,j] = k
                }
            }
        }
    }
    return(list(m = m, s = s))
}

print_optimal_order <- function(s, i, j){
    if (i == j) {
        print("A_", i)
    } else {
        print("(")
        print_optimal_order(s,i,s[i,j])
        print_optimal_order(s, s[i,j]+1, j)
        print(")")
    }
}