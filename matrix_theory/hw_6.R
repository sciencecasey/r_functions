source("functions/gaus_elim_functions.R")
# 5.4.28
r <- matrix(c(1,2,2,1,1,0,2,0,2,0,4,-3, 0,2,0,1,2,1,4,-1,1,2,2,4), nrow = 4)
r
r <- e_lincomb(r, 2,1,-2)
r <- e_lincomb(r, 3, 1, -2)
r <- e_lincomb(r, 4,1,-1)
r <- e_lincomb(r, 2,4,-2)
r <- e_scale(r, 4,-1)
r <- e_scale(r,2,1/6)
r <- e_lincomb(r, 1,4,-1)
r <- e_lincomb(r, 4,2,-5)
r <- e_switch(r,3,4)
r <- e_switch(r, 2,3)
r <- e_lincomb(r, 1,3,3)

m <- r[,4:6]
# m multiplied by
m %*% matrix(c(0,2,0,1)) # doesn't work, should it?

# 5.5.3
r <- matrix( c(1, -1, -1, 2,
              -1,  2,  3, 1,
               2, -3, -3, 2,
               1,  1,  2, 6), nrow = 4)
library(magrittr)
r = r %>% e_lincomb(., 3,1,1) %>%
    e_lincomb(., 2,1,1) %>%
    e_lincomb(., 4,1,-2)
r
r = r %>% e_lincomb(., 3,2,-2) %>%
    e_lincomb(., 4,2,-3) %>%
    e_lincomb(.,1,2,1)
r
r %>% e_lincomb(., 1,3,-1) %>%
    e_lincomb(., 2,3,1) %>%
    e_lincomb(., 4,3,-1)
r


# 5.5.13
r <- matrix(c(1,2,-1,0, 4, 8, -4, -3, 0, 1, 3, 4, 2,5,1,4), nrow = 4)
r <- r %>% e_lincomb(2,1,-2) %>%
    e_lincomb(3,1,1)
r %>% e_lincomb(3,2,-3) %>%
    e_lincomb(4,2,-4)

