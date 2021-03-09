# created DCT_2 function
source("../functions/DCT_2.R")
source("../functions/pca_routine.R")

# a. take DCT of each matrix
dct_mat1 <- DCT_2(mat1)
dct_mat2 <- DCT_2(mat2)
dct_mat4 <- DCT_2(mat4)
dct_mat7 <- DCT_2(mat7)
dct_mat8 <- DCT_2(mat8)
dct_mat9 <- DCT_2(mat9)
dct_mat11 <- DCT_2(mat11)
dct_mat12 <- DCT_2(mat12)
dct_mat17 <- DCT_2(mat17)
dct_mat22 <- DCT_2(mat22)

# make a list of these matrices
all_mat <- list(dct_mat1, dct_mat2, dct_mat4, dct_mat7,
                dct_mat8, dct_mat9, dct_mat11, dct_mat12,
                dct_mat17, dct_mat22)


# b. extract horizontal, vertical, and diagonal coefficients of the transforms
#vert_vals <- matrix(rep(0, 28), ncol = 28)
#horz_vals <- matrix(rep(0, 28), ncol = 28)
#diag_vals <- matrix(rep(0, 28), ncol = 28)
hc <- matrix(rep(0, 224), nrow = 224) # hold the number of coefficients in each mast
vc <- matrix(rep(0, 224), nrow = 224)
dc <- matrix(rep(0, 335), nrow = 335) # the number of nonzero coef in diag is more
for (i in seq(1, length(newmat))){
  horiz = get_horizontal(all_mat[[i]]) # matrix of zeros in mask locations
  coef <- horiz!= 0
  hcoef <- horiz[coef] # place nonzero coefficients into a single vector
  hc <- cbind(hc, hcoef)
  vert <- get_vertical(all_mat[[i]])
  coef <- vert != 0
  vcoef <- vert[coef]
  vc <- cbind(vc, vcoef)
  diag <- get_diagonal(all_mat[[i]])
  coef <- diag!= 0
  dcoef <- diag[coef]
  dc <- cbind(dc, dcoef)
  # below was wrong, should apply over all indeces at once, not one at a time!
  # # get the pca
  # hpca = prin_cov(horiz)
  # vpca = prin_cov(vert)
  # dpca = prin_cov(diag)
  # hpca = extract_top(hpca, percent = .95)
  # vpca = extract_top(vpca, percent = .95)
  # dpca = extract_top(dpca, percent = .95)
  # horz_vals = matrix(c(horz_vals, hpca), ncol = 28)
  # vert_vals = matrix(c(vert_vals, vpca), ncol = 28)
  # diag_vals = matrix(c(diag_vals, dpca), ncol = 28)
}

# remove initial cols of zeros
hc = hc[,-1]
vc = vc[,-1]
dc = dc[,-1]

#re-orient with features as columns
hc = t(hc)
vc = t(vc)
dc = t(dc)

# correlations across fetures used solely to decide how many components to keep
rh = cor(hc)
rv = cor(vc)
rd = cor(dc)
# principle components
rh_pc = prcomp(rh, scale = TRUE)
rv_pc = prcomp(rv, scale = TRUE)
rd_pc = prcomp(rd,scale = TRUE)

# extract the components up to at least .95
# horizontal
explained = summary(rh_pc)
explained = explained$importance["Cumulative Proportion",]
keep = explained<.95
keep = explained[keep]
# grab the last (up to 95% or more)
numkeep <- length(keep)+1
hkeep <- numkeep
# vertical
explained = summary(rv_pc)
explained = explained$importance["Cumulative Proportion",]
keep = explained<.95
keep = explained[keep]
# grab the last (up to 95% or more)
numkeep <- length(keep)+1
vkeep <- numkeep
# diagonal
explained = summary(rd_pc)
explained = explained$importance["Cumulative Proportion",]
keep = explained<.95
keep = explained[keep]
# grab the last (up to 95% or more)
numkeep <- length(keep)+1
dkeep <- numkeep

# Use covariance to reduce dimensionality for the number determined w/ correlation
# covariance of the DCT coefficients
cov_h = cov(hc)
cov_v = cov(vc)
cov_d = cov(dc)
# principle components of the covariance
cd_pc = prin_cov(cov_d)
cv_pc = prin_cov(cov_v)
ch_pc = prin_cov(cov_h)

# get the top items we selected from above
pcaD <- dc %*% cd_pc$vec[,1:dkeep] # grab the indeces from dkeep multiply 10 X 335 by 335*dkeepSize = 10*dkeepSize
pcaH <- hc %*% ch_pc$vec[,1:hkeep]
pcaV <- vc %*% cv_pc$vec[,1:vkeep]

# plot!
cl <-colors()
colur <- runif(10, max = 657)
cl <- cl[colur]
plot(pcaV[,2]~pcaV[,1], col = cl, lwd = 3)
points(pcaD[,2]~pcaD[,1], col = cl, lwd = 3)
points(pcaH[,2]~pcaH[,1], col = cl, lwd = 3)
# Try boxplots
th = t(pcaH)
tv = t(pcaV)
td = t(pcaD)
boxplot(th, col = cl, lwd = 3, horizontal = TRUE, main = "Horizontal Components to 95%")
boxplot(td, col = cl, lwd = 3, horizontal = TRUE, main = "Diagonal Components to 95%")
boxplot(tv, col = cl, lwd = 3, horizontal = TRUE, main = "Vertical Components to 95%")



