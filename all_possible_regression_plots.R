# adjest the number in label and pos to correspond to the number of regressors

# only selecting the largest R2
r2 <- c()
label <- c()
for(num in unique(all_pos$n)){
    sub <- all_pos$predictors[all_pos$n==num]
    i <- which.max(all_pos$rsquare[all_pos$n==num])
    label[num] <- sub[i]
    r2[num] <- max(all_pos$rsquare[all_pos$n==num])
}
plot(r2~unique(all_pos$n), ylab = 'R Squared', xlab = 'Number of Regressors',
     ylim = c(.6, 1), main = 'Largest R2 for K regressors'); text(
         r2~unique(all_pos$n), labels=c(label[-length(r2)], 'all'), pos=c(1,1,1,1,1,3,1,3), cex = .75)
# adjusted
label <- c()
r2 <- c()
for(num in unique(all_pos$n)){
    sub <- all_pos$predictors[all_pos$n==num]
    i <- which.max(all_pos$adjr[all_pos$n==num])
    label[num] <- sub[i]
    r2[num] <- max(all_pos$adjr[all_pos$n==num])
}
plot(r2~unique(all_pos$n), ylab = 'Adjusted R Squared', xlab = 'Number of Regressors',
     ylim = c(.6, 1), main = 'Largest Adjusted R2 for K regressors'); text(
         r2~unique(all_pos$n), labels=c(label[-length(r2)], 'all'), pos=c(1,1,1,1,1,3,1,3), cex = .75)



# only selecting the smallest CP
cp <- c()
label <- c()
for(num in unique(all_pos$n)){
    sub <- all_pos$predictors[all_pos$n==num]
    i <- which.min(all_pos$cp[all_pos$n==num])
    label[num] <- sub[i]
    cp[num] <- min(all_pos$cp[all_pos$n==num])
}
plot(cp~unique(all_pos$n), ylab = 'C_p', xlab = 'Number of Regressors', ylim = c(-1, 8),
     xlim = c(0, 8), main = 'Smallest C_p for K regressors'); text(
         cp~unique(all_pos$n), labels=c(label[-length(r2)], 'all'), pos=c(3,1,1,3,1,3,1), cex = .75)

# mse
mse <- c()
label <- c()
for(num in unique(all_pos$n)){
    sub <- all_pos$predictors[all_pos$n==num]
    i <- which.min(all_pos$msep[all_pos$n==num])
    label[num] <- sub[i]
    mse[num] <- min(all_pos$msep[all_pos$n==num])
}
plot(mse~unique(all_pos$n), ylab = 'MSE', xlab = 'Number of Regressors',
     main = 'Smallest MSE for K regressors'); text(
         mse~unique(all_pos$n), labels=c(label[-length(r2)], 'all'), pos=c(1,1,1,1,3,3,3,3,3), cex = .75)
