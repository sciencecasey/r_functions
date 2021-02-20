# https://www.statology.org/mahalanobis-distance-r/?unapproved=15591&moderation-hash=0e1f9bf2cbd5670479253a01b29d36fc#comment-15591

df = data.frame(score = c(91, 93, 72, 87, 86, 73, 68, 87, 78, 99, 95, 76, 84, 96, 76, 80, 83, 84, 73, 74),
                hours = c(16, 6, 3, 1, 2, 3, 2, 5, 2, 5, 2, 3, 4, 3, 3, 3, 4, 3, 4, 4),
                prep = c(3, 4, 0, 3, 4, 0, 1, 2, 1, 2, 3, 3, 3, 2, 2, 2, 3, 3, 2, 2),
                grade = c(70, 88, 80, 83, 88, 84, 78, 94, 90, 93, 89, 82, 95, 94, 81, 93, 93, 90, 89, 89))

str(df)
summary(df)

# built in function takes the df of data, 
# the mean vector of the distribution & covariance matrix as inputs
mahalanobis(df, colMeans(df, na.rm = TRUE), cov(df, method = "pearson"))
mahalanobis(df)
# multiple distances given, one for each row of the data frame 
# add col of distances to df

df$mahalanobis <- mahalanobis(df, colMeans(df, na.rm = TRUE), cov(df, method = "pearson"))

# determine if any are statistically significant ?? what would this mean
# p value that corresponds to Chi-Square stat of the distance, 
# k-1 DOF: k is number of variables (as we calculated params for each? 
# it looks to me like we should have 8 bc we calculated cov & mean) ??
# (recall this is a squared dist with skew that often looks at variances)

df$p <- pchisq(df$mahalanobis, df = 2, lower.tail = FALSE)
df
