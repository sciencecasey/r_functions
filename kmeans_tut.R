#' K means clustering in r practice multiple ways
#' https://www.statology.org/k-means-clustering-in-r/

# built in function

df <- USArrests
df <- na.omit(df)
df <- scale(df) # normalize the dataset


# General form/background
# nstart: The number of initial configurations. 
# Because it’s possible that different initial 
# starting clusters can lead to different results, 
# it’s recommended to use several different 
# initial configurations. 
# The k-means algorithm will find the initial 
# configurations that lead to the smallest 
# within-cluster variation.
# keans(data = df, centers = colMeans(df), nstart = ??)

# Try to find the optimal number of clusters
# use plots to decide
library(factoextra)
# look for elbow
fviz_nbclust(df, FUNcluster = kmeans, method = "wss") # within sum-squares
# looks like k = 4
# check gap statistic
# compares intra-cluster variation for different values of k with 
# their expected values for dist. w/o clustering
library(cluster)
gap <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap)
# gap stat highest at k = 4


# perform klustering!
#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 4 clusters
km <- kmeans(df, centers = 4, nstart = 25)

#view results
km
print(summary(km))

# this uses the first 2 principle components
fviz_cluster(km, data = df)

#find means of each cluster
aggregate(USArrests, 
          by=list(cluster=km$cluster), mean)

df <- cbind(df, kmean_cluster = km$cluster)

# alternative, use kmeaods!!
# uses medians rather than means so more robust
# if there are outliers

# find optimal clusters for dataset
# pam stands for partition around median
# takes input:
# data: Name of the dataset.
# k: The number of clusters.
# metric: The metric to use to calculate 
# distance. Default is euclidean but you 
# could also specify manhattan.
# stand: Whether or not to standardize 
# each variable in the dataset. Default = FALSE.
# test with a graph
fviz_nbclust(df, FUNcluster = pam, method = "wss")
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df,
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
# again, 4
#make this example reproducible
set.seed(1)

#perform k-medoids clustering with k = 4 clusters
kmed <- pam(df, k = 4)

#view results
kmed
#plot results of final k-medoids model
fviz_cluster(kmed, data = df)

# compare with kmeans
df <- cbind(df, med_cluster = kmed$clustering)
df <- as.data.frame(df)

# check where different
bool <- (df$kmean_cluster != df$med_cluster)
sum(bool) # 21 are different, looks like they might just have swapped 1 and 4
bool1 <- df$med_cluster == 1
bool4 <- df$med_cluster == 4
df$med_cluster[bool1] = 4
df$med_cluster[bool4] = 1
# recheck
bool <- (df$kmean_cluster != df$med_cluster)
sum(bool) # perfect overlap!
