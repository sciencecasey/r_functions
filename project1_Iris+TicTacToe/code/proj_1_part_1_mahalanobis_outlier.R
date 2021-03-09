# mahalanobis methodology described in wrap up Data Processing Module
library(dplyr)
library(lattice)
library(here)
library(car)
# import self-made function to return 
source(here("../functions/testy_testStats.R"))

# segregate table by class 
classifying_iris <- as_tibble(iris[1:50,])
classifying_iris <- cbind(classifying_iris, iris[51:100,], iris[101:150,])
str(classifying_iris)
# remove class columns
classifying_iris <- classifying_iris[-5]
classifying_iris <- classifying_iris[-9]
classifying_iris <- classifying_iris[-13]
str(classifying_iris)
# rename columns by class
colnames(classifying_iris) <- c("Setosa_SepLength", "Setosa_SepWidth", "Setosa_PetLength", "Setosa_PetWidth",
                                "Versi_SepLength", "Versi_SepWidth", "Versi_PetLength", "Versi_PetWidth",
                                "Virgi_SepLength", "Virgi_SepWidth", "Virgi_PetLength", "Virgi_PetWidth")
str(classifying_iris)
# Calculate the Mahal dist overall
classifying_iris$allMahl = mahalanobis(classifying_iris, 
                                       center = colMeans(classifying_iris), 
                                       cov = cov(classifying_iris))


# Make function for Mahl distance for each value by the value's mean
one_mahl <- function(x){
    #'@param x a vector of type numeric
    #'uses variance & mean to calculate mahalanobis distance
    #'@return vector of distances
    dist = (x - mean(x))^2*var(x)
    return(dist)
}


# add as columns to the tibble
# separate the distances by both class and feature
classifying_iris$Set_SLength.Mahl<-one_mahl(classifying_iris$Setosa_SepLength)
classifying_iris$Versi_SLength.Mahl <- one_mahl(classifying_iris$Versi_SepLength)
classifying_iris$Virgi_SLength.Mahl <- one_mahl(classifying_iris$Virgi_SepLength)
classifying_iris$Set_SWidth.Mahl <- one_mahl(classifying_iris$Setosa_SepWidth)
classifying_iris$Versi_SWidth.Mahl <- one_mahl(classifying_iris$Versi_SepWidth)
classifying_iris$Virgi_SWidth.Mahl <- one_mahl(classifying_iris$Virgi_SepWidth)
classifying_iris$Set_PLength.Mahl <- one_mahl(classifying_iris$Setosa_PetLength)
classifying_iris$Versi_PLength.Mahl <- one_mahl(classifying_iris$Versi_PetLength)
classifying_iris$Virgi_PLength.Mahl <- one_mahl(classifying_iris$Virgi_PetLength)
classifying_iris$Set_PWidth.Mahl <- one_mahl(classifying_iris$Setosa_PetWidth)
classifying_iris$Versi_PWidth.Mahl <- one_mahl(classifying_iris$Versi_PetWidth)
classifying_iris$Virgi_PWidth.Mahl <- one_mahl(classifying_iris$Virgi_PetWidth)


# Sort full Table: 
# organize by class (as in original table, we remove by class across all features)
# in other words, we are looking for class outliers within a feature, that won't remove key info about other features
setosa <- classifying_iris %>% select(starts_with("Set"))
virginica <- classifying_iris %>% select(starts_with("Virg"))
versicolor <- classifying_iris %>% select(starts_with("Versi"))

# save SD and Mean to watch changes after removals
orig_setosa_stats <- summaryStats(setosa)
orig_versicolor_stats <- summaryStats(versicolor)
orig_virginica_stats <- summaryStats(virginica)

# plot distances to explore
cl = colors()
cl = c(cl[10], cl[20], cl[30], cl[40])
boxplot(setosa[-(1:4)], col = cl, xlab = "Features")
boxplot(versicolor[-(1:4)], col = cl, xlab = "Features")
boxplot(virginica[-(1:4)], col = cl, xlab = "Features")

# check the descending Max order (for visual inspection)
setosa <- setosa[order(setosa$Set_PLength.Mahl, decreasing = TRUE),]
# flag outside 3 SD of the mean
crit1 <- mean(setosa$Set_PLength.Mahl) + 3*sd(setosa$Set_PLength.Mahl)
crit2 <- mean(setosa$Set_PLength.Mahl) - 3*sd(setosa$Set_PLength.Mahl)
# count number of ouliers
sum(setosa$Set_PLength.Mahl > crit1 | setosa$Set_PLength.Mahl < crit2)
# 3 outliers! find location
index <- c(setosa$Set_PLength.Mahl > crit1 | setosa$Set_PLength.Mahl < crit2)
# add flag row for outlier
setosa$flag <- rep(0, length(setosa$Setosa_SepLength)) # start with zero counts
setosa$flag[index] = setosa$flag[index]+1 
# repeat across features
# Petal Width
setosa <- setosa[order(setosa$Set_PWidth.Mahl, decreasing = TRUE),]
crit1 <- mean(setosa$Set_PWidth.Mahl) + 3*sd(setosa$Set_PWidth.Mahl)
crit2 <- mean(setosa$Set_PWidth.Mahl) - 3*sd(setosa$Set_PWidth.Mahl)
# count number of ouliers
sum(setosa$Set_PWidth.Mahl > crit1 | setosa$Set_PWidth.Mahl < crit2)
# 1 outlier! find location
index <- c(setosa$Set_PWidth.Mahl > crit1 | setosa$Set_PWidth.Mahl < crit2)
# add to flag
setosa$flag[index] = setosa$flag[index]+1 
# Sepal Length
setosa <- setosa[order(setosa$Set_SLength.Mahl, decreasing = TRUE),]
crit1 <- mean(setosa$Set_SLength.Mahl) + 3*sd(setosa$Set_SLength.Mahl)
crit2 <- mean(setosa$Set_SLength.Mahl) - 3*sd(setosa$Set_SLength.Mahl)
# count number of ouliers
sum(setosa$Set_SLength.Mahl > crit1 | setosa$Set_SLength.Mahl < crit2)
# 1 outlier! find location
index <- c(setosa$Set_SLength.Mahl > crit1 | setosa$Set_SLength.Mahl < crit2)
# add to flag
setosa$flag[index] = setosa$flag[index]+1
# Sepal Width
setosa <- setosa[order(setosa$Set_SWidth.Mahl, decreasing = TRUE),]
crit1 <- mean(setosa$Set_SWidth.Mahl) + 3*sd(setosa$Set_SWidth.Mahl)
crit2 <- mean(setosa$Set_SWidth.Mahl) - 3*sd(setosa$Set_SWidth.Mahl)
# count number of ouliers
sum(setosa$Set_SWidth.Mahl > crit1 | setosa$Set_SWidth.Mahl < crit2)
# 1 outlier! find location
index <- c(setosa$Set_SWidth.Mahl > crit1 | setosa$Set_SWidth.Mahl < crit2)
# add to flag
setosa$flag[index] = setosa$flag[index]+1
# See if anything is flagged more than once
sum(setosa$flag>1) # everything flagged only once
# don't remove any as would affect rest of sample too much (relatively small sample)

# Virginica
# flag outside 3 SD of the mean
m_mean <- mean(virginica$Virgi_PLength.Mahl)
m_sd <- sd(virginica$Virgi_PLength.Mahl)
crit1 <- m_mean + 3*m_sd
crit2 <- m_mean - 3*m_sd
# count number of ouliers
sum(virginica$Virgi_PLength.Mahl > crit1 | virginica$Virgi_PLength.Mahl < crit2) # 1 outlier!
index <- c(virginica$Virgi_PLength.Mahl > crit1 | virginica$Virgi_PLength.Mahl < crit2)
virginica$flag <- rep(0, length(virginica[,1]))
# add to flag
virginica$flag[index] = virginica$flag[index]+1
# repeat across features
# Petal Width
m_mean <- mean(virginica$Virgi_PWidth.Mahl)
m_sd <- sd(virginica$Virgi_PWidth.Mahl)
crit1 <- m_mean + 3*m_sd
crit2 <- m_mean - 3*m_sd
# count number of ouliers
sum(virginica$Virgi_PWidth.Mahl > crit1 | virginica$Virgi_PWidth.Mahl < crit2) # 1 outlier
index <- c(virginica$Virgi_PWidth.Mahl > crit1 | virginica$Virgi_PWidth.Mahl < crit2)
virginica$flag[index] = virginica$flag[index]+1
# Sepal Length
m_mean <- mean(virginica$Virgi_SLength.Mahl)
m_sd <- sd(virginica$Virgi_SLength.Mahl)
crit1 <- m_mean + 3*m_sd
crit2 <- m_mean - 3*m_sd
# count number of ouliers
sum(virginica$Virgi_SLength.Mahl > crit1 | virginica$Virgi_SLength.Mahl < crit2)
# 1 outlier! find location
index <- c(virginica$Virgi_SLength.Mahl > crit1 | virginica$Virgi_SLength.Mahl < crit2)
# add to flag
virginica$flag[index] = virginica$flag[index]+1
# Sepal Width
m_mean <- mean(virginica$Virgi_SWidth.Mahl)
m_sd <- sd(virginica$Virgi_SWidth.Mahl)
crit1 <- m_mean + 3*m_sd
crit2 <- m_mean - 3*m_sd
# count number of ouliers
sum(virginica$Virgi_SWidth.Mahl > crit1 | virginica$Virgi_SWidth.Mahl < crit2)
# 3 outliers! find location
index <- c(virginica$Virgi_SWidth.Mahl > crit1 | virginica$Virgi_SWidth.Mahl < crit2)
# add to flag
virginica$flag[index] = virginica$flag[index]+1
# See if anything is flagged more than once
sum(virginica$flag>1) # everything flagged only once
# don't remove any as would affect rest of sample too much (relatively small sample)

# try again with versicolor
versicolor$flag <- rep(0, length(versicolor[,1]))
m_mean <- mean(versicolor$Versi_PLength.Mahl)
m_sd <- sd(versicolor$Versi_PLength.Mahl)
crit1 <- m_mean + 3*m_sd
crit2 <- m_mean - 3*m_sd
# count number of ouliers
sum(versicolor$Versi_PLength.Mahl > crit1 | versicolor$Versi_PLength.Mahl < crit2) # 1 outlier!
index <- c(versicolor$Versi_PLength.Mahl > crit1 | versicolor$Versi_PLength.Mahl < crit2)
# add to flag
versicolor$flag[index] = versicolor$flag[index]+1
# repeat across features
# Petal Width
m_mean <- mean(versicolor$Versi_PWidth.Mahl)
m_sd <- sd(versicolor$Versi_PWidth.Mahl)
crit1 <- m_mean + 3*m_sd
crit2 <- m_mean - 3*m_sd
# count number of ouliers
sum(versicolor$Versi_PWidth.Mahl > crit1 | versicolor$Versi_PWidth.Mahl < crit2) # 1 outlier
index <- c(versicolor$Versi_PWidth.Mahl > crit1 | versicolor$Versi_PWidth.Mahl < crit2)
versicolor$flag[index] = versicolor$flag[index]+1
# Sepal Length
m_mean <- mean(versicolor$Versi_SLength.Mahl)
m_sd <- sd(versicolor$Versi_SLength.Mahl)
crit1 <- m_mean + 3*m_sd
crit2 <- m_mean - 3*m_sd
# count number of ouliers
sum(versicolor$Versi_SLength.Mahl > crit1 | versicolor$Versi_SLength.Mahl < crit2) # no outliers
# Sepal Width
m_mean <- mean(versicolor$Versi_SWidth.Mahl)
m_sd <- sd(versicolor$Versi_SWidth.Mahl)
crit1 <- m_mean + 3*m_sd
crit2 <- m_mean - 3*m_sd
# count number of ouliers
sum(versicolor$Versi_SWidth.Mahl > crit1 | versicolor$Versi_SWidth.Mahl < crit2)
# 1 outlier! find location
index <- c(versicolor$Versi_SWidth.Mahl > crit1 | versicolor$Versi_SWidth.Mahl < crit2)
# add to flag
versicolor$flag[index] = versicolor$flag[index]+1
# See if anything is flagged more than once
sum(versicolor$flag>1) # everything flagged only once

# Since everything flagged only once, check if anything outside 4 SD and remove it anyway
# flag outside 3 SD of the mean
crit1 <- mean(setosa$Set_PLength.Mahl) + 4*sd(setosa$Set_PLength.Mahl)
crit2 <- mean(setosa$Set_PLength.Mahl) - 4*sd(setosa$Set_PLength.Mahl)
# count number of ouliers
sum(setosa$Set_PLength.Mahl > crit1 | setosa$Set_PLength.Mahl < crit2)
# 0 outliers
# Petal Width
crit1 <- mean(setosa$Set_PWidth.Mahl) + 4*sd(setosa$Set_PWidth.Mahl)
crit2 <- mean(setosa$Set_PWidth.Mahl) - 4*sd(setosa$Set_PWidth.Mahl)
# count number of ouliers
sum(setosa$Set_PWidth.Mahl > crit1 | setosa$Set_PWidth.Mahl < crit2)
# 1 outlier! find location
index <- c(setosa$Set_PWidth.Mahl > crit1 | setosa$Set_PWidth.Mahl < crit2)
# remove it
setosa <- setosa[!index,]
# Sepal Length
crit1 <- mean(setosa$Set_SLength.Mahl) + 4*sd(setosa$Set_SLength.Mahl)
crit2 <- mean(setosa$Set_SLength.Mahl) - 4*sd(setosa$Set_SLength.Mahl)
# count number of ouliers
sum(setosa$Set_SLength.Mahl > crit1 | setosa$Set_SLength.Mahl < crit2)
# Sepal Width
crit1 <- mean(setosa$Set_SWidth.Mahl) + 4*sd(setosa$Set_SWidth.Mahl)
crit2 <- mean(setosa$Set_SWidth.Mahl) - 4*sd(setosa$Set_SWidth.Mahl)
# count number of ouliers
sum(setosa$Set_SWidth.Mahl > crit1 | setosa$Set_SWidth.Mahl < crit2)
# 1 outlier! find location
index <- c(setosa$Set_SWidth.Mahl > crit1 | setosa$Set_SWidth.Mahl < crit2)
# remove it
setosa <- setosa[!index,]

# Virginica
m_mean <- mean(virginica$Virgi_PLength.Mahl)
m_sd <- sd(virginica$Virgi_PLength.Mahl)
crit1 <- m_mean + 4*m_sd
crit2 <- m_mean - 4*m_sd
# count number of ouliers
sum(virginica$Virgi_PLength.Mahl > crit1 | virginica$Virgi_PLength.Mahl < crit2) # 0 outliers
# Petal Width
m_mean <- mean(virginica$Virgi_PWidth.Mahl)
m_sd <- sd(virginica$Virgi_PWidth.Mahl)
crit1 <- m_mean + 4*m_sd
crit2 <- m_mean - 4*m_sd
# count number of ouliers
sum(virginica$Virgi_PWidth.Mahl > crit1 | virginica$Virgi_PWidth.Mahl < crit2) # 0
# Sepal Length
m_mean <- mean(virginica$Virgi_SLength.Mahl)
m_sd <- sd(virginica$Virgi_SLength.Mahl)
crit1 <- m_mean + 4*m_sd
crit2 <- m_mean - 4*m_sd
# count number of ouliers
sum(virginica$Virgi_SLength.Mahl > crit1 | virginica$Virgi_SLength.Mahl < crit2)
# 1 outlier! find location
index <- c(virginica$Virgi_SLength.Mahl > crit1 | virginica$Virgi_SLength.Mahl < crit2)
# remove it
virginica <- virginica[!index,]
# Sepal Width
m_mean <- mean(virginica$Virgi_SWidth.Mahl)
m_sd <- sd(virginica$Virgi_SWidth.Mahl)
crit1 <- m_mean + 4*m_sd
crit2 <- m_mean - 4*m_sd
# count number of ouliers
sum(virginica$Virgi_SWidth.Mahl > crit1 | virginica$Virgi_SWidth.Mahl < crit2)

#Versicolor
m_mean <- mean(versicolor$Versi_PLength.Mahl)
m_sd <- sd(versicolor$Versi_PLength.Mahl)
crit1 <- m_mean + 4*m_sd
crit2 <- m_mean - 4*m_sd
# count number of ouliers
sum(versicolor$Versi_PLength.Mahl > crit1 | versicolor$Versi_PLength.Mahl < crit2) # 1 outlier!
index <- c(versicolor$Versi_PLength.Mahl > crit1 | versicolor$Versi_PLength.Mahl < crit2)
versicolor <- versicolor[!index,]
# Petal Width
m_mean <- mean(versicolor$Versi_PWidth.Mahl)
m_sd <- sd(versicolor$Versi_PWidth.Mahl)
crit1 <- m_mean + 4*m_sd
crit2 <- m_mean - 4*m_sd
# count number of ouliers
sum(versicolor$Versi_PWidth.Mahl > crit1 | versicolor$Versi_PWidth.Mahl < crit2) # 1 outlier
# Sepal Length
m_mean <- mean(versicolor$Versi_SLength.Mahl)
m_sd <- sd(versicolor$Versi_SLength.Mahl)
crit1 <- m_mean + 4*m_sd
crit2 <- m_mean - 4*m_sd
# count number of ouliers
sum(versicolor$Versi_SLength.Mahl > crit1 | versicolor$Versi_SLength.Mahl < crit2) # no outliers
# Sepal Width
m_mean <- mean(versicolor$Versi_SWidth.Mahl)
m_sd <- sd(versicolor$Versi_SWidth.Mahl)
crit1 <- m_mean + 4*m_sd
crit2 <- m_mean - 4*m_sd
# count number of ouliers
sum(versicolor$Versi_SWidth.Mahl > crit1 | versicolor$Versi_SWidth.Mahl < crit2)
# 1 outlier! find location
index <- c(versicolor$Versi_SWidth.Mahl > crit1 | versicolor$Versi_SWidth.Mahl < crit2)
versicolor <- versicolor[!index,]

# put back into a dataframe
str(versicolor)
Sepal_Length <- c(setosa$Setosa_SepLength, virginica$Virgi_SepLength, versicolor$Versi_SepLength)
Sepal_Width <- c(setosa$Setosa_SepWidth, virginica$Virgi_SepWidth, versicolor$Versi_SepWidth)
Petal_Length <- c(setosa$Setosa_PetLength, virginica$Virgi_PetLength, versicolor$Versi_PetLength)
Petal_Width <- c(setosa$Setosa_PetWidth, virginica$Virgi_PetWidth, versicolor$Versi_PetWidth)
Species <- c(rep("setosa", length(setosa[,1])), rep("virginica", length(virginica[,1])), 
                 rep("versicolor", length(versicolor[,1])))
no_out_iris <- cbind(Sepal_Length, Sepal_Width, Petal_Length, Petal_Width, Species)
head(no_out_iris)
library(dplyr)
no_out_iris <- as_tibble(no_out_iris)
no_out_iris[,1:4] <- no_out_iris[,1:4] %>% mutate_if(is.character,as.numeric)
no_out_iris <- no_out_iris %>% mutate_if(is.character,as.factor)
str(no_out_iris)

# get new statistics
# save SD and Mean to watch changes after removals
new_setosa_stats <- summaryStats(setosa)
new_versicolor_stats <- summaryStats(versicolor)
new_virginica_stats <- summaryStats(virginica)
setosa_stats <- cbind(orig_setosa_stats[,1:3], new_setosa_stats[,1:3])
versicolor_stats <- cbind(orig_versicolor_stats[,1:3], new_versicolor_stats[,1:3])
virginica_stats <- cbind(orig_virginica_stats[,1:3], new_virginica_stats[,1:3])
# rm(list = c("new_setosa_stats", "new_versicolor_stats", "new_virginica_stats"))
(change <- setosa_stats[,5]-setosa_stats[,2])
setosa_stats <- cbind(setosa_stats, mean_change = change)
(change <- setosa_stats[,6]-setosa_stats[,3])
setosa_stats <- cbind(setosa_stats, sd_change = change)
change <- virginica_stats[,5] - virginica_stats[,2]
virginica_stats <- cbind(virginica_stats, mean_change <- change)
change <- virginica_stats[,6] - virginica_stats[,3]
virginica_stats <- cbind(virginica_stats, sd_change <- change)
change <- versicolor_stats[,5] - versicolor_stats[,2]
versicolor_stats <- cbind(versicolor_stats, mean_change = change)
change <- versicolor_stats[,6] - versicolor_stats[,3]
versicolor_stats <- cbind(versicolor_stats, sd_change = change)
out_removal_changes <- rbind(setosa_stats, virginica_stats, versicolor_stats)
out_removal_changes <- cbind(removed = (out_removal_changes[,1] - out_removal_changes[,4]), out_removal_changes[,7:8])
out_removal_changes <- as.data.frame(out_removal_changes)


library(formattable)
pdf("output/outlier_removal_summary_changes.pdf", 
    width = 8, height = 8, #set aspect ratio on paper (inches) 
    colormodel = "cmyk", 
    bg = "white", 
    paper = "us")
formattable(out_removal_changes, list(mean_change = 
                                          formatter("span", style = ~style(color = ifelse(mean_change>.01, "red", NA))),
                                      sd_change = 
                                          formatter("span", style = ~style(color = ifelse(sd_change>.01, "red", NA)))))
dev.off()

# Plot before and after
pdf("output/outlier_ellipsoids.pdf", 
    width = 8, height = 8, #set aspect ratio on paper (inches) 
    colormodel = "cmyk", 
    bg = "white", 
    paper = "us")
dataEllipse(y = iris$Petal.Length, 
            x = iris$Sepal.Length,
            levels = c(.5,.75,.95), 
            robust = TRUE,
            groups = iris$Species, 
            xlab = "Sepal Length",
            ylab = "Petal Length",
            main = "Original Data Outliers")
dataEllipse(y = no_out_iris$Petal_Length, 
            x = no_out_iris$Sepal_Length,
            levels = c(.5,.75,.95), 
            robust = TRUE,
            groups = no_out_iris$Species, 
            xlab = "Sepal Length",
            ylab = "Petal Length",
            main = "Outliers Removed")
dev.off()  

# Explore the other possibilites as well
dataEllipse(y = iris$Petal.Length, 
            x = iris$Sepal.Width,
            levels = c(.5,.75,.95), 
            robust = TRUE,
            groups = iris$Species, 
            xlab = "Sepal Width",
            ylab = "Petal Length",
            main = "Original Data Outliers")
dataEllipse(y = iris$Petal.Length, 
            x = iris$Petal.Width,
            levels = c(.5,.75,.95), 
            robust = TRUE,
            groups = iris$Species, 
            xlab = "Petal Width",
            ylab = "Petal Length",
            main = "Original Data Outliers")
dataEllipse(y = iris$Petal.Width, 
            x = iris$Sepal.Width,
            levels = c(.5,.75,.95), 
            robust = TRUE,
            groups = iris$Species, 
            xlab = "Sepal Width",
            ylab = "Petal Width",
            main = "Original Data Outliers")
dataEllipse(y = iris$Sepal.Length, 
            x = iris$Sepal.Width,
            levels = c(.5,.75,.95), 
            robust = TRUE,
            groups = iris$Species, 
            xlab = "Sepal Width",
            ylab = "Sepal Length",
            main = "Original Data Outliers")
dataEllipse(y = iris$Petal.Width, 
            x = iris$Sepal.Length,
            levels = c(.5,.75,.95), 
            robust = TRUE,
            groups = iris$Species, 
            xlab = "Sepal Length",
            ylab = "Petal Width",
            main = "Original Data Outliers")
