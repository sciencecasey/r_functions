library(dplyr)
library(lattice)
library(latticeExtra)
# 1.
# Implement an algorithm to read in the iris dataset
visIris = iris #import the dataset into R

# Data Exploration
str(visIris) 
typeof(visIris) #contains a list -- mixed dataset
anyNA(visIris$Sepal.Length || visIris$Sepal.Width || 
        visIris$Petal.Length || visIris$Petal.Width || 
        visIris$Species) #check for missing values 

# 2.
# Implement an algorithm to visualize two aspects of the dataset
# open pdf graphics creator
pdf("iris_features_CRichards.pdf", 
    width = 8, height = 8, #set aspect ratio on paper (inches) 
    colormodel = "cmyk", 
    bg = "white", 
    paper = "us")
xyplot(data = visIris, 
       visIris$Petal.Length~visIris$Sepal.Width, 
       type = 'p', 
       groups = Species, 
       scales = list(alternating = FALSE),
       par.settings=ggplot2like(),
       axis=axis.grid,
       auto.key = list(title = "Species", space = "right"), 
       main = "Sepal Width to Petal Length",
       xlab = "Sepal Width",
       ylab = "Petal Length",
       aspect = 1)
xyplot(data = visIris,
       visIris$Petal.Length~visIris$Sepal.Length,
       type = 'p',
       groups = Species,
       scales = list(alternating = FALSE),
       par.settings = ggplot2like(),
       axis = axis.grid,
       auto.key = list(title = "Species", space = "right"),
       main = "Petal Length to Sepal Length",
       ylab = "Petal Length",
       xlab = "Sepal Length",
       aspect = 1
       )
xyplot(data = visIris,
       visIris$Petal.Length~visIris$Petal.Width,
       type = 'p',
       groups = Species,
       scales = list(alternating = FALSE),
       par.settings = ggplot2like(),
       axis = axis.grid,
       auto.key = list(title = "Species", space = "right"),
       main = "Petal Length to Petal Width",
       ylab = "Petal Length",
       xlab = "Petal Width",
       aspect = 1
)
dev.off()

# 3
# sort the features in dataset
# use built in function. 
# If method is unspecified and input is less than 200, default uses 
# insertion sort (source: Rdocs). 
# create the a vector of species in order by petal length
# c.
temp = visIris[order(visIris$Petal.Length),]


# examining if any of these separated plant types
# d.
sum(temp$Species[1:50]!="setosa") +
    sum(temp$Species[51:100] != "versicolor") +
    sum(temp$Species[101:150] != "virginica") 
# 6 total overlap - 

# inspect where the overlaps occcured 
sum(temp$Species[1:50]!="setosa") # no overlap first 50
sum(temp$Species[51:100] != "versicolor") 
# 3 overlapping - print what they are
temp = temp$Species[51:100]
n = c(temp!="versicolor")
temp[n] # all three virginica
temp = visIris[order(visIris$Petal.Length),]
sum(temp$Species[101:150] != "virginica") # 3 overlap
# print the overlap (should be versicolor)
temp = temp$Species[101:150]
n = c(temp!="virginica")
temp[n] # all three versicolor
# Petal Length had 6 total overlapping classes

# Petal Width
temp = visIris[order(visIris$Petal.Width),]
sum(temp$Species[1:50]!="setosa") +
    sum(temp$Species[51:100] != "versicolor") +
    sum(temp$Species[101:150] != "virginica") 
# 6 total overlap

# Sepal Length
temp = visIris[order(visIris$Sepal.Length),]
sum(temp$Species[1:50]!="setosa") +
    sum(temp$Species[51:100] != "versicolor") +
    sum(temp$Species[101:150] != "virginica") 
# 37 total
temp = visIris[order(visIris$Sepal.Length),]
temp = temp$Species[1:50]
n = c(temp!="setosa")
temp[n] # both other classes represented, 6 total
temp = visIris[order(visIris$Sepal.Length),]
temp = temp$Species[51:100]
n = c(temp!="versicolor")
temp[n] # 6 setosa, 12 virginica 
temp = visIris[order(visIris$Sepal.Length),]
sum(temp$Species[101:150] != "virginica") 
# print the overlap 
temp = temp$Species[101:150]
n = c(temp!="virginica")
temp[n] # 13 total
# 37 overlap!

# Sepal Width
temp = visIris[order(visIris$Sepal.Width),]
sum(temp$Species[1:50]!="setosa") +
    sum(temp$Species[51:100] != "versicolor") +
    sum(temp$Species[101:150] != "virginica") 
# 118

# Petal length and width are best at organizing data
# see if we can do better if we use both with dplyr
# arrange across code modified from tidyverse docs: 
# https://dplyr.tidyverse.org/reference/arrange.html
temp = iris %>% arrange(across(starts_with("Petal")))
sum(temp$Species[1:50]!="setosa") +
    sum(temp$Species[51:100] != "versicolor") +
    sum(temp$Species[101:150] != "virginica") 
# same amount: 6

visIris[order(visIris$Sepal.Length),] 
visIris[order(visIris$Sepal.Width),]
visIris[order(visIris$Petal.Width),]


# next script is called mahalanobis_iris.R
visIris = iris
summaryStats(visIris[,1:4])
fullSumSetosa <- summaryFull(visIris[1:50,1:4])
fullSumVersicolor <- summaryFull(visIris[51:100,1:4])
fullSumVirginica <- summaryFull(visIris[101:150, 1:4])
str(fullSumSetosa[,1])


combinedSum <- cbind(Set_PetalLength = fullSumSetosa[,1], Versi_PetalLength= fullSumVersicolor[,1], Ver_PetalLength = fullSumVirginica[,1])
# combinedSum <- cbind(combinedSum, Mean_PL = rowMeans(combinedSum[,1:3]), Var_PL = diag(var(x = combinedSum[])))
combinedSum <- cbind(combinedSum, Set_PetalWidth = fullSumSetosa[,2], Versi_PetalWidth= fullSumVersicolor[,2], Ver_PetalWidth = fullSumVirginica[,2])
combinedSum <- cbind(combinedSum, Set_SepalLength = fullSumSetosa[,3], Versi_SepalLength= fullSumVersicolor[,3], Ver_SepalLength = fullSumVirginica[,3])
combinedSum <- cbind(combinedSum, Set_SepalWidth = fullSumSetosa[,4], Versi_SepalWidth= fullSumVersicolor[,4], Ver_SepalWidth = fullSumVirginica[,4])
