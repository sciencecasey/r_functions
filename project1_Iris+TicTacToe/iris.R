library(dplyr)
library(lattice)
library(latticeExtra)
#Implement an algorithm to read in the iris dataset
visIris = iris #import the dataset into R

#Data Exploration
str(visIris) 
typeof(visIris) #contains a list -- mixed dataset
anyNA(visIris$Sepal.Length || visIris$Sepal.Width || 
        visIris$Petal.Length || visIris$Petal.Width || 
        visIris$Species) #check for missing values 

#Implement an algorithm to visualize two aspects of the dataset
#open pdf graphics creator
pdf("iris_sepalWidthXpetalLegnth_CRichards.pdf", 
    width = 8, height = 8, #set aspect ratio on paper (inches) 
    colormodel = "cmyk", 
    bg = "white", 
    paper = "us")
xyplot(data = visIris, 
       visIris$Petal.Length~visIris$Sepal.Width, 
       type = 'l', 
       groups = Species, 
       scales = list(alternating = FALSE),
       par.settings=ggplot2like(),
       axis=axis.grid,
       auto.key = list(title = "Sort Type", space = "right"), 
       main = "Sepal Width to Petal Length, by Species",
       ylab = "Sepal Width",
       xlab = "Petal Length",
       aspect = 1)
dev.off()
