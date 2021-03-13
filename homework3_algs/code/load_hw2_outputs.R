# Load the datasets created from PCA Hw 2
diagPCA <- readRDS("../hw2_DataAnalysis/output/diagonalKeptEigsPCA.rds")
vertPCA <- readRDS("../hw2_DataAnalysis/output/verticalKeptEigsPCA.rds")
horizPCA <- readRDS("../hw2_DataAnalysis/output/horizontalKeptEigsPCA.rds")
anyDirPCA <- readRDS("../hw2_DataAnalysis/output/anyDirectionEigsPCA.rds")
 
# Load functions from that project
source("../functions/DCT_2.R")
source("../functions/pca_routine.R")
source("../functions/em.R")
