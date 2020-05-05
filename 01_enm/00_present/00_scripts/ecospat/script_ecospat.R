#' ---
#' title: ecospat
#' author: mauricio vancine
#' date: 2019-04-13
#' ---

# packages
library(ecospat)
citation("ecospat")

# 1 Load data ---------------------------------------------------------
# 1.0.1 Test data for the ecospat library
data(ecospat.testData)
names(ecospat.testData)

# 1.0.2 Test data for the Niche Overlap Analysis
data(ecospat.testNiche.inv)
names(ecospat.testNiche.inv)

data(ecospat.testNiche.nat)
names(ecospat.testNiche.nat)

fpath <- system.file("extdata", "ecospat.testTree.tre", package="ecospat")
fpath

tree <- read.tree(fpath)
tree$tip.label
plot(tree, cex = .6)

# 2 Pre-Modelling Analysis ------------------------------------------------
# 2.1 Spatial Auto-correlation - Mantel Correlogram
ecospat.mantel.correlogram(dfvar = ecospat.testData[c(2:16)],
                           colxy = 1:2, 
                           n = 100,
                           colvar = 3:7, 
                           max = 1000, 
                           nclass = 10, 
                           nperm = 100)

# 2.2 Predictor Variable Selection
# Number of Predictors with Correlation
colvar <- ecospat.testData[c(4:8)]
colvar

x <- cor(colvar, method = "pearson")
x

ecospat.npred (x, th = 0.75)

x <- cor(colvar, method = "spearman")
x

ecospat.npred (x, th = 0.75)

# 2.3 Climate Analogy Tools
x <- ecospat.testData[c(4:8)]
x

p <- x[1:90,] #A projection dataset.
p

ref <- x[91:300, ] # A reference dataset

ecospat.climan(ref, p)
