rm(list = ls())
gc()

# Libraries
library(BayesTree) # For BART

# Read in
load("data/flood_data.RData")

# Clusters and seed
nCores <- 5
mySeed <- 123

# Read in indices for test data
n <- nrow(coords)
indexTest <- readRDS("results/test_points.RDS")
nTest <- length(indexTest)
storms <- 1:5
nSubj <- length(storms)
stormsTest <- 6:10
nTestSubj <- length(stormsTest)

Y <- lapply(storms, \(i) out[i, ])
X <- lapply(storms, \(i) {
  Xstorm <- matrix(rep(unlist(inputs[i, ]), n), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters #[indexTrain]
  X <- cbind(Xstorm, Xelev)
  colnames(X) <- c(colnames(inputs), "elev")
  return(X)
})
S <- coords[ , 1:2]

YTest <- lapply(stormsTest, \(i) out[i, indexTest])
XTest <- lapply(stormsTest, \(i) {
  Xstorm <- matrix(rep(unlist(inputs[i, ]), nTest), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTest]
  X <- cbind(Xstorm, Xelev)
  colnames(X) <- c(colnames(inputs), "elev")
  return(X)
})
STest <- coords[indexTest, 1:2]

X <- do.call('rbind', X)
XTest <- do.call('rbind', XTest)
Y <- do.call('c', Y)
YTest <- do.call('c', YTest)

dim(X)
dim(XTest)
dim(Y)
dim(YTest)

##############
#### BART ####
##############

bart_obj <- bart(X, Y, XTest)
preds <- bart_obj$yhat.test.mean
mean((YTest - preds)^2)

length(YTest)
length(preds)

rm(list=ls())
gc()
