# Clear environment and free unused memory
rm(list = ls())
gc()

# SOURCES
source("../mcmc_functions/mcmc.R") # Metropolis-Gibbs Sampler
source("../mcmc_functions/priors.R")
source("../mcmc_functions/jacobians.R")
source("../mcmc_functions/likelihood.R")
source("../mcmc_functions/posterior.R")
source("../other_functions/sparse.R") # For sparse GP
source("../other_functions/helper_functions.R") # Other misc functions (not part of MCMC)

# Libraries
library(parallel) # For parallel computation
library(doParallel) # For parallel computation
library(foreach) # For parallel computation
library(fields) # Distance matrix calculation
library(mvtnorm)

# Read in
load("data/flood_data.RData")

# Clusters and seed
nCores <- 2
totalCores <- 10
mySeed <- 123

# Randomly sample indices for train and test data
nObs <- nrow(coords)
pctTrain <- 0.95
n <- nTrain <- floor(pctTrain * nObs)
nTest <- ceiling((1 - pctTrain) * nObs)
set.seed(mySeed)
indexTrain <- readRDS("results/data_split.RDS")[[1]]
indexTest <- readRDS("results/data_split.RDS")[[2]]

# Divide using train and test indices
storms <- 1:50
Y <- lapply(storms, \(i) out[i, indexTrain])
X <- lapply(storms, \(i) {
  Xintercept <- rep(1, nTrain)
  Xstorm <- matrix(rep(unlist(inputs[i, ]), nTrain), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTrain]
  X <- cbind(Xintercept, Xstorm, Xelev)
  colnames(X) <- c("int", colnames(inputs), "elev")
  return(X)
})
S <- as.matrix(coords[indexTrain, 1:2])
D <- rdist(S)

YTest <- lapply(storms, \(i) out[i, indexTest])
XTest <- lapply(storms, \(i) {
  Xintercept <- rep(1, nTest)
  Xstorm <- matrix(rep(unlist(inputs[i, ]), nTest), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTest]
  X <- cbind(Xintercept, Xstorm, Xelev)
  colnames(X) <- c("int", colnames(inputs), "elev")
  return(X)
})
STest <- as.matrix(coords[indexTest, 1:2])
DTest <- rdist(STest)

# Helper function to run subsets in parallel for sketching
sketching_parallel <- function(i) {
  results <- mcmc(X = X, Y = Y, D = D, S = S,
                  nSubj = nSubj,
                  theta = thetaVals[i],
                  propSD = c(0.02, 0.02),
                  nIter = 2000, nBurn = 500,
                  model = model,
                  mProp = mProp,
                  transform = TRUE)
  results
}

mVals <- c(5, 10, 25, 50, 100)
mPropVals <- mVals / n
nSubj <- length(X)
thetaVals <- seq(10, 100, length = totalCores)
model <- "full_gp"
test_subj <- 1
MSPE <- numeric(length(mVals))
acc <- matrix(0, nrow = length(mVals), ncol = 2)

for (i in 1:length(mVals)) {
  mProp <- mPropVals[[i]]
  
  cl <- makeCluster(nCores)
  registerDoParallel(cl)
  strt <- Sys.time()
  set.seed(mySeed)
  obj <- foreach(i = 1:nCores, .packages = "mvtnorm") %dopar% sketching_parallel(i)  
  final.time <- Sys.time() - strt 
  stopCluster(cl)
  
  predsList <- lapply(obj, \(x) x$preds)
  predictions <- Reduce("+", predsList) / length(predsList)
  MSPE[i] <- mean((predictions[2, ] - YTest[[test_subj]])^2)
  accVals <- lapply(obj, \(x) x$acceptance)
  acc[i, ] <- Reduce("+", accVals) / length(accVals)
}

saveRDS(list(mVals = mVals, MSPE = MSPE), "results/mspe.RDS")
mVals
MSPE
