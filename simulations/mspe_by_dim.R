# SOURCES
source("../mcmc_functions/mcmc.R") # Metropolis-Gibbs Sampler
source("../mcmc_functions/priors.R")
source("../mcmc_functions/jacobians.R")
source("../mcmc_functions/likelihood.R")
source("../mcmc_functions/posterior.R")
source("../other_functions/sparse.R") # For sparse GP
source("../other_functions/helper_functions.R") # Other misc functions (not part of MCMC)

# Libraries
library(anticlust) # for balanced clustering
library(splitTools) # for stratified splitting
library(twinning) # for multiplet splitting
library(parallel) # For parallel computation
library(doParallel) # For parallel computation
library(foreach) # For parallel computation
library(fields) # Distance matrix calculation
library(mvtnorm)
library(pracma) # For sparse matrix calculation

# Number of clusters for parallel implementation
nCores <- 10
mySeed <- 1234

# Load train and test data
load("data/train.RData")
load("data/test.RData")
nSubj <- length(train$X)
n <- nrow(train$X[[1]])
nTest <- nrow(test$X[[1]])
X <- train$X
Y <- train$Y
S <- train$S
D <- train$D
XTest <- test$X
YTest <- test$Y
STest <- test$S
DTest <- test$D

# Helper function to run subsets in parallel for sketching
sketching_parallel <- function(i) {
  results <- mcmc(X = X, Y = Y, D = D, S = S,
                  nSubj = nSubj,
                  theta = thetaVals[i],
                  propSD = c(0.04, 0.3),
                  nIter = 2000, nBurn = 500,
                  model = model,
                  mProp = mProp,
                  transform = TRUE)
  results
}

mVals <- seq(10, 100, by = 10)
mPropVals <- mVals / n
thetaVals <- seq(1, 5, length = nCores)
model <- "full_gp"
test_subj <- nSubj
MSPE <- numeric(length(mVals))

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
  MSPE[i] <- mean((predictions[2, ] - test$Y[[test_subj]])^2)
}

saveRDS(list(mVals = mVals, MSPE = MSPE), "results/mspe.RDS")
mVals
MSPE
