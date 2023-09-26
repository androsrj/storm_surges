# SOURCES
source("../mcmc_functions/mcmc.R") # Metropolis-Gibbs Sampler
source("../mcmc_functions/priors.R")
source("../mcmc_functions/jacobians.R")
source("../mcmc_functions/likelihood.R")
source("../mcmc_functions/posterior.R")
source("../other_functions/helper_functions.R") # Other misc functions (not part of MCMC)
source("../other_functions/parallel_functions.R")

# Libraries
library(parallel) # For parallel computation
library(doParallel) # For parallel computation
library(foreach) # For parallel computation
library(fields) # Distance matrix calculation
library(mvtnorm)

# Number of clusters for parallel implementation
nCores <- 10
mySeed <- 9999

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
  
mVals <- c(1, 2, 5, 10, 25, 50, 100, 200, 300, 400, 500)
mPropVals <- mVals / n
thetaVals <- seq(1, 5, length = nCores)
model <- "full_gp"
test_subjects <- 1:10
propSD <- c(0.05, 0.25)
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

  accVals <- lapply(obj, \(x) x$acceptance)
  acc[i, ] <- Reduce("+", accVals) / length(accVals)
  
  for (j in test_subjects) {
    predsList <- lapply(obj, \(x) x$preds[[j]])
    predictions <- Reduce("+", predsList) / length(predsList)
    MSPE[i] <- MSPE[i] + mean((predictions[2, ] - test$Y[[j]])^2)
  }
  MSPE[i] <- MSPE[i] / length(test_subjects)
}

saveRDS(list(mVals = mVals, MSPE = MSPE), "results/mspe.RDS")
mVals
MSPE
acc
