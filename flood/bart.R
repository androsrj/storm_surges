# Clear environment and free unused memory
rm(list = ls())
gc()

# SOURCES
source("../mcmc_functions/mcmc.R") # Metropolis-Gibbs Sampler
source("../mcmc_functions/priors.R")
source("../mcmc_functions/jacobians.R")
source("../mcmc_functions/likelihood.R")
source("../mcmc_functions/posterior.R")
source("../other_functions/parallel_functions.R") # Parallel wrapper functions
source("../other_functions/helper_functions.R") # Other misc functions (not part of MCMC)

# Libraries
library(BayesTree)
library(fields)

# Read in
load("data/flood_data.RData")

# Clusters and seed
nCores <- 2
totalCores <- 10
mySeed <- 123

# Randomly sample indices for train and test data
nObs <- nrow(coords)
indices <- readRDS("results/data_split.RDS")
indexTrain <- indices[[1]]
indexTest <- indices[[2]]
nTrain <- n <- length(indexTrain)
nTest <- length(indexTest)

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


# BART
nSubj <- length(storms)
test_subj <- 1
flood_results_bart <- bart(X[[1]], Y[[1]], XTest[[1]])

saveRDS(flood_results_bart, paste0("results/flood_results_bart.RDS"))

