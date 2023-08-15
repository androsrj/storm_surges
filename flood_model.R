# Clear environment and free unused memory
rm(list = ls())
gc()

# SOURCES
source("mcmc_functions/flood_mcmc.R") # Metropolis-Gibbs Sampler
source("mcmc_functions/priors.R")
source("mcmc_functions/jacobians.R")
source("mcmc_functions/likelihood.R")
source("mcmc_functions/posterior.R")
source("other_functions/parallel_functions.R") # Parallel wrapper functions
source("other_functions/helper_functions.R") # Other misc functions (not part of MCMC)

# Libraries
library(parallel) # For parallel computation
library(doParallel) # For parallel computation
library(foreach) # For parallel computation
library(mvtnorm)
library(fields)
library(MBA)
library(anticlust) # for balanced clustering
library(splitTools) # for stratified splitting

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
indexTrain <- sort(sample(1:nObs, nTrain))
indexTest <- sort(setdiff(1:nObs, indexTrain))
saveRDS(list(indexTrain, indexTest), "results/data_split.RDS")

# Divide using train and test indices
storms <- 1:50
YTrain <- lapply(storms, \(i) out[i, indexTrain])
XTrain <- lapply(storms, \(i) {
  Xintercept <- rep(1, nTrain)
  Xstorm <- matrix(rep(unlist(inputs[i, ]), nTrain), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTrain]
  X <- cbind(Xintercept, Xstorm, Xelev)
  colnames(X) <- c("int", colnames(inputs), "elev")
  return(X)
})
STrain <- as.matrix(coords[indexTrain, 1:2])
DTrain <- rdist(STrain)

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


# Sketching
thetaVals <- seq(0.1, 0.5, length = totalCores)
cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
set.seed(mySeed)
obj <- foreach(i = 1:totalCores, .packages = "mvtnorm") %dopar% sketching_parallel(i)  
final.time <- Sys.time() - strt 
stopCluster(cl)
if (file.exists(".RData")) {
  file.remove(".RData")
}
gc()
flood_results_sketching <- wasserstein(nChains = totalCores, method = "sketching")
saveRDS(flood_results_sketching, paste0("results/flood_results_sketching.RDS"))



# D&C, subdomain split
indexSubd <- balanced_clustering(STrain, totalCores)
subsetsX <- lapply(1:totalCores, function(k) XTrain[which(indexSubd == k), ])
subsetsY <- lapply(1:totalCores, function(k) YTrain[which(indexSubd == k)])
subsetsD <- lapply(1:totalCores, function(k) DTrain[which(indexSubd == k), which(indexSubd == k)])

cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
set.seed(mySeed)
obj <- foreach(i = 1:totalCores, .packages = "mvtnorm") %dopar% subdomains_parallel(i)  
final.time <- Sys.time() - strt 
stopCluster(cl)
if (file.exists(".RData")) {
  file.remove(".RData")
}
gc()
flood_results_subdomains <- wasserstein(nChains = totalCores, method = "subdomains")
saveRDS(flood_results_subdomains, paste0("results/flood_results_subdomains.RDS"))



# D&C, stratified split
indexSubd <- balanced_clustering(STrain, totalCores)
indexStrat <- list2Vec(partition(indexSubd, p = rep(1/nCores, totalCores)))
subsetsX <- lapply(1:totalCores, function(k) XTrain[which(indexStrat == k), ])
subsetsY <- lapply(1:totalCores, function(k) YTrain[which(indexStrat == k)])
subsetsD <- lapply(1:totalCores, function(k) DTrain[which(indexStrat == k), which(indexStrat == k)])

cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
set.seed(mySeed)
obj <- foreach(i = 1:totalCores, .packages = "mvtnorm") %dopar% stratified_parallel(i)  
final.time <- Sys.time() - strt 
stopCluster(cl)
if (file.exists(".RData")) {
  file.remove(".RData")
}
gc()
flood_results_stratified <- wasserstein(nChains = totalCores, method = "stratified")
saveRDS(flood_results_stratified, paste0("results/flood_results_stratified.RDS"))


