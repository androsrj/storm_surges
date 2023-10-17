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
mySeed <- 1997
test_subjects <- 6:10

# Randomly sample indices for train and test data
n <- nrow(coords)
pctTest <- 0.01
nTest <- floor(pctTest * n)
set.seed(mySeed)
indexTest <- sort(sample(1:n, nTest))
saveRDS(indexTest, "results/test_points.RDS")

# Divide using train and test indices
storms <- 1:5
Y <- lapply(storms, \(i) out[i, ])
X <- lapply(storms, \(i) {
  Xintercept <- rep(1, n)
  Xstorm <- matrix(rep(unlist(inputs[i, ]), n), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters
  X <- cbind(Xintercept, Xstorm, Xelev)
  colnames(X) <- c("int", colnames(inputs), "elev")
  return(X)
})
S <- as.matrix(coords[ , 1:2])
D <- rdist(S)

YTest <- lapply(test_subjects, \(i) out[i, indexTest])
XTest <- lapply(test_subjects, \(i) {
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
thetaVals <- seq(10^5, 10^6, length = totalCores)
model <- "full_gp"
mProp <- 0.01
propSD <- c(0.01, 0.14)
nSubj <- length(storms)

cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
set.seed(mySeed)
obj <- foreach(i = storms, .packages = "mvtnorm") %dopar% sketching_parallel(i)  
final.time <- Sys.time() - strt 
stopCluster(cl)
if (file.exists(".RData")) {
  file.remove(".RData")
}
gc()
flood_results_sketching <- wasserstein(results = obj, time = final.time)
saveRDS(flood_results_sketching, paste0("results/flood_results_alt.RDS"))









# D&C, subdomain split
#indexSubd <- balanced_clustering(STrain, totalCores)
#subsetsX <- lapply(1:totalCores, function(k) { 
#  lapply(storms, function(s) XTrain[[s]][which(indexSubd == k), ])
#  })
#subsetsY <- lapply(1:totalCores, function(k) { 
#  lapply(storms, function(s) YTrain[[s]][which(indexSubd == k)])
#})
#subsetsD <- lapply(1:totalCores, function(k) DTrain[which(indexSubd == k), which(indexSubd == k)])
#
#cl <- makeCluster(nCores)
#registerDoParallel(cl)
#strt <- Sys.time()
#set.seed(mySeed)
#obj <- foreach(i = 1:totalCores, .packages = "mvtnorm") %dopar% subdomains_parallel(i)  
#final.time <- Sys.time() - strt 
#stopCluster(cl)
#if (file.exists(".RData")) {
#  file.remove(".RData")
#}
#gc()
#flood_results_subdomains <- wasserstein(nChains = totalCores, method = "subdomains")
#saveRDS(flood_results_subdomains, paste0("results/flood_results_subdomains.RDS"))



# D&C, stratified split
#indexStrat <- list2Vec(partition(indexSubd, p = rep(1/nCores, totalCores)))
#subsetsX <- lapply(1:totalCores, function(k) { 
#  lapply(storms, function(s) XTrain[[s]][which(indexStrat == k), ])
#})
#subsetsY <- lapply(1:totalCores, function(k) { 
#  lapply(storms, function(s) YTrain[[s]][which(indexStrat == k)])
#})
#subsetsD <- lapply(1:totalCores, function(k) DTrain[which(indexStrat == k), which(indexStrat == k)])
#
#cl <- makeCluster(nCores)
#registerDoParallel(cl)
#strt <- Sys.time()
#set.seed(mySeed)
#obj <- foreach(i = 1:totalCores, .packages = "mvtnorm") %dopar% stratified_parallel(i)  
#final.time <- Sys.time() - strt 
#stopCluster(cl)
#if (file.exists(".RData")) {
#  file.remove(".RData")
#}
#gc()
#flood_results_stratified <- wasserstein(nChains = totalCores, method = "stratified")
#saveRDS(flood_results_stratified, paste0("results/flood_results_stratified.RDS"))


