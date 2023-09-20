# SOURCES
source("../mcmc_functions/mcmc.R") # Metropolis-Gibbs Sampler
source("../mcmc_functions/priors.R")
source("../mcmc_functions/jacobians.R")
source("../mcmc_functions/likelihood.R")
source("../mcmc_functions/posterior.R")
source("../other_functions/sparse.R") # For sparse GP
source("../other_functions/parallel_functions.R") # Parallel wrapper functions
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
#nCores <- detectCores() / 2
nCores <- 10
mySeed <- 1234
nKnots <- 500
propSD <- c(0.04, 0.3)

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

# Create knot data
locations <- runif(nKnots * 2, floor(min(S)), ceiling(max(S)))
SKnot <- matrix(locations, nrow = nKnots, ncol = 2)
DKnot <- rdist(SKnot)

#######################################################
########### RUN MCMC FOR DIVIDE-AND-CONQUER ###########
#######################################################
DC_results_full_gp <- vector("list", 4)
DC_results_sparse_gp <- vector("list", 4)
DC_results_mpp <- vector("list", 4)

# Divide indices for each split type (excluding knot data)
indices <- vector("list", 4)
indices[[1]] <- balanced_clustering(S, nCores) # subdomains
indices[[2]] <- list2Vec(partition(indices[[1]], p = rep(1/nCores, nCores))) # stratified
indices[[3]] <- multiplet(as.data.frame(cbind(X[[1]], Y[[1]])), k = nCores) # multiplets
indices[[4]] <- list2Vec(split(sample(1:n, n, replace = FALSE), as.factor(1:nCores))) # random
splits <- c("subdomains", "stratified", "multiplets", "random")

# Get results for all 4 types of splits, both full GP and MPP
for (j in 1:4) {
  
  # Create split indices for divide-and-conquer
  splitType <- splits[j]
  index <- indices[[j]]
  subsetsX <- lapply(1:nCores, function(k) { 
    lapply(1:nSubj, function(s) X[[s]][which(index == k), ])
  })
  subsetsY <- lapply(1:nCores, function(k) { 
    lapply(1:nSubj, function(s) Y[[s]][which(index == k)])
  })
  subsetsS <- lapply(1:nCores, function(k) S[which(index == k), ])
  subsetsD <- lapply(1:nCores, function(k) D[which(index == k), which(index == k)])
  
  #### FULL GAUSSIAN PROCESS ####
  model <- "full_gp"
  dir.path <- paste0("results/d_and_c/full_gp/", splitType)
  if (!dir.exists(dir.path)) {
    dir.create(dir.path)
  }
  
  # Parallel
  cl <- makeCluster(nCores)
  registerDoParallel(cl)
  strt <- Sys.time()
  set.seed(mySeed)
  obj <- foreach(i = 1:nCores, .packages = "mvtnorm") %dopar% DC_parallel(i)
  final.time <- Sys.time() - strt 
  stopCluster(cl)
  
  # Wasserstein averages of quantiles across subsets
  DC_results_full_gp[[j]] <- wasserstein(nChains = nCores, 
                                         method = "d_and_c",
                                         model = "full_gp",
                                         splitType = splitType,
                                         time = final.time)
  
  #### SPARSE GAUSSIAN PROCESS ####
  model <- "sparse_gp"
  dir.path <- paste0("results/d_and_c/sparse_gp/", splitType)
  if (!dir.exists(dir.path)) {
    dir.create(dir.path)
  }
  
  # Parallel
  cl <- makeCluster(nCores)
  registerDoParallel(cl)
  strt <- Sys.time()
  set.seed(mySeed)
  obj <- foreach(i = 1:nCores, .packages = c("mvtnorm", "pracma")) %dopar% DC_parallel(i)
  final.time <- Sys.time() - strt 
  stopCluster(cl)
  
  # Wasserstein averages of quantiles across subsets
  DC_results_sparse_gp[[j]] <- wasserstein(nChains = nCores, 
                                           method = "d_and_c",
                                           model = "sparse_gp",
                                           splitType = splitType,
                                           time = final.time)
  
  #### MODIFIED PREDICTIVE PROCESS ####
  model <- "mpp"
  dir.path <- paste0("results/d_and_c/mpp/", splitType)
  if (!dir.exists(dir.path)) {
    dir.create(dir.path)
  }
  
  #subsetsX <- lapply(1:nCores, function(k) { 
  #  lapply(1:nSubj, function(s) X[[s]][c(which(index == k), (nTrain + 1):nObs), ])
  #})
  #subsetsY <- lapply(1:nCores, function(k) { 
  #  lapply(1:nSubj, function(s) Y[[s]][c(which(index == k), (nTrain + 1):nObs)])
  #})
  #subsetsD <- lapply(1:nCores, function(k) D[c(which(index == k), (nTrain + 1):nObs), c(which(index == k), (nTrain + 1):nObs)])
  
  # Parallel
  cl <- makeCluster(nCores)
  registerDoParallel(cl)
  strt<-Sys.time()
  set.seed(mySeed)
  obj <- foreach(i = 1:nCores, .packages = c("mvtnorm", "fields")) %dopar% DC_parallel(i)
  final.time <- Sys.time() - strt 
  stopCluster(cl)
  
  # Wasserstein averages of quantiles across subsets
  DC_results_mpp[[j]] <- wasserstein(nChains = nCores, 
                                     method = "d_and_c", 
                                     model = "mpp",
                                     splitType = splitType,
				     time = final.time)
}

saveRDS(DC_results_full_gp, "results/d_and_c/full_gp/final_results.RDS")
saveRDS(DC_results_sparse_gp, "results/d_and_c/sparse_gp/final_results.RDS")
saveRDS(DC_results_mpp, "results/d_and_c/mpp/final_results.RDS")


#######################################################
############### RUN MCMC FOR SKETCHING ################
#######################################################

# Sequence of values for theta to iterate over
thetaVals <- seq(1, 5, length = nCores)
mProp <- 0.05

#### FULL GAUSSIAN PROCESS ####
model <- "full_gp"

# Parallel
cl <- makeCluster(nCores)
registerDoParallel(cl)
strt<-Sys.time()
set.seed(mySeed)
obj <- foreach(i = 1:nCores, .packages = "mvtnorm") %dopar% sketching_parallel(i)  
final.time <- Sys.time() - strt 
stopCluster(cl)

# Wasserstein averages of quantiles across reps
sketching_results_full_gp <- wasserstein(nChains = nCores, 
                                         method = "sketching", 
                                         model = "full_gp",
                                         splitType = NULL,
                                         time = final.time)
saveRDS(sketching_results_full_gp, "results/sketching/full_gp/final_results.RDS")

#### SPARSE GAUSSIAN PROCESS ####
model <- "sparse_gp"

# Parallel
cl <- makeCluster(nCores)
registerDoParallel(cl)
strt<-Sys.time()
set.seed(mySeed)
obj <- foreach(i = 1:nCores, .packages = c("mvtnorm", "pracma")) %dopar% sketching_parallel(i)  
final.time <- Sys.time() - strt 
stopCluster(cl)

# Wasserstein averages of quantiles across reps
sketching_results_sparse_gp <- wasserstein(nChains = nCores, 
                                           method = "sketching", 
                                           model = "sparse_gp",
                                           splitType = NULL,
                                           time = final.time)
saveRDS(sketching_results_sparse_gp, "results/sketching/sparse_gp/final_results.RDS")

#### MODIFIED PREDICTIVE PROCESS ####
model <- "mpp"

# Parallel
cl <- makeCluster(nCores)
registerDoParallel(cl)
strt<-Sys.time()
set.seed(mySeed)
obj <- foreach(i = 1:nCores, .packages = c("mvtnorm", "fields")) %dopar% sketching_parallel(i)  
final.time <- Sys.time() - strt 
stopCluster(cl)

# Wasserstein averages of quantiles across reps
sketching_results_mpp <- wasserstein(nChains = nCores, 
                                     method = "sketching", 
                                     model = "mpp",
                                     splitType = NULL,
                                     time = final.time)
saveRDS(sketching_results_mpp, "results/sketching/mpp/final_results.RDS")

