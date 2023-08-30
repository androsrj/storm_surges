# SOURCES
source("mcmc_functions/mcmc.R") # Metropolis-Gibbs Sampler
source("mcmc_functions/priors.R")
source("mcmc_functions/jacobians.R")
source("mcmc_functions/likelihood.R")
source("mcmc_functions/posterior.R")
source("other_functions/sparse.R") # For sparse GP
source("other_functions/parallel_functions.R") # Parallel wrapper functions
source("other_functions/helper_functions.R") # Other misc functions (not part of MCMC)

# Libraries
library(anticlust) # for balanced clustering
library(splitTools) # for stratified splitting
library(twinning) # for multiplet splitting
library(parallel) # For parallel computation
library(doParallel) # For parallel computation
library(foreach) # For parallel computation
library(mvtnorm)
library(pracma) # For sparse matrix calculation

# Number of clusters for parallel implementation
#nCores <- detectCores() / 2
nCores <- 10
mySeed <- 1234
nKnots <- 500

# Load train and test data
load("data/train.RData")
load("data/test.RData")
nObs <- nrow(train$X)
nTrain <- nObs - nKnots
n <- nTrain
nTest <- nrow(test$X)
X <- train$X
Y <- train$Y
S <- train$S
D <- train$D

#######################################################
########### RUN MCMC FOR DIVIDE-AND-CONQUER ###########
#######################################################
DC_results_full_gp <- vector("list", 4)
DC_results_sparse_gp <- vector("list", 4)
DC_results_mpp <- vector("list", 4)

# Divide indices for each split type (excluding knot data)
indices <- vector("list", 4)
indices[[1]] <- balanced_clustering(S[1:nTrain, ], nCores) # subdomains
indices[[2]] <- list2Vec(partition(indices[[1]], p = rep(1/nCores, nCores))) # stratified
indices[[3]] <- multiplet(as.data.frame(cbind(X, Y))[1:nTrain, ], k = nCores) # multiplets
indices[[4]] <- list2Vec(split(sample(1:nTrain, nTrain, replace = FALSE), as.factor(1:nCores))) # random
splits <- c("subdomains", "stratified", "multiplets", "random")

# Get results for all 4 types of splits, both full GP and MPP
for (j in 1:4) {
  splitType <- splits[j]
  
  #### FULL GAUSSIAN PROCESS ####
  model <- "full_gp"
  dir.path <- paste0("results/d_and_c/full_gp/", splitType)
  if (!dir.exists(dir.path)) {
    dir.create(dir.path)
  }
  
  # Create split indices for full/sparse GP (only first nTrain obs)
  index <- indices[[j]]
  subsetsX <- lapply(1:nCores, function(k) X[which(index == k), ])
  subsetsY <- lapply(1:nCores, function(k) Y[which(index == k)])
  subsetsD <- lapply(1:nCores, function(k) D[which(index == k), which(index == k)])
  
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
                                         model = "full_gp",
                                         splitType = splitType)
  
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
                                           model = "sparse_gp",
                                           splitType = splitType)
  
  #### MODIFIED PREDICTIVE PROCESS ####
  model <- "mpp"
  dir.path <- paste0("results/d_and_c/mpp/", splitType)
  if (!dir.exists(dir.path)) {
    dir.create(dir.path)
  }
  
  # Create split indices for MPP (includes knot data)
  index <- indices[[j]]
  subsetsX <- lapply(1:nCores, function(k) X[c(which(index == k), (nTrain + 1):nObs), ])
  subsetsY <- lapply(1:nCores, function(k) Y[c(which(index == k), (nTrain + 1):nObs)])
  subsetsD <- lapply(1:nCores, function(k) D[c(which(index == k), (nTrain + 1):nObs), c(which(index == k), (nTrain + 1):nObs)])
  
  # Parallel
  cl <- makeCluster(nCores)
  registerDoParallel(cl)
  strt<-Sys.time()
  set.seed(mySeed)
  obj <- foreach(i = 1:nCores, .packages = "mvtnorm") %dopar% DC_parallel(i)
  final.time <- Sys.time() - strt 
  stopCluster(cl)
  
  # Wasserstein averages of quantiles across subsets
  DC_results_mpp[[j]] <- wasserstein(nChains = nCores, 
                                     model = "mpp",
                                     splitType = splitType)
}

saveRDS(DC_results_full_gp, "results/d_and_c/full_gp/final_results.RDS")
saveRDS(DC_results_sparse_gp, "results/d_and_c/sparse_gp/final_results.RDS")
saveRDS(DC_results_mpp, "results/d_and_c/mpp/final_results.RDS")


#######################################################
############### RUN MCMC FOR SKETCHING ################
#######################################################

# Sequence of values for theta to iterate over
thetaVals <- seq(1, 5, length = nCores)

#### FULL GAUSSIAN PROCESS ####
model <- "full_gp"
X <- train$X[1:nTrain, ]
Y <- train$Y[1:nTrain]
S <- train$S[1:nTrain, ]
D <- train$D[1:nTrain, 1:nTrain]

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
                                         model = "full_gp",
                                         splitType = "sketching")
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
                                         model = "sparse_gp",
                                         splitType = "sketching")
saveRDS(sketching_results_sparse_gp, "results/sketching/sparse_gp/final_results.RDS")

#### MODIFIED PREDICTIVE PROCESS ####
model <- "mpp"
X <- train$X
Y <- train$Y
S <- train$S
D <- train$D

# Parallel
cl <- makeCluster(nCores)
registerDoParallel(cl)
strt<-Sys.time()
set.seed(mySeed)
obj <- foreach(i = 1:nCores, .packages = "mvtnorm") %dopar% sketching_parallel(i)  
final.time <- Sys.time() - strt 
stopCluster(cl)

# Wasserstein averages of quantiles across reps
sketching_results_mpp <- wasserstein(nChains = nCores, 
                                     model = "mpp",
                                     splitType = "sketching")
saveRDS(sketching_results_mpp, "results/sketching/mpp/final_results.RDS")

