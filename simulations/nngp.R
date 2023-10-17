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
test_subjects <- 1:5

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


## NNGP
nSubj <- 10
storms <- 1:nSubj
nIter <- 2500
cov.model <- "exponential"
starting <- list("phi"=5, "sigma.sq"=1, "tau.sq"=0.2)
tuning <- list("phi"=0.1, "sigma.sq"=5, "tau.sq"=2)
priors <- list("phi.Unif"=c(1,20), "sigma.sq.IG"=c(1, 1), "tau.sq.IG"=c(1, 1))

# Remove global attributes from X train and test
X <- lapply(storms, \(i) train$X[[i]][,2:3])
XTest <- lapply(storms, \(i) test$X[[i]][,2:3])

cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
set.seed(mySeed)
nngp_obj <- foreach(i = storms, .packages = "spNNGP") %dopar% spNNGP(Y[[i]] ~ X[[i]], coords=S,
                                                                     starting=starting, method="latent",
                                                                     n.neighbors=10, tuning=tuning,
                                                                     priors=priors, cov.model=cov.model,
                                                                     n.samples=nIter, n.omp.threads=1)
gc()
nngp_preds <- foreach(i = storms, .packages="spNNGP") %dopar% predict(nngp_obj[[i]],
                                                                      matrix(c(rep(1, nTest), XTest[[i]]), ncol = 3),
                                                                      as.matrix(STest))

final.time <- Sys.time() - strt
stopCluster(cl)
if (file.exists(".RData")) {
  file.remove(".RData")
}
gc()

saveRDS(list(obj= nngp_obj, preds=nngp_preds), "temp_nngp.RDS")

preds <- lapply(1:10, \(i) apply(nngp_preds[[i]]$p.y.0, 1, mean))
mspe <- sapply(1:10, \(i) mean((preds[[i]] - test$Y[[i]])^2) )
mspe
mean(mspe)

