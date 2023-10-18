rm(list = ls())
gc()

# Libraries
library(parallel) # For parallel computation
library(doParallel) # For parallel computation
library(foreach) # For parallel computation
library(BayesTree) # For BART
library(spNNGP) # For NNGP

# Read in
load("data/flood_data.RData")

# Clusters and seed
nCores <- 10
mySeed <- 123

# Read in indices for train and test data
n <- nrow(coords)
#indices <- readRDS("results/data_split.RDS")
#indexTrain <- indices[[1]]
indexTest <- readRDS("results/test_points.RDS")
#nTrain <- length(indexTrain)
nTest <- length(indexTest)
storms <- 1:5
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
  Xstorm <- matrix(rep(unlist(inputs[i, ]), n), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTest]
  X <- cbind(Xstorm, Xelev)
  colnames(X) <- c(colnames(inputs), "elev")
  return(X)
})
STest <- coords[indexTest, 1:2]

## BART
cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
set.seed(mySeed)
results <- foreach(i = storms, .packages = "BayesTree") %dopar% bart(X[[i]], Y[[i]], XTest[[i]])
final.time <- Sys.time() - strt
stopCluster(cl)
if (file.exists(".RData")) {
  file.remove(".RData")
}
gc()

saveRDS(results, "results/flood_results_bart.RDS")

## NNGP
nIter <- 2000
cov.model <- "exponential"
starting <- list("phi"=5, "sigma.sq"=1, "tau.sq"=0.2)
tuning <- list("phi"=0.1, "sigma.sq"=5, "tau.sq"=2)
priors <- list("phi.Unif"=c(1,20), "sigma.sq.IG"=c(1, 1), "tau.sq.IG"=c(1, 1))

# Remove global attributes from X train and test
X <- lapply(storms, \(i) coords$elev_meters)
XTest <- lapply(stormsTest, \(i) coords$elev_meters)

cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
set.seed(mySeed)
nngp_obj <- foreach(i = storms, .packages = "spNNGP") %dopar% spNNGP(Y[[i]] ~ X[[i]], coords=S, 
                                                                     starting=starting, method="latent", 
                                                                     n.neighbors=10, tuning=tuning, 
                                                                     priors=priors, cov.model=cov.model,
                                                                     n.samples=nIter, n.omp.threads=1, fit.rep=TRUE)
gc()

#nngp_preds <- foreach(i = stormsTest, .packages="spNNGP") %dopar% predict(nngp_obj[[i - 5]], 
#                                                                      matrix(c(rep(1, n), XTest[[i - 5]]), ncol = 2), 
#                                                                      as.matrix(S))

final.time <- Sys.time() - strt
stopCluster(cl)
if (file.exists(".RData")) {
  file.remove(".RData")
}
gc()

#saveRDS(list(obj = nngp_obj, preds = nngp_preds), "results/flood_results_nngp.RDS")
saveRDS(nngp_obj, "results/flood_results_nngp.RDS")
