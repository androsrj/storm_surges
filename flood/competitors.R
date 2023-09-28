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
nObs <- nrow(coords)
indices <- readRDS("results/data_split.RDS")
indexTrain <- indices[[1]]
indexTest <- indices[[2]]
nTrain <- n <- length(indexTrain)
nTest <- length(indexTest)

storms <- 1:10
Y <- lapply(storms, \(i) out[i, indexTrain])
X <- lapply(storms, \(i) {
  #Xintercept <- rep(1, nTrain)
  Xstorm <- matrix(rep(unlist(inputs[i, ]), nTrain), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTrain]
  X <- cbind(Xstorm, Xelev)
  colnames(X) <- c(colnames(inputs), "elev")
  return(X)
})
S <- coords[indexTrain, 1:2]

YTest <- lapply(storms, \(i) out[i, indexTest])
XTest <- lapply(storms, \(i) {
  #Xintercept <- rep(1, nTest)
  Xstorm <- matrix(rep(unlist(inputs[i, ]), nTest), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTest]
  X <- cbind(Xstorm, Xelev)
  colnames(X) <- c(colnames(inputs), "elev")
  return(X)
})
STest <- coords[indexTest, 1:2]

## BART
#cl <- makeCluster(nCores)
#registerDoParallel(cl)
#strt <- Sys.time()
#set.seed(mySeed)
#results <- foreach(i = storms, .packages = "BayesTree") %dopar% bart(X[[i]], Y[[i]], XTest[[i]])
#final.time <- Sys.time() - strt
#stopCluster(cl)
if (file.exists(".RData")) {
  file.remove(".RData")
}
gc()

#saveRDS(results, "results/flood_results_bart.RDS")

## NNGP
nIter <- 1000
cov.model <- "exponential"
starting <- list("phi"=20, "sigma.sq"=1, "tau.sq"=0.2)
tuning <- list("phi"=5, "sigma.sq"=5, "tau.sq"=2)
priors <- list("phi.Unif"=c(1,100), "sigma.sq.IG"=c(1, 1), "tau.sq.IG"=c(1, 1))

# Remove global attributes from X train and test
X <- lapply(storms, \(i) coords$elev_meters[indexTrain])
XTest <- lapply(storms, \(i) coords$elev_meters[indexTest])

cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
set.seed(mySeed)
nngp_obj <- foreach(i = storms, .packages = "spNNGP") %dopar% spNNGP(Y[[i]] ~ X[[i]], coords=S, 
                                                                     starting=starting, method="latent", 
                                                                     n.neighbors=10, tuning=tuning, 
                                                                     priors=priors, cov.model=cov.model,
                                                                     n.samples=nIter, n.omp.threads=1)
nngp_preds <- foreach(i = storms, .packages="spNNGP") %dopar% predict(nngp_obj[[i]], 
                                                                      matrix(c(rep(1, nTest), XTest[[i]]), ncol = 2), 
                                                                      as.matrix(STest))
final.time <- Sys.time() - strt
stopCluster(cl)
if (file.exists(".RData")) {
  file.remove(".RData")
}
gc()

saveRDS(list(obj = nngp_obj, preds = nngp_preds), "results/flood_results_nngp.RDS")
