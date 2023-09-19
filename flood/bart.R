# Clear environment and free unused memory
rm(list = ls())
gc()

# Libraries
library(parallel) # For parallel computation
library(doParallel) # For parallel computation
library(foreach) # For parallel computation
library(BayesTree) # For BART

# Read in
load("data/flood_data.RData")

# Clusters and seed
nCores <- 10
mySeed <- 123

# Randomly sample indices for train and test data
nObs <- nrow(coords)
indices <- readRDS("results/data_split.RDS")
indexTrain <- indices[[1]]
indexTest <- indices[[2]]
nTrain <- n <- length(indexTrain)
nTest <- length(indexTest)

storms <- 1:5
Y <- lapply(storms, \(i) out[i, indexTrain])
X <- lapply(storms, \(i) {
  Xintercept <- rep(1, nTrain)
  Xstorm <- matrix(rep(unlist(inputs[i, ]), nTrain), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTrain]
  X <- cbind(Xintercept, Xstorm, Xelev)
  colnames(X) <- c("int", colnames(inputs), "elev")
  return(X)
})

YTest <- lapply(storms, \(i) out[i, indexTest])
XTest <- lapply(storms, \(i) {
  Xintercept <- rep(1, nTest)
  Xstorm <- matrix(rep(unlist(inputs[i, ]), nTest), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTest]
  X <- cbind(Xintercept, Xstorm, Xelev)
  colnames(X) <- c("int", colnames(inputs), "elev")
  return(X)
})

cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
set.seed(mySeed)
flood_results_bart <- foreach(i = 1:nCores, .packages = "BayesTree") %dopar% bart(X[[i]], Y[[i]], XTest[[i]])  
final.time <- Sys.time() - strt 
stopCluster(cl)
if (file.exists(".RData")) {
  file.remove(".RData")
}
gc()

saveRDS(flood_results_bart, paste0("results/flood_results_bart.RDS"))




