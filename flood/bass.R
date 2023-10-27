
rm(list = ls())
gc()

# Libraries
library(parallel) # For parallel computation
library(doParallel) # For parallel computation
library(foreach) # For parallel computation
library(BayesTree) # For BART
library(spNNGP) # For NNGP
library(BASS) # For BASS

# Read in
load("data/flood_data.RData")

# Clusters and seed
nCores <- 20
mySeed <- 999

# Read in indices for test data
n <- nrow(coords)
indexTest <- readRDS("results/test_points.RDS")
nTest <- length(indexTest)
storms <- 1:5
nSubj <- length(storms)
stormsTest <- 6:10
nTestSubj <- length(stormsTest)


##############
#### BASS ####
##############

nStorms <- length(storms) + length(stormsTest)
nTestSubj <- length(stormsTest)
n <- nrow(coords)
inputs <- inputs[1:nStorms, ]
out <- out[1:nStorms, ]
gc()

bassParallel <- function(seed) {
  set.seed(seed)
  obj <- bassPCA(inputs[-stormsTest, ], out[-stormsTest, ], n.pc=3, n.cores=1)
  predictions <- predict(obj, inputs[stormsTest, ])[ , , indexTest]
  preds <- apply(predictions, 2:3, mean)
  quants <- apply(predictions, 2:3, quantile, c(0.025, 0.975))
  return(list(preds = preds, lower = quants[1, , ], upper = quants[2, , ]))
}

cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
nReps <- 100
seeds <- rep(1100, 10000, by = 100)
predictions <- foreach(i=1:nReps, .packages = "BASS") %dopar% bassParallel(seed = mySeed - i) 
stopCluster(cl)

bassPreds <- bassLower <- bassUpper <- matrix(0, nrow = nTestSubj, ncol = nTest)
for (j in 1:nReps) {
  bassPreds <- bassPreds + predictions[[j]]$preds
  bassLower <- bassLower + predictions[[j]]$lower
  bassUpper <- bassUpper + predictions[[j]]$upper
}

bassPreds <- bassPreds / nReps
bassUpper <- bassUpper / nReps
bassLower <- bassLower / nReps
bassTime <- Sys.time() - strt

mspe <- sapply(1:nTestSubj, \(i) mean((bassPreds[i, ] - out[stormsTest[i], indexTest])^2) )
print(mean(mspe))

pct <- sapply(1:nTestSubj, \(i) mean((bassPreds[i, ] > 4/3.28084) == (out[stormsTest[i], indexTest] > 4/3.28084)) )
print(1-mean(pct))

bass <- list(preds = bassPreds,
	     lower = bassLower,
	     upper = bassUpper,
	     time = bassTime)
saveRDS(bass, "results/flood_results_bass.RDS")


rm(list=ls())
gc()
if (file.exists(".RData")) {
  remove(".RData")
}
