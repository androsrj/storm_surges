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
nCores <- 10
mySeed <- 999

# Read in indices for test data
n <- nrow(coords)
indexTest <- readRDS("results/test_points.RDS")
nTest <- length(indexTest)
storms <- 1:5
nSubj <- length(storms)
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
  Xstorm <- matrix(rep(unlist(inputs[i, ]), nTest), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTest]
  X <- cbind(Xstorm, Xelev)
  colnames(X) <- c(colnames(inputs), "elev")
  return(X)
})
STest <- coords[indexTest, 1:2]

##############
#### BART ####
##############

strt <- Sys.time()
bart_obj <- bart(do.call('rbind', X), do.call('c', Y), do.call('rbind', XTest))
bartPreds <- bart_obj$yhat.test.mean
bartLower <- apply(bart_obj$yhat.test, 2, quantile, 0.025)
bartUpper <- apply(bart_obj$yhat.test, 2, quantile, 0.975)

bartTime <- Sys.time() - strt
gc()

bart <- list(preds = bartPreds, 
	     lower = bartLower, 
	     upper = bartUpper,
	     time = bartTime)
saveRDS(bart, "results/flood_results_bart.RDS")
gc()


##############
#### NNGP ####
##############

nIter <- 2500
cov.model <- "exponential"
starting <- list("phi"=5000, "sigma.sq"=1, "tau.sq"=0.2)
tuning <- list("phi"=100, "sigma.sq"=2, "tau.sq"=2)
priors <- list("phi.Unif"=c(1,10000), "sigma.sq.IG"=c(1, 1), "tau.sq.IG"=c(1, 1))

# Remove global attributes from X train and test
X <- lapply(storms, \(i) coords$elev_meters)
XTest <- lapply(stormsTest, \(i) coords$elev_meters)

cl <- makeCluster(nCores)
registerDoParallel(cl)
strt <- Sys.time()
set.seed(mySeed)
nngp_obj <- foreach(i = 1:nSubj, .packages = "spNNGP") %dopar% spNNGP(Y[[i]] ~ X[[i]], coords=S, 
                                                                     starting=starting, method="latent", 
                                                                     n.neighbors=10, tuning=tuning, 
                                                                     priors=priors, cov.model=cov.model,
                                                                     n.samples=nIter, n.omp.threads=1, fit.rep=TRUE)
nngpTime <- Sys.time() - strt
stopCluster(cl)
gc()

# Extract parameter estimates and credible intervals
means <- apply(sapply(1:nSubj, function(i) apply(nngp_obj[[i]]$p.theta.samples, 2, mean)), 1, mean)
lowers <- apply(sapply(1:nSubj, function(i) apply(nngp_obj[[i]]$p.theta.samples, 2, quantile, .025)), 1, mean)
uppers <- apply(sapply(1:nSubj, function(i) apply(nngp_obj[[i]]$p.theta.samples, 2, quantile, .975)), 1, mean)
betas <- sapply(1:nSubj, function(i) mean(nngp_obj[[i]]$p.beta.samples[ ,2]))
nngpParams <- as.data.frame(rbind(means, lowers, uppers))
nngpParams <- data.frame(nngpParams, beta = c(mean(betas), quantile(betas, c(.025, .975))))
rownames(nngpParams) <- c("mean", "lower", "upper")

# Aggregate predictions for test points
nngpPreds <- apply(sapply(1:nTestSubj, \(i) nngp_obj[[i]]$y.hat.quant[indexTest, 1]), 1, mean)
nngpLower <- apply(sapply(1:nTestSubj, \(i) nngp_obj[[i]]$y.hat.quant[indexTest, 2]), 1, mean)
nngpUpper <- apply(sapply(1:nTestSubj, \(i) nngp_obj[[i]]$y.hat.quant[indexTest, 3]), 1, mean)

nngp <- list(params = nngpParams, 
	     preds = nngpPreds,
	     lower = nngpLower,
	     upper = nngpUpper,
	     time = nngpTime) 
saveRDS(nngp, "results/flood_results_nngp.RDS")


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
set.seed(mySeed)
predictions <- foreach(i=1:nReps, .packages = "BASS") %dopar% bassParallel(seed = mySeed - i)
stopCluster(cl)

dim(predictions[[1]]$preds)
dim(predictions[[1]]$lower)
dim(predictions[[1]]$upper)

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

#mspe <- sapply(1:nTestSubj, \(i) mean((bassPreds[i, ] - out[stormsTest[i], indexTest])^2) )
#print(mean(mspe))
#pct <- sapply(1:nTestSubj, \(i) mean((bassPreds[i, ] > 4) == (out[stormsTest[i], indexTest] > 4)) )
#print(1-mean(pct))

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
