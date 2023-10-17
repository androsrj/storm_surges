# Clear environment and free unused memory
rm(list = ls())
gc()

### LOG LIKELIHOOD ###
logLik <- function(Sigma, beta) {
  newSigma <- phi %*% Sigma %*% t(phi)
  sum(sapply(subjs, function(i) {
    dmvnorm(as.vector(newY[[i]]), as.vector(newX[[i]] %*% beta), newSigma, log = TRUE)
  })) * (n / m)
}

library(mvtnorm)
library(fields)

# Read in
load("data/flood_data.RData")

# Clusters and seed
nCores <- 2
totalCores <- 10
mySeed <- 999
test_subjects <- 1:10

# Randomly sample indices for train and test data
nObs <- nrow(coords)
pctTrain <- 0.95
n <- nTrain <- floor(pctTrain * nObs)
nTest <- ceiling((1 - pctTrain) * nObs)
set.seed(mySeed)
indices <- readRDS("results/data_split.RDS")
indexTrain <- indices[[1]]
indexTest <- indices[[2]]

storms <- 1:50
Y <- lapply(storms, \(i) out[i, indexTrain])
X <- lapply(storms, \(i) {
  Xintercept <- rep(1, nTrain)
  Xstorm <- matrix(rep(unlist(inputs[i, ]), nTrain), ncol = 5, byrow = TRUE)
  Xelev <- coords$elev_meters[indexTrain]
  X <- cbind(Xintercept, Xstorm, Xelev)
  colnames(X) <- c("int", colnames(inputs), "elev")
  return(X)
})
S <- as.matrix(coords[indexTrain, 1:2])
D <- rdist(S)

m <- 100
phi <<- matrix(rnorm(m * n, 0, 1 / sqrt(n)), nrow = m, ncol = n)
newY <<- lapply(Y, \(y) phi %*% y) 
newX <<- lapply(X, \(x) phi %*% x) 

results <- readRDS("results/flood_results_sketching.RDS")
sigma2 <- 2
tau2 <- 0.25
beta <- results$means[3:9]
subjs <- storms
theta <- seq(20, 50, by=5)

lik <- sapply(1:length(theta), \(i) logLik(Sigma = sigma2 * exp(-theta[i] * D), beta = beta))
theta
lik

