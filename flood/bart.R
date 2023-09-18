# Clear environment and free unused memory
rm(list = ls())
gc()

# SOURCES
#source("../other_functions/parallel_functions.R") # Parallel wrapper functions
#source("../other_functions/helper_functions.R") # Other misc functions (not part of MCMC)

# Libraries
library(BayesTree)
library(fields)

# Read in
load("data/flood_data.RData")

# Clusters and seed
nCores <- 2
totalCores <- 10
mySeed <- 123

# Randomly sample indices for train and test data
nObs <- nrow(coords)
indices <- readRDS("results/data_split.RDS")
indexTrain <- indices[[1]]
indexTest <- indices[[2]]
nTrain <- n <- length(indexTrain)
nTest <- length(indexTest)

# Divide using train and test indices
storms <- 1:50
Y <- c(t(out[storms, indexTrain]))
X <- cbind(
  inputs[storms, ][rep(storms, each = nTrain), ],
  elev = rep(coords$elev_meters[indexTrain], length(storms))
)

YTest <- c(t(out[storms, indexTest]))
XTest <- cbind(
  inputs[storms, ][rep(storms, each = nTest), ],
  elev = rep(coords$elev_meters[indexTest], length(storms))
)


# BART
flood_results_bart <- bart(X, Y, XTest)

saveRDS(flood_results_bart, paste0("results/flood_results_bart.RDS"))

