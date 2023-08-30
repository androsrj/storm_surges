source("other_functions/spatial_data.R")
mySeed <- 123

# Sample sizes
nTrain <- 10000
nKnots <- 500
nTest <- 1000
n <- nTrain + nKnots
nSubj <- 10

# True parameter values
trueSigma2 <- 2
trueTau2 <- 0.2
trueTheta <- 3
trueBeta <- c(2, -1)
trueGamma <- 5
p <- length(trueBeta) + length(trueGamma)

# Generate training data
set.seed(mySeed)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
X[1:nTrain, ] <- X[order(X[1:nTrain, 1]), ]
Z <- seq(5, 50, length = nSubj)
train <- spatialData(n = n, 
                     X = X, 
                     Z = Z,
                     sigma2 = trueSigma2, 
                     tau2 = trueTau2, 
                     theta = trueTheta, 
                     beta = trueBeta,
                     gamma = trueGamma)
save(train, file = "data/train.RData")

# Generate testing data
set.seed(mySeed)
X <- matrix(rnorm(nTest * p), nrow = nTest, ncol = p)
X <- X[order(X[ , 1]), ]
Z <- seq(5, 50, length = nSubj)
test <- spatialData(n = nTest, 
                    X = X, 
                    Z = Z,
                    sigma2 = trueSigma2, 
                    tau2 = trueTau2, 
                    theta = trueTheta, 
                    beta = trueBeta,
                    gamma = trueGamma)
save(test, file = "data/test.RData")
