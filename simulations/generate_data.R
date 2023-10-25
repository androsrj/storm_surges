source("../other_functions/spatial_data.R")
mySeed <- 1997

# Sample sizes
n <- 10000
nTest <- 1000
nSubj <- 10
nTestSubj <- 5

# True parameter values
trueSigma2 <- 2
trueTau2 <- 0.2
trueTheta <- 3
trueBeta <- c(2, -1)
trueGamma <- 5
p <- length(trueBeta) + length(trueGamma)

# Generate training data
set.seed(mySeed)
X <- matrix(rnorm(n * length(trueBeta)), nrow = n, ncol = length(trueBeta))
X <- X[order(X[ , 1]), ]
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
X <- matrix(rnorm(nTest * length(trueBeta)), nrow = nTest, ncol = length(trueBeta))
X <- X[order(X[ , 1]), ]
#Z <- seq(5, 50, length = nSubj)
Z <- runif(nTestSubj, 5, 50)
test <- spatialData(n = nTest, 
                    X = X, 
                    Z = Z,
                    sigma2 = trueSigma2, 
                    tau2 = trueTau2, 
                    theta = trueTheta, 
                    beta = trueBeta,
                    gamma = trueGamma)
save(test, file = "data/test.RData")
