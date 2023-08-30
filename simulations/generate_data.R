source("other_functions/spatial_data.R")
mySeed <- 123

# Sample sizes
nTrain <- 10000
nKnots <- 500
nTest <- 1000
n <- nTrain + nKnots

# True parameter values
trueSigma2 <- 2
trueTau2 <- 0.2
trueTheta <- 3
trueBeta <- c(2, -1)
p <- length(trueBeta)

# Generate training data
set.seed(mySeed)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
X[1:nTrain, ] <- X[order(X[1:nTrain, 1]), ]
train <- spatialData(n = n, 
                     X = X, 
                     sigma2 = trueSigma2, 
                     tau2 = trueTau2, 
                     theta = trueTheta, 
                     beta = trueBeta)
save(train, file = "data/train.RData")

# Generate testing data
set.seed(mySeed)
X <- matrix(rnorm(nTest * p), nrow = nTest, ncol = p)
X <- X[order(X[ , 1]), ]
test <- spatialData(n = nTest, 
                    X = X, 
                    sigma2 = trueSigma2, 
                    tau2 = trueTau2, 
                    theta = trueTheta, 
                    beta = trueBeta)
save(test, file = "data/test.RData")
