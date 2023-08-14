library(fields)

# Function to simulate spatial data
spatialData <- function(n, X = NULL, S = NULL, range = c(0, 10), dims = 2, 
                    sigma2 = 2, tau2 = 0.2, theta = 3, beta = c(2, -1),
                    covariance = "exponential") {
  
  # Sample the locations and put them into an n-by-dims matrix
  # Unless the coordinates are pre-supplied
  if (is.null(S)) {
    locations <- runif(n * dims, range[1], range[2])
    S <- matrix(locations, nrow = n, ncol = dims)
  }
  
  # Order S by sum of coordinates
  S <- S[order(rowSums(S)), ]
  
  # Compute the covariance matrix (symmetric)
  if (covariance == "exponential") {
    D <- rdist(S)
  } else if (covariance == "exp_squared") {
    D <- rdist(S)^2
  } else {
    stop("Covariance function must be either exponential or exp_squared.")
  }
  C <- sigma2 * exp(- theta * D)
  
  # Sample W
  W <- t(rmvnorm(1, sigma = C))
  
  # If X is not supplied, set it to a matrix containing a column of ones (intercept) 
  # and one standard normal predictor
  if (is.null(X)) {
    Xint <- rep(1, n)
    Xpred <- rnorm(n)
    X <- matrix(Xint, Xpred, nrow = n, ncol = 2)
  }
  
  # Sample epsilon
  eps <- rnorm(n, 0, sqrt(tau2))
  
  # Generate Y
  Y <- X %*% beta + W + eps
  
  # Return data
  return(list(X = X, Y = as.vector(Y), W = as.vector(W), D = D, S = S))
}
