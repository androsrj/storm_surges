library(fields)

# Function to simulate spatial data
spatialData <- function(n, X, Z, 
                        sigma2, tau2, theta, beta, gamma,
                        S = NULL, range = c(0, 10), dims = 2, 
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
  
  # Sample epsilon
  #eps <- rnorm(n, 0, sqrt(tau2))
  
  # Generate Y
  nSubj <- length(Z)
  n <- nrow(X)
  #Y <- lapply(1:nSubj, \(i) X %*% beta + rep(Z[i], n) * gamma + W + eps)
  X <- lapply(1:nSubj, \(i) cbind(rep(Z[i], n), X))
  Y <- lapply(1:nSubj, \(i) X[[i]] %*% c(gamma, beta) + W + rnorm(n, 0, sqrt(tau2)))
  
  # Return data
  return(list(X = X, Y = Y, W = as.vector(W), D = D, S = S))
}
