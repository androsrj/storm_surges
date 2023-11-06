### Wasserstein mean for multiple Markov chains (multiple distributions) 
wasserstein <- function(results, time) {

  wassersteinAcc <- rowMeans(sapply(results, \(x) unlist(x$acceptance)))
  wassersteinMeans <- rowMeans(sapply(results, \(x) unlist(x$posteriorMedians)))
  wassersteinLower <- rowMeans(sapply(results, \(x) unlist(x$credLower)))
  wassersteinUpper <- rowMeans(sapply(results, \(x) unlist(x$credUpper)))
  
  predictions <- vector("list", nTestSubj)
  for (i in 1:nTestSubj) {
    predsList <- lapply(results, \(x) x$preds[[i]])
    predictions[[i]] <- Reduce("+", predsList) / length(predsList)
  }
    
  wassersteinResults <- list(acc = wassersteinAcc, 
                             means = wassersteinMeans, 
                             lower = wassersteinLower,
                             upper = wassersteinUpper, 
                             predictions = predictions,
                             time = time)
}

# Energy score (CRPS) calculation for predictions vs truth
energy_score = function(y, z){
  d = dim(z)
  n_samp = d[1]
  n = d[2]
  G = colMeans(sapply(1:n,function(i) sapply(1:n_samp, function(k) norm(z[k,i]-y[i],type='2'))))
  UQ = sapply(1:n,function(i) (1/(2*n_samp^2))*sum(as.matrix(distances::distances(z[,i]))))
  return(G - UQ)
}


# Calculates the base of the covariance matrix for likelihood function
baseVariance <- function(theta, phi, D) {
  
  if (model == "mpp") {
    #DTrain <- D[1:nTrain, 1:nTrain]
    #DKnot <- D[(nTrain + 1):nObs, (nTrain + 1):nObs]
    nKnots <- nrow(DKnot)
    n <- nrow(D)
    DCov <- D_mpp[1:n, (n + 1):(n + nKnots)]
    
    CTrain <- exp(- theta * diag(D))
    CCov <- exp(- theta * DCov)
    CKnot <- exp(- theta * DKnot)
    
    # Modified predictive process (variance correction)
    middle <- tcrossprod(CCov %*% solve(CKnot), CCov)
    delta <- diag(CTrain - diag(middle))
    B <- tcrossprod(phi %*% (middle + delta), phi) 
    
  } else if (model == "full_gp") {
    
    C <- exp(- theta * D)
    B <- tcrossprod(phi %*% C, phi)
    
  } else if (model == "sparse_gp") {
    
    C <- exp(- theta * D)
    B <- tcrossprod(phi %*% C, phi)
    B <- sparse(B)
    
  } else (
    stop("Model type must be either 'mpp', 'full_gp', or 'sparse_gp'.")
  )
  
  return(B)
  
}


# Convert list of cluster labels to vector of indices (for data subsetting)
list2Vec <- function(ls) {
  temp <- rep(seq_along(ls), lengths(ls))
  temp[unlist(ls)]
}

