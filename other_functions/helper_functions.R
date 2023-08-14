### Wasserstein mean for multiple Markov chains (multiple distributions) 
wasserstein <- function(nChains, alpha = 0.05) {
  results <- vector("list", nChains)
  for (i in 1:nChains) {
    path <- paste0("results/flood_rep", i, ".RDS")
    results[[i]] <- readRDS(path)
  }
  wassersteinAcc <- rowMeans(sapply(results, \(x) unlist(x$acceptance)))
  wassersteinMeans <- rowMeans(sapply(results, \(x) unlist(x$posteriorMedians)))
  wassersteinLower <- rowMeans(sapply(results, \(x) unlist(x$credLower)))
  wassersteinUpper <- rowMeans(sapply(results, \(x) unlist(x$credUpper)))
  
  #predsList <- lapply(results, \(x) x$preds)
  #predictions <- Reduce("+", predsList) / length(predsList)
  
  wassersteinResults <- list(acc = wassersteinAcc, 
                             means = wassersteinMeans, 
                             lower = wassersteinLower,
                             upper = wassersteinUpper, 
                             #predictions = predictions,
                             time = final.time)
}


# Calculates the base of the covariance matrix for likelihood function
baseVariance <- function(theta, phi) {
  
  C <- exp(- theta * DTrain)
  B <- tcrossprod(phi %*% C, phi)
  
  return(B)
}


# Convert list of cluster labels to vector of indices (for data subsetting)
list2Vec <- function(ls) {
  temp <- rep(seq_along(ls), lengths(ls))
  temp[unlist(ls)]
}

