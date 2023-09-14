# Helper function to run subsets in parallel for D-and-C
DC_parallel <- function(i) {
  path <- paste0("results/d_and_c/", model, "/", splitType, "/rep", i, ".RDS")
  results <- mcmc(X = subsetsX[[i]], 
                  Y = subsetsY[[i]],
                  D = subsetsD[[i]],
                  S = subsetsS[[i]],
                  nSubj = nSubj,
                  theta = runif(1, 2, 4),
                  propSD = c(0.03, 0.1),
                  nIter = 2000, nBurn = 500,
                  model = model,
                  transform = FALSE)
  saveRDS(results, path)
}

# Helper function to run subsets in parallel for sketching
sketching_parallel <- function(i) {
  path <- paste0("results/sketching/", model, "/rep", i, ".RDS")
  results <- mcmc(X = X, Y = Y, D = D, S = S,
                  nSubj = nSubj,
                  theta = thetaVals[i],
                  propSD = propSD,
                  nIter = 2000, nBurn = 500,
                  model = model,
                  mProp = mProp,
                  transform = TRUE)
  saveRDS(results, path)
}
