# Helper function to run subsets in parallel for D-and-C
DC_parallel <- function(i) {
  path <- paste0("results/d_and_c/", model, "/", splitType, "/rep", i, ".RDS")
  results <- mcmc(X = subsetsX[[i]], 
                  Y = subsetsY[[i]],
                  D = subsetsD[[i]],
                  S = subsetsS[[i]],
                  nSubj = nSubj,
                  theta = runif(1, 2, 4),
                  propSD = c(0.1, 0.25),
                  nIter = 1000, nBurn = 100,
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
                  propSD = c(0.08, 0.65),
                  nIter = 1000, nBurn = 100,
                  model = model,
                  mProp = 0.02,
                  transform = TRUE)
  saveRDS(results, path)
}
