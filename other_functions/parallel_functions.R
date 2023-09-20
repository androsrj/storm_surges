# Helper function to run subsets in parallel for D-and-C
DC_parallel <- function(i) {
  #path <- paste0("results/d_and_c/", model, "/", splitType, "/rep", i, ".RDS")
  results <- mcmc(X = subsetsX[[i]], 
                  Y = subsetsY[[i]],
                  D = subsetsD[[i]],
                  S = subsetsS[[i]],
                  theta = runif(1, 2, 4),
                  test_subjects = test_subjects,
                  propSD = propSD,
                  nIter = 200, nBurn = 50,
                  model = model,
                  transform = FALSE)
  #saveRDS(results, path)
  results
}

# Helper function to run subsets in parallel for sketching
sketching_parallel <- function(i) {
  #path <- paste0("results/sketching/", model, "/rep", i, ".RDS")
  results <- mcmc(X = X, Y = Y, D = D, S = S,
                  theta = thetaVals[i],
                  test_subjects = test_subjects,
                  propSD = propSD,
                  nIter = 200, nBurn = 50,
                  model = model,
                  mProp = mProp,
                  transform = TRUE)
  #saveRDS(results, path)
  results
}
