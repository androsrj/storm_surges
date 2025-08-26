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
                  nIter = 5000, nBurn = 1000,
                  model = model,
                  transform = FALSE)
  #saveRDS(results, paste0("temp/results_", i, ".RDS"))
  results
}

# Helper function to run subsets in parallel for sketching
sketching_parallel <- function(i) {
  #path <- paste0("results/sketching/", model, "/rep", i, ".RDS")
  results <- mcmc(X = X, Y = Y, D = D, S = S,
                  theta = thetaVals[i],
                  test_subjects = test_subjects,
                  propSD = propSD,
                  nIter = 5000, nBurn = 1000,
                  model = model,
                  mProp = mProp,
                  transform = TRUE)
  #saveRDS(results, path)
  results
}
