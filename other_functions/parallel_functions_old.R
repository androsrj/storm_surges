# Helper function to run subsets in parallel for sketching
sketching_parallel <- function(i) {
  path <- paste0("results/sketching/rep", i, ".RDS")
  flood_results <- flood_mcmc(X = XTrain, Y = YTrain, D = DTrain, S = STrain,
                              nSubj = nSubj,
                              theta = thetaVals[i],
                              propSD = c(0.01, 0.04),
                              nIter = 2000, nBurn = 500,
                              model = "full_gp",
                              mProp = 0.01,
                              transform = TRUE)
  saveRDS(flood_results, path)
}


# Parallel for subdomain split
subdomains_parallel <- function(i) {
  path <- paste0("results/d_and_c/subdomains/rep", i, ".RDS")
  flood_results <- flood_mcmc(X = subsetsX[[i]], Y = subsetsY[[i]], 
                              D = subsetsD[[i]], S = subsetsS[[i]],
                              nSubj = nSubj,
                              theta = runif(1, 10, 100),
                              propSD = c(0.01, 0.04),
                              nIter = 2000, nBurn = 500,
                              model = "full_gp",
                              mProp = 0.01,
                              transform = TRUE)
  saveRDS(flood_results, path)
}


# Parallel for stratified split
stratified_parallel <- function(i) {
  path <- paste0("results/d_and_c/stratified/rep", i, ".RDS")
  flood_results <- flood_mcmc(X = subsetsX[[i]], Y = subsetsY[[i]], subsetsD[[i]],
                              nSubj = nSubj,
                              theta = runif(1, 10, 100),
                              propSD = c(0.01, 0.04),
                              nIter = 2000, nBurn = 500,
                              model = "full_gp",
                              mProp = 0.01,
                              transform = TRUE)
  saveRDS(flood_results, path)
}
