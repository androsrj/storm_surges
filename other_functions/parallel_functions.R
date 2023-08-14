# Helper function to run subsets in parallel for sketching
sketching_parallel <- function(i) {
  path <- paste0("results/flood_rep", i, ".RDS")
  flood_results <- flood_mcmc(X = XTrain, Y = YTrain, D = DTrain,
                              nKnots = 0,
                              theta = thetaVals[i],
                              propSD = c(0.05, 0.035),
                              nIter = 100, nBurn = 100,
                              model = "full_gp",
                              mProp = 0.01,
                              transform = TRUE)
  saveRDS(flood_results, path)
}
