### FINAL MCMC SKETCHING FUNCTION ###
# Priors, Jacobians, likelihood, and posterior must already be sourced

mcmc <- function(X, Y, D, S,
                 theta, test_subjects,
                 model = c("full_gp", "mpp", "sparse_gpp")[1],
                 propSD = c(0.1, 0.1), 
                 nIter = 1000, nBurn = 100, nThin = 2,
                 mProp = 0.1, transform = TRUE) {
  return(theta^2)
}


