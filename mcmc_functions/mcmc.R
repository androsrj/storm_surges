### FINAL MCMC SKETCHING FUNCTION ###
# Priors, Jacobians, likelihood, and posterior must already be sourced

mcmc <- function(X, Y, D, S,
                 nSubj, theta,
                 model = c("full_gp", "mpp", "sparse_gpp")[1],
                 propSD = c(0.1, 0.1), 
                 nIter = 1000, nBurn = 100, nThin = 2,
                 mProp = 0.1, transform = TRUE) {
  
  # Save model type and theta globally
  model <<- model
  theta <<- theta
  subjs <<- 1:nSubj
  
  # Dimensions
  n <- nrow(X[[1]])
  p <- ncol(X[[1]])
  
  # Generate phi and compress data, if desired
  if (transform == TRUE) {
    m <<- round(mProp * n)
    phi <<- matrix(rnorm(m * n, 0, 1 / sqrt(n)), nrow = m, ncol = n)
    newY <<- lapply(Y, \(y) phi %*% y) 
    newX <<- lapply(X, \(x) phi %*% x) 
  } else {
    m <<- n
    phi <<- diag(m)
    newY <<- Y
    newX <<- X
  }
  
  # If MPP model, setup to obtain partition of covariance matrix
  if (model == "mpp") {
    S_mpp <<- rbind(S, SKnot)
    D_mpp <<- rdist(S_mpp)
  }
  
  # MCMC chain properties
  nIter <- nBurn + nIter # 15 to 20 thousand ideally
  
  # Tuning parameters for variance of each proposal distribution
  # Can be user-supplied
  sdSigma2 <- propSD[1]
  sdTau2 <- propSD[2]
  #sdTheta <- propSD[3]
  
  #trSigma2 <- trTau2 <- trTheta <- numeric(nIter) # Transformed parameters
  trSigma2 <- trTau2 <- numeric(nIter) # Transformed parameters
  beta <- matrix(0, nrow = p, ncol = nIter) # Beta
  #acceptSigma2 <- acceptTau2 <- acceptTheta <- 0 # Track acceptance rates
  acceptSigma2 <- acceptTau2 <- 0 # Track acceptance rates
  
  # Initial values of transformed parameters (except for beta, not transformed)
  trSigma2[1] <- log(2)
  trTau2[1] <- log(0.2)
  #trTheta[1] <- log((3 - a) / (b - 3))
  beta[ , 1] <- rep(0, p)
  
  # Base of covariance matrix for updating sigma2 (only need to compute once)
  B <<- baseVariance(theta, phi = phi, D = D)
  Sigma <<- exp(trSigma2[1]) * B + exp(trTau2[1]) * diag(m)
    
  # Initial predictions for storm 1 (and non-transformed covariance matrix)
  BTest <<- exp(-theta * DTest)
  SigmaTest <<- exp(trSigma2[1]) * BTest + exp(trTau2[1]) * diag(nTest)
  YPreds <- matrix(0, nrow = nTest, ncol = nIter) 
  YPreds[ , 1] <- t(rmvnorm(1, mean = as.vector(XTest[[1]] %*% beta[ , 1]), sigma = SigmaTest))
  
  # Run Gibbs/Metropolis for one chain
  for (i in 2:nIter) {

    cat(paste0("Beginning iteration ", i, ".\n"))
    
    ### Metropolis update (sigma2) ###
    
    propTrSigma2 <- rnorm(1, mean = trSigma2[i - 1], sd = sdSigma2)
    #MHratio <- logPost(propTrSigma2, trTau2[i - 1], theta, beta[ , i - 1]) - 
    #  logPost(trSigma2[i - 1], trTau2[i - 1], theta, beta[ , i - 1])
    MHratio <- logRatioSigma2(propTrSigma2, 
                              trSigma2[i - 1], 
                              trTau2[i - 1],
                              beta[ , i - 1])
    
    if(runif(1) < exp(MHratio)) {
      trSigma2[i] <- propTrSigma2
      Sigma <<- SigmaProp
      acceptSigma2 <- acceptSigma2 + 1
    } else {
      trSigma2[i] <- trSigma2[i - 1]
    }
    
    ### Metropolis update (tau2) ###
    
    propTrTau2 <- rnorm(1, mean = trTau2[i - 1], sd = sdTau2)
    #MHratio <- logPost(trSigma2[i], propTrTau2, theta, beta[ , i - 1]) - 
    #  logPost(trSigma2[i], trTau2[i - 1], theta, beta[ , i - 1])
    MHratio <- logRatioTau2(trSigma2[i - 1], 
                            propTrTau2,
                            trTau2[i - 1],
                            beta[ , i - 1])
    if (runif(1) < exp(MHratio)) { 
      trTau2[i] <- propTrTau2
      Sigma <<-  SigmaProp
      acceptTau2 <- acceptTau2 + 1
    } else {
      trTau2[i] <- trTau2[i - 1]
    }
    
    ### Metropolis update (theta) ###
    
    #propTrTheta <- rnorm(1, mean = trTheta[i - 1], sd = sdTheta)
    #MHratio <- logPost(trSigma2[i], trTau2[i], propTrTheta, beta[ , i - 1]) - 
    #  logPost(trSigma2[i], trTau2[i], trTheta[i - 1], beta[ , i - 1])
    #if(runif(1) < exp(MHratio)) {
    #  trTheta[i] <- propTrTheta
    #  acceptTheta <- acceptTheta + 1 
    #} else {
    #  trTheta[i] <- trTheta[i - 1]
    #}
    #cat("Updated theta.\n")
    
    ### Gibbs update (beta) ###
    
    SigmaInv <- solve(Sigma)
    #SigmaBeta <- (n / m) * t(newX) %*% SigmaInv %*% newX + diag(p)
    SigmaBetaList <- lapply(newX, \(x) t(x) %*% SigmaInv %*% x)
    SigmaBeta <- (n / m) * (Reduce("+", SigmaBetaList) + diag(p))
    SigmaBetaInv <- solve(SigmaBeta)
    #meanBeta <- (n / m) * SigmaBetaInv %*% t(newX) %*% SigmaInv %*% newY
    meanBetaList <- lapply(subjs, \(i) t(newX[[i]]) %*% SigmaInv %*% newY[[i]])
    meanBeta <- (n / m) * SigmaBetaInv %*% Reduce("+", meanBetaList)
    beta[ , i] <- t(rmvnorm(1, meanBeta, SigmaBetaInv))
    
    ### Posterior predictive sampling for storm 1 ###
    # Computationally expensive - only compute every 10th iteration
    if (i %% 10 == 0) {
      SigmaTest <- exp(trSigma2[i]) * BTest + exp(trTau2[i]) * diag(nTest)
      YPreds[ , i] <- t(rmvnorm(1, mean = as.vector(XTest[[1]] %*% beta[ , i]), sigma = SigmaTest))
    }
  }
  
  # Acceptance rates (for Metropolis-sampled parameters)
  acceptance <- c(sigma2 = acceptSigma2, 
                  tau2 = acceptTau2) / nIter
  
  # Remove burn-in and perform thinning
  index <- seq(nBurn + 1, nIter, by = nThin)
  indexY <- seq(nBurn + 10, nIter, by = 10)
  trSigma2 <- trSigma2[index]
  trTau2 <- trTau2[index]
  #trTheta <- trTheta[index]
  beta <- beta[ , index]
  YPreds <- YPreds[ , indexY]
  nSamples <- length(index)
  
  # Back-transform
  sigma2 <- exp(trSigma2)
  tau2 <- exp(trTau2)
  #theta <- fInv(trTheta)
  
  # Trace plots
  #pdf(paste0("../paper/figures/trace_plots/trace_plots_", model, ".pdf"))
  #plot(1:nSamples, sigma2, type = 'l', ylab = "Sigma2", main = "")
  #plot(1:nSamples, tau2, type = 'l', ylab = "Tau2", main = "")
  ##plot(1:nSamples, theta, type = 'l', ylab = "Trace Plot for theta")
  #plot(1:nSamples, beta[1, ], type = 'l', ylab = "Beta_1", main = "")
  #plot(1:nSamples, beta[p, ], type = 'l', ylab = "Beta_p", main = "")
  #dev.off()  
  
  # Posterior mean estimates (can be somewhat skewed because of back-transformations)
  posteriorMeans <- list(sigma2 = mean(sigma2),
                         tau2 = mean(tau2),
                         #theta = mean(theta),
                         beta = apply(beta, 1, mean))
  
  # Posterior median estimates (more accurate)
  posteriorMedians <- list(sigma2 = median(sigma2),
                           tau2 = median(tau2),
                           #theta = median(theta),
                           beta = apply(beta, 1, median))
  
  # 95% credible interval bounds
  credLower <- list(sigma2 = quantile(sigma2, 0.025), 
                    tau2 = quantile(tau2, 0.025),
                    #theta = quantile(theta, 0.025), 
                    beta = apply(beta, 1, quantile, 0.025))
  credUpper <- list(sigma2 = quantile(sigma2, 0.975), 
                    tau2 = quantile(tau2, 0.975),
                    #theta = quantile(theta, 0.975), 
                    beta = apply(beta, 1, quantile, 0.975))
  
  # Posterior predictive results for test data
  preds <- apply(YPreds, 1, quantile, c(0.025, 0.5, 0.975))
  
  # Return results
  return(list(acceptance = acceptance, 
              posteriorMeans = posteriorMeans,
              posteriorMedians = posteriorMedians,
              credLower = credLower,
              credUpper = credUpper,
              preds = preds,
	      predSamples = YPreds))
}


