### LOG PRIORS ### 

# Sigma2 (inverse gamma)
logPriorSigma2 <- function(sigma2, a = 1, b = 1) {
  a * log(b) - lgamma(a) - (a + 1) * log(sigma2) - b / sigma2
}

# Tau2 (inverse gamma)
logPriorTau2 <- function(tau2, a = 1, b = 1) {
  a * log(b) - lgamma(a) - (a + 1) * log(tau2) - b / tau2
}

# Theta (uniform for now, could try discrete later)
logPriorTheta <- function(theta, a = 0.5, b = 10) {
  dunif(theta, a, b, log = TRUE)
}

# Beta (standard MV normal)
logPriorBeta <- function(beta) {
  p <- length(beta)
  dmvnorm(beta, mean = rep(0, p), sigma = diag(p), log = TRUE)
}