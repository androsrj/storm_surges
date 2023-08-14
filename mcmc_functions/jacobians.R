### JACOBIAN AND TRANSFORMATIONS (for theta) ###

# Transformation for theta, where trTheta = log((theta - a) / (b - theta))
f <- function(x, a = 0.5, b = 10) {
  log((a - x) / (x - b))
}

# Inverse transformation for theta
fInv <- function(trTheta, a = 0.5, b = 10) {
  (b * exp(trTheta) + a) / (1 + exp(trTheta))
}

# Log-Jacobian for theta, (log-derivative of fInv function above)
logJac <- function(trTheta, a = 0.5, b = 10) {
  # log( (b - a) * exp(trTheta) / (1 + exp(trTheta))^2 ) # or simplify, as below
  log(b - a) + trTheta - 2 * log(1 + exp(trTheta))
}