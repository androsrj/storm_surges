sketching <- readRDS("results/flood_results_sketching.RDS")
bart <- readRDS("results/flood_results_bart.RDS")
nngp <- readRDS("results/flood_results_nngp.RDS")
data_split <- readRDS("results/data_split.RDS")
indexTest <- data_split[[2]]
nTest <- length(indexTest)
load("data/flood_data.RData")
test_subjects <- 1:2


# Parameter estimates
sigma2 <- c(sketching$means['sigma2'], 
            sketching$lower[1],
            sketching$upper[1])
tau2 <- c(sketching$means['tau2'], 
          sketching$lower[2],
          sketching$upper[2])
beta <- c(sketching$means['beta7'], 
          sketching$lower[9],
          sketching$upper[9])
sketchParams <- data.frame(sigma2, tau2, beta)
rownames(sketchParams) <- c("mean", "lower", "upper")

means <- apply(sapply(test_subjects, function(i) apply(nngp$obj[[i]]$p.theta.samples, 2, mean)), 1, mean)
lowers <- apply(sapply(test_subjects, function(i) apply(nngp$obj[[i]]$p.theta.samples, 2, quantile, .025)), 1, mean)
uppers <- apply(sapply(test_subjects, function(i) apply(nngp$obj[[i]]$p.theta.samples, 2, quantile, .975)), 1, mean)
betas <- sapply(test_subjects, function(i) mean(nngp$obj[[i]]$p.beta.samples[ ,2]))
nngpParams <- as.data.frame(rbind(means, lowers, uppers))
nngpParams <- data.frame(nngpParams, beta = c(mean(betas), quantile(betas, c(.025, .975))))
rownames(nngpParams) <- c("mean", "lower", "upper")


length <- cvg <- score <- mspe <- pct <- matrix(0, nrow = length(test_subjects), ncol = 3)
for (i in test_subjects) {
  # True values
  trueTest <- out[i, indexTest]
  
  # BART predictions
  bartPreds <- bart[[i]]$yhat.test.mean
  bartCI <- apply(bart[[i]]$yhat.test, 2, quantile, c(0.025, 0.975))
  bartLower <- bartCI[1, ]
  bartUpper <- bartCI[2, ]

  # NNGP predictions
  nngpPreds <- apply(nngp$preds[[i]]$p.y.0, 1, mean)
  nngpLower <- apply(nngp$preds[[i]]$p.y.0, 1, quantile, .025)
  nngpUpper <- apply(nngp$preds[[i]]$p.y.0, 1, quantile, .975)

  # Sketching predictions
  sketchPreds <- sketching$predictions[[i]][2, ]
  sketchLower <- sketching$predictions[[i]][1, ]
  sketchUpper <- sketching$predictions[[i]][3, ]
  
  # Length
  lengthSketch <- mean(sketchUpper - sketchLower)
  lengthBart <- mean(bartUpper - bartLower)
  lengthNNGP <- mean(nngpUpper - nngpLower)
  length[i, ] <- c(lengthSketch, lengthBart, lengthNNGP)

  # Coverage
  cvgSketch <- mean(sketchUpper > trueTest & sketchLower < trueTest)
  cvgBart <- mean(bartUpper > trueTest & bartLower < trueTest)
  cvgNNGP <- mean(nngpUpper > trueTest & nngpLower < trueTest)
  cvg[i, ] <- c(cvgSketch, cvgBart, cvgNNGP)

  # MSPE
  mspeSketch <- mean((sketchPreds - trueTest)^2)
  mspeBart <- mean((bartPreds - trueTest)^2)
  mspeNNGP <- mean((nngpPreds - trueTest)^2)
  mspe[i, ] <- c(mspeSketch, mspeBart, mspeNNGP)

  # Interval score
  a <- 0.05
  scoreSketch <- mean( (sketchUpper - sketchLower) + 
                 2/a * (sketchLower - trueTest) * 
                 (trueTest < sketchLower) + 
                 2/a * (trueTest - sketchUpper) * 
                 (trueTest > sketchUpper) ) 
  scoreBart <- mean( (bartUpper - bartLower) +
		   2/a * (bartLower - trueTest) *
		   (trueTest < bartLower) + 
		   2/a * (trueTest - bartUpper) * 
		   (trueTest > bartUpper) )
  scoreNNGP <- mean( (nngpUpper - nngpLower) +
		   2/a * (nngpLower - trueTest) *
		   (trueTest < nngpLower) + 
		   2/a * (trueTest - nngpUpper) * 
		   (trueTest > nngpUpper) )
  score[i, ] <- c(scoreSketch, scoreBart, scoreNNGP)

  # Over/under 4 feet
  trueFeet <- trueTest * 3.28084
  trueOver <- trueFeet >= 4.0

  sketchFeet <- sketchPreds * 3.28084
  sketchOver <- sketchFeet >= 4.0
  sketchPct <- mean(trueOver == sketchOver)
  
  bartFeet <- bartPreds * 3.28084
  bartOver <- bartFeet >= 4.0
  bartPct <- mean(trueOver == bartOver)

  nngpFeet <- nngpPreds * 3.28084
  nngpOver <- nngpFeet >= 4.0
  nngpPct <- mean(trueOver == nngpOver)
  
  pct[i, ] <- c(sketchPct, bartPct, nngpPct)
}

length <- apply(length, 2, mean)
cvg <- apply(cvg, 2, mean)
mspe <- apply(mspe, 2, mean)
score <- apply(score, 2, mean)
pct <- 1 - apply(pct, 2, mean)

# Parameter estimates

sketchParams
nngpParams

# Predictive diagnostics (our approach vs BART)
preds_df <- data.frame(cvg = cvg, 
		       length = length, 
		       mspe = mspe, 
		       score = score, 
		       pct = pct)
rownames(preds_df) <- c("Sketching", "BART", "NNGP")
preds_df

# Also make sure that acceptance rates are satisfactory
sketching$acc
