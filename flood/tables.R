source("../other_functions/helper_functions.R")
sketching <- readRDS("results/flood_results_sketching.RDS")
nngp <- readRDS("results/flood_results_nngp.RDS")
bass <- readRDS("results/flood_results_bass.RDS")
#bart <- readRDS("results/flood_results_bart.RDS")
indexTest <- readRDS("results/test_points.RDS")
nTest <- length(indexTest)
test_subjects <- 6:10
nTestSubj <- length(test_subjects)
load("data/flood_data.RData")
n <- nrow(coords)

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

# Calculate all predictive diagnostics
length <- cvg <- score <- mspe <- pct <- matrix(0, nrow = length(test_subjects), ncol = 3)
for (i in 1:nTestSubj) {
  # True values
  trueTest <- out[test_subjects[i], indexTest]

  # Sketching predictions for storm i
  sketchPreds <- sketching$predictions[[i]][2, ]
  sketchLower <- sketching$predictions[[i]][1, ]
  sketchUpper <- sketching$predictions[[i]][3, ]

  # BASS predictions for storm i
  bassPreds <- bass$preds[i, ]
  bassLower <- bass$lower[i, ]
  bassUpper <- bass$upper[i, ]

  # BART predictions for storm i
  #bartPreds <- bart$preds[((i - 1) * nTest + 1):(i * nTest)]
  #bartLower <- bart$lower[((i - 1) * nTest + 1):(i * nTest)]
  #bartUpper <- bart$upper[((i - 1) * nTest + 1):(i * nTest)]
  
  # Length
  lengthSketch <- mean(sketchUpper - sketchLower)
  lengthNNGP <- mean(nngp$upper - nngp$lower)
  lengthBass <- mean(bassUpper - bassLower)
  #lengthBart <- mean(bartUpper - bartLower)
  length[i, ] <- c(lengthSketch, lengthNNGP, lengthBass)

  # Coverage
  cvgSketch <- mean(sketchUpper > trueTest & sketchLower < trueTest)
  cvgNNGP <- mean(nngp$upper > trueTest & nngp$lower < trueTest)
  cvgBass <- mean(bassUpper > trueTest & bassLower < trueTest)
  #cvgBart <- mean(bartUpper > trueTest & bartLower < trueTest)
  cvg[i, ] <- c(cvgSketch, cvgNNGP, cvgBass)

  # MSPE
  mspeSketch <- mean((sketchPreds - trueTest)^2)
  mspeNNGP <- mean((nngp$preds - trueTest)^2)
  mspeBass <- mean((bassPreds - trueTest)^2)
  #mspeBart <- mean((bartPreds - trueTest)^2)
  mspe[i, ] <- c(mspeSketch, mspeNNGP, mspeBass)

  # Interval score
  a <- 0.05
  scoreSketch <- mean( (sketchUpper - sketchLower) + 
                 2/a * (sketchLower - trueTest) * 
                 (trueTest < sketchLower) + 
                 2/a * (trueTest - sketchUpper) * 
                 (trueTest > sketchUpper) ) 
  scoreNNGP <- mean( (nngp$upper - nngp$lower) +
		   2/a * (nngp$lower - trueTest) *
		   (trueTest < nngp$lower) + 
		   2/a * (trueTest - nngp$upper) * 
		   (trueTest > nngp$upper) )
  scoreBass <- mean( (bassUpper - bassLower) +
		   2/a * (bassLower - trueTest) *
		   (trueTest < bassLower) + 
		   2/a * (trueTest - bassUpper) *
		   (trueTest > bassUpper) )
  #scoreBart <- mean( (bartUpper - bartLower) +
  #		   2/a * (bartLower - trueTest) *
  #		   (trueTest < bartLower) + 
  #		   2/a * (trueTest - bartUpper) * 
  #		   (trueTest > bartUpper) )
  score[i, ] <- c(scoreSketch, scoreNNGP, scoreBass)

  # Over/under 4 feet
  trueFeet <- trueTest * 3.28084
  trueOver <- trueFeet >= 4.0

  sketchFeet <- sketchPreds * 3.28084
  sketchOver <- sketchFeet >= 4.0
  sketchPct <- mean(trueOver == sketchOver)

  nngpFeet <- nngp$preds * 3.28084
  nngpOver <- nngpFeet >= 4.0
  nngpPct <- mean(trueOver == nngpOver)

  bassFeet <- bassPreds * 3.28084
  bassOver <- bassFeet >= 4.0
  bassPct <- mean(trueOver == bassOver)
  
  #bartFeet <- bartPreds * 3.28084
  #bartOver <- bartFeet >= 4.0
  #bartPct <- mean(trueOver == bartOver)

  pct[i, ] <- c(sketchPct, nngpPct, bassPct)
}

sketchCRPS <- mean(sapply(1:nTestSubj, function(i) {
  truth <- out[test_subjects[i], indexTest]
  preds <- sapply(1:nTestSubj, \(i) sketching$predictions[[i]][2, ])
  mean(energy_score(truth, preds))
}))

length <- apply(length, 2, mean)
cvg <- apply(cvg, 2, mean)
mspe <- apply(mspe, 2, mean)
crps <- c(sketchCRPS, nngp$crps, bass$crps)
score <- apply(score, 2, mean)
pct <- 1 - apply(pct, 2, mean)

# Parameter estimates
sketchParams
nngp$params


# Predictive diagnostics 
preds_df <- data.frame(mspe = mspe,
		       crps = crps,
		       cvg = cvg, 
		       score = score, 
		       pct = pct)
rownames(preds_df) <- c("Sketching", "NNGP", "BASS")
preds_df

# Also make sure that acceptance rates are satisfactory
sketching$acc

rm(list=ls())
gc()
if (file.exists(".RData")) {
  remove(".RData")
}
