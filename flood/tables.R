sketching <- readRDS("results/flood_results_sketching.RDS")
bart <- readRDS("results/flood_results_bart.RDS")
data_split <- readRDS("results/data_split.RDS")
indexTest <- data_split[[2]]
nTest <- length(indexTest)
load("data/flood_data.RData")
test_subj <- 1
trueTest <- out[test_subj, indexTest]

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

# BART predictions
bartPreds <- bart[[test_subj]]$yhat.test.mean
bartCI <- apply(bart[[test_subj]]$yhat.test, 2, quantile, c(0.025, 0.975))
bartLower <- bartCI[1, ]
bartUpper <- bartCI[2, ]

# Length
lengthSketch <- mean(sketching$predictions[3,] - sketching$predictions[1,])
lengthBart <- mean(bartUpper - bartLower)
length <- c(lengthSketch, lengthBart)

# Coverage
cvgSketch <- mean(sketching$predictions[3,] > trueTest & sketching$predictions[1,] < trueTest)
cvgBart <- mean(bartUpper > trueTest & bartLower < trueTest)
cvg <- c(cvgSketch, cvgBart)

# MSPE
mspeSketch <- mean((sketching$predictions[2,] - trueTest)^2)
mspeBart <- mean((bartPreds - trueTest)^2)
mspe <- c(mspeSketch, mspeBart)

# Interval score
a <- 0.05
scoreSketch <- mean( (sketching$predictions[3, ] - sketching$predictions[1, ]) + 
                 2/a * (sketching$predictions[1, ] - trueTest) * 
                 (trueTest < sketching$predictions[1, ]) + 
                 2/a * (trueTest - sketching$predictions[3, ]) * 
                 (trueTest > sketching$predictions[3, ]) ) 
scoreBart <- mean( (bartUpper - bartLower) +
		   2/a * (bartLower - trueTest) *
		   (trueTest < bartLower) + 
		   2/a * (trueTest - bartUpper) * 
		   (trueTest > bartUpper) )
score <- c(scoreSketch, scoreBart)

# Over/under 4 feet
trueFeet <- trueTest * 3.28084
sketchFeet <- sketching$predictions[2, ] * 3.28084
trueOver <- trueFeet >= 4.0
sketchOver <- sketchFeet >= 4.0
sketchPct <- mean(trueOver == sketchOver)
bartFeet <- bartPreds * 3.28084
bartOver <- bartFeet >= 4.0
bartPct <- mean(trueOver == bartOver)
pct <- c(sketchPct, bartPct)

# Parameter estimates
params <- data.frame(sigma2, tau2, beta)
rownames(params) <- c("mean", "lower", "upper")
params

# Predictive diagnostics (our approach vs BART)
preds_df <- data.frame(cvg = cvg, 
		       length = length, 
		       mspe = mspe, 
		       score = score, 
		       pct = pct)
rownames(preds_df) <- c("Sketching", "BART")
preds_df

# Also make sure that acceptance rates are satisfactory
sketching$acc
