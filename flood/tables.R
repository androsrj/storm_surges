sketching <- readRDS("results/flood_results_sketching.RDS")
data_split <- readRDS("results/data_split.RDS")
indexTest <- data_split[[2]]
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

# Length
length <- mean(sketching$predictions[3,] - sketching$predictions[1,])

# Coverage
cvg <- mean(sketching$predictions[3,] > trueTest & sketching$predictions[1,] < trueTest)

# MSPE
mspe <- mean((sketching$predictions[2,] - trueTest)^2)

# Interval score
a <- 0.05
score <- mean( (sketching$predictions[3, ] - sketching$predictions[1, ]) + 
                 2/a * (sketching$predictions[1, ] - trueTest[[test_subj]]) * 
                 (trueTest[[test_subj]] < sketching$predictions[1, ]) + 2/a * 
                 (trueTest[[test_subj]] - sketching$predictions[3, ]) * 
                 (trueTest[[test_subj]] > sketching$predictions[3, ]) ) 

# Over/under 4 feet
trueFeet <- trueTest * 3.28084
predsFeet <- sketching$predictions[2, ] * 3.28084
trueOver <- trueFeet >= 4.0
predOver <- predsFeet >= 4.0
pctCorrect <- mean(trueOver == predOver)

# Final results table
final_df <- data.frame(sigma2, tau2, beta)
rownames(final_df) <- c("mean", "lower", "upper")
final_df

# Predictive diagnostics
c(cvg = cvg, length = length, mspe = mspe, score = score, pct = pctCorrect)

# Also make sure that acceptance rates are satisfactory
sketching$acc
