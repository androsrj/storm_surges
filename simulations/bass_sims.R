load("data/flood_data.RData")
mySeed <- 9999

storms <- 1:5
stormsTest <- 6:10
nTestSubj <- length(stormsTest)
inputs <- inputs[c(storms, stormsTest), ]
out <- out[c(storms, stormsTest), ]
indexTest <- readRDS("results/test_points.RDS")
nTest <- length(indexTest)

library(BASS)
set.seed(mySeed)
model <- bassPCA(inputs[-stormsTest, ], out[-stormsTest, ], n.pc = 3, n.cores = 1)
predictions <- predict(model, inputs[stormsTest, ])[ , , indexTest]

bassPreds <- apply(predictions, 2:3, mean)
bassLower <- apply(predictions, 2:3, quantile, .025)
bassUpper <- apply(predictions, 2:3, quantile, .975)
bass <- list(preds = bassPreds, lower = bassLower, upper = bassUpper)
saveRDS(bass, "results/flood_results_bass.RDS")

mspe <- sapply(1:nTestSubj, \(i) mean((bassPreds[i, ] - out[stormsTest[i], indexTest])^2) )
mean(mspe)

pct <- sapply(1:nTestSubj, \(i) mean((bassPreds[i, ] >= 4/3.28084) == (out[stormsTest[i], indexTest] >= 4/3.28084)) )
1-mean(pct)
