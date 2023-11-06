library(MBA)
library(fields)

# Read in
load("data/flood_data.RData")

# Read in test points and results
indexTest <- readRDS("results/test_points.RDS")
nTest <- length(indexTest)
test_subj <- 10
which_test <- 5

flood_results_sketching <- readRDS("results/flood_results_sketching.RDS")
flood_results_nngp <- readRDS("results/flood_results_nngp.RDS")
flood_results_bass <- readRDS("results/flood_results_bass.RDS")
flood_results_bart <- readRDS("results/flood_results_bart.RDS")

estSketch <- flood_results_sketching$predictions[[which_test]][2, ]
estNNGP <- flood_results_nngp$preds
estBASS <- flood_results_bass$preds[which_test, ]
estBART <- flood_results_bart$preds[((which_test - 1) * nTest + 1):(which_test * nTest)]

lowest <- min(c(estSketch, estNNGP, estBASS, estBART))
highest <- max(c(estSketch, estNNGP, estBASS, estBART))
lims <- c(lowest, highest) * 1.035

# Surface plot for actual water level of storm 10 at all test points
trueY <- out[test_subj, indexTest]
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], trueY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/surf_truth.pdf")
par(cex = 1.5, oma = c(0, 0, 0, 0.75))
image.plot(pred.surf, xaxs ="r", yaxs = "r", main="", zlim = lims, col = hcl.colors(12, "terrain", rev=TRUE), 
	   legend.lab = "Water Level (meters)", legend.cex = 2, legend.mar = 5)
dev.off()

# Surface plot for estimated water level of storm 10 at testing data points (sketching) 
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estSketch), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/surfplots_flood.pdf")
par(cex = 1.5)
par(mfrow = c(2, 2))
image(pred.surf, xaxs ="r", yaxs = "r", main="", xlab = "Sketching", zlim = lims, col = hcl.colors(12, "terrain", rev = TRUE))

# Surface plot for NNGP 
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estNNGP), no.X=100, no.Y=100, extend=T)$xyz.est
image(pred.surf, xaxs ="r", yaxs = "r", main="", xlab = "NNGP", zlim = lims, col = hcl.colors(12, "terrain", rev = TRUE))

# Surface plot for BASS
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estBASS), no.X=100, no.Y=100, extend=T)$xyz.est
image(pred.surf, xaxs ="r", yaxs = "r", main="", xlab = "BASS", zlim = lims, col = hcl.colors(12, "terrain", rev = TRUE))

# Surface plot for BART
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estBART), no.X=100, no.Y=100, extend=T)$xyz.est
image(pred.surf, xaxs ="r", yaxs = "r", main="", xlab = "BART", zlim = lims, col = hcl.colors(12, "terrain", rev = TRUE))
dev.off()

# Density plots for posteriors of each parameter
sk <- readRDS("results/params.RDS")
sigma2Quants <- tau2Quants <- betaQuants <- gammaQuants <- numeric(100)
for (i in 1:100) {
  sigma2Quants[i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[1]], i / 100)))
  tau2Quants[i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[2]], i / 100)))
  betaQuants[i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][7, ], i / 100)))
  gammaQuants[i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][4, ], i / 100)))
}

pdf("../figures/densities.pdf")
par(mfrow = c(2, 2))
plot(density(sigma2Quants), 
     lwd = 2,
     main = "", ylab = "", xlab = "Sigma2", yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[1]], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red', lwd = 3)
abline(v = markers[c(1,3)], col = 'red', lwd = 3, lty = 2)

plot(density(tau2Quants), 
     lwd = 2,
     main = "", ylab = "", xlab = "Tau2", yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[2]], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red', lwd = 3)
abline(v = markers[c(1,3)], col = 'red', lwd = 3, lty = 2)

plot(density(betaQuants), 
     lwd = 2,
     main = "", ylab = "", xlab = "Beta", yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][7, ], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red', lwd = 3)
abline(v = markers[c(1,3)], col = 'red', lwd = 3, lty = 2)

plot(density(gammaQuants),
     lwd = 2,
     main = "", ylab = "", xlab = "Gamma (Min Velocity)", yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][4, ], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red', lwd = 3)
abline(v = markers[c(1,3)], col = 'red', lwd = 3, lty = 2)


dev.off()
