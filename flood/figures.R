library(MBA)
library(fields)
library(latex2exp)

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
par(cex.lab = 2, oma = c(0, 0, 0, 0.75))
image.plot(pred.surf, xaxs ="r", yaxs = "r", main="", xlab = "Truth", zlim = lims, col = hcl.colors(12, "terrain", rev=TRUE), 
	   legend.lab = "Water Level (meters)", legend.cex = 2, legend.mar = 5)
dev.off()

# Surface plot for estimated water level of storm 10 at testing data points (sketching) 
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estSketch), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/surfplots_flood.pdf")
par(cex.lab = 2)
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
sigma2Quants <- tau2Quants <- betaQuants <- numeric(100)
gammaQuants <- matrix(0, nrow = 5, ncol = 100)
for (i in 1:100) {
  sigma2Quants[i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[1]], i / 100)))
  tau2Quants[i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[2]], i / 100)))
  betaQuants[i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][7, ], i / 100)))
  gammaQuants[1, i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][2, ], i / 100)))
  gammaQuants[2, i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][3, ], i / 100)))
  gammaQuants[3, i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][4, ], i / 100)))
  gammaQuants[4, i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][5, ], i / 100)))
  gammaQuants[5, i] <- mean(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][6, ], i / 100)))
}

pdf("../figures/densities.pdf", width = 9, height = 5)
par(mfrow = c(2, 4))
plot(density(sigma2Quants), 
     lwd = 2,
     main = "", ylab = "", xlab = TeX(r'($\sigma^2$)'), yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[1]], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red4', lwd = 3)
abline(v = markers[c(1,3)], col = 'red4', lwd = 3, lty = 2)

plot(density(tau2Quants), 
     lwd = 2,
     main = "", ylab = "", xlab = TeX(r'($\tau^2$)'), yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[2]], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red4', lwd = 3)
abline(v = markers[c(1,3)], col = 'red4', lwd = 3, lty = 2)

plot(density(betaQuants), 
     lwd = 2,
     main = "", ylab = "", xlab = TeX(r'($\beta$ (Elevation))'), yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][7, ], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red4', lwd = 3)
abline(v = markers[c(1,3)], col = 'red4', lwd = 3, lty = 2)

plot(density(gammaQuants[1, ]),
     lwd = 2,
     main = "", ylab = "", xlab = TeX(r'($\gamma_1$ (Sea Level Rise))'), yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][2, ], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red4', lwd = 3)
abline(v = markers[c(1,3)], col = 'red4', lwd = 3, lty = 2)

plot(density(gammaQuants[2, ]),
     lwd = 2,
     main = "", ylab = "", xlab = TeX(r'($\gamma_2$ (Direction))'), yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][3, ], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red4', lwd = 3)
abline(v = markers[c(1,3)], col = 'red4', lwd = 3, lty = 2)

plot(density(gammaQuants[3, ]),
     lwd = 2,
     main = "", ylab = "", xlab = TeX(r'($\gamma_3$ (Min Velocity))'), yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][4, ], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red4', lwd = 3)
abline(v = markers[c(1,3)], col = 'red4', lwd = 3, lty = 2)

plot(density(gammaQuants[4, ]),
     lwd = 2,
     main = "", ylab = "", xlab =  TeX(r'($\gamma_4$ (Min Air Pressure))'), yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][5, ], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red4', lwd = 3)
abline(v = markers[c(1,3)], col = 'red4', lwd = 3, lty = 2)

plot(density(gammaQuants[5, ]),
     lwd = 2,
     main = "", ylab = "", xlab =  TeX(r'($\gamma_5$ (Latitude))'), yaxt = 'n',
     cex.lab = 1.5, cex.axis =  1.5)
markers <- apply(sapply(1:10, \(j) quantile(sk[[j]]$paramSamples[[3]][6, ], c(.025, .5, .975))), 1, mean)
abline(v = markers[2], col = 'red4', lwd = 3)
abline(v = markers[c(1,3)], col = 'red4', lwd = 3, lty = 2)


dev.off()
