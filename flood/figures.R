library(MBA)
library(fields)

# Read in
load("data/flood_data.RData")

# Read in test points
indexTest <- readRDS("results/test_points.RDS")
nTest <- length(indexTest)
test_subj <- 10
which_test <- 5

# Surface plot for actual water level of storm 10 at all test points
trueY <- out[test_subj, indexTest]
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], trueY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/surf_truth.pdf")
par(cex = 1.5, oma = c(0, 0, 0, 0.75))
image.plot(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev=TRUE), 
	   legend.lab = "Water Level (meters)", legend.cex = 2, legend.mar = 5)
dev.off()

# Surface plot for estimated water level of storm 10 at testing data points (sketching) 
flood_results_sketching <- readRDS("results/flood_results_sketching.RDS")
estY <- flood_results_sketching$predictions[[which_test]][2, ]
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estY), no.X=100, no.Y=100, extend=T)$xyz.est
par(cex = 1.5)
pdf("../figures/surf_sketching.pdf")
image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
dev.off()

# Surface plot for NNGP 
flood_results_nngp <- readRDS("results/flood_results_nngp.RDS")
estY <- flood_results_nngp$preds
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estY), no.X=100, no.Y=100, extend=T)$xyz.est
par(cex = 1.5)
pdf("../figures/surf_nngp.pdf")
image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
dev.off()

# Surface plot for BASS
flood_results_bass <- readRDS("results/flood_results_bass.RDS")
estY <- flood_results_bass$preds[which_test, ]
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estY), no.X=100, no.Y=100, extend=T)$xyz.est
par(cex = 1.5)
pdf("../figures/surf_bass.pdf")
image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
dev.off()

# Surface plot for BART
flood_results_bart <- readRDS("results/flood_results_bart.RDS")
estY <- flood_results_bart$preds[((which_test - 1) * nTest + 1):(which_test * nTest)]
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estY), no.X=100, no.Y=100, extend=T)$xyz.est
par(cex = 1.5)
pdf("../figures/surf_bart.pdf")
image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
dev.off()

