library(MBA)
library(fields)

# Read in
load("data/flood_data.RData")

# Read in test points
indices <- readRDS("results/data_split.RDS")
indexTest <- indices[[2]]
test_subj <- 1

# Surface plot for actual water level of storm 1 at all test points
trueY <- colMeans(out)[indexTest]
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], trueY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/storm1_truth.pdf")
par(cex = 1.5, oma = c(0, 0, 0, 0.75))
image.plot(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev=TRUE), 
	   legend.lab = "Water Level (meters)", legend.cex = 2, legend.mar = 5)
dev.off()

# Surface plot for estimated water level of storm 1 at testing data points (sketching) 
flood_results_sketching <- readRDS("results/flood_results_sketching.RDS")
estY <- flood_results_sketching$predictions[[test_subj]][2, ]
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estY), no.X=100, no.Y=100, extend=T)$xyz.est
par(cex = 1.5)
pdf("../figures/storm1_sketching.pdf")
image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
dev.off()


# Surface plot for estimated water level of storm 1 at testing data points (subdomains) 
#flood_results_subdomains <- readRDS("results/flood_results_subdomains.RDS")
#estY <- flood_results_subdomains$predictions[[test_subj]][2, ]
#pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estY), no.X=100, no.Y=100, extend=T)$xyz.est
#par(cex = 1.5)
#pdf("../figures/storm1_subdomains.pdf")
#image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
#dev.off()


# Surface plot for estimated water level of storm 1 at testing data points (stratified) 
#flood_results_stratified <- readRDS("results/flood_results_stratified.RDS")
#estY <- flood_results_stratified$predictions[[test_subj]][2, ]
#pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estY), no.X=100, no.Y=100, extend=T)$xyz.est
#par(cex = 1.5)
#pdf("../figures/storm1_stratified.pdf")
#image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
#dev.off()

# Surface plot for BART
flood_results_bart <- readRDS("results/flood_results_bart.RDS")
estY <- flood_results_bart[[test_subj]]$yhat.test.mean
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estY), no.X=100, no.Y=100, extend=T)$xyz.est
par(cex = 1.5)
pdf("../figures/storm1_bart.pdf")
image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
dev.off()

