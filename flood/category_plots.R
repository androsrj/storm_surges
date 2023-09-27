library(MBA)
library(fields)
library(plotrix)

# Read in
load("data/flood_data.RData")

# Read in test points
indices <- readRDS("results/data_split.RDS")
indexTest <- indices[[2]]
test_subjects <- 1:10
trueY <- colMeans(out)[indexTest]
slowStorms <- which(inputs$v[test_subjects] < 25)
fastStorms <- which(inputs$v[test_subjects] > 25)

allSketch <- rnorm(length(trueY))
slowSketch <- rnorm(length(trueY))
fastSketch <- rnorm(length(trueY))

allBART <- rnorm(length(trueY))
slowBART <- rnorm(length(trueY))
fastBART <- rnorm(length(trueY))


pdf("temp_plots.pdf", width = 10)
par(mfrow = c(2,3), oma = c(0, 1, 0, 2), cex.lab = 1.5)

# All storms, sketching
surf <-  mba.surf(cbind(coords[indexTest, 1:2], trueY - allSketch), no.X=100, no.Y=100, extend=T)$xyz.est
image(surf, xaxs ="r", yaxs = "r", main="All Storms", ylab = "Sketching",
           col = hcl.colors(12, "terrain", rev=TRUE))

# Slow storms, sketching
surf <-  mba.surf(cbind(coords[indexTest, 1:2], trueY - slowSketch), no.X=100, no.Y=100, extend=T)$xyz.est
image(surf, xaxs ="r", yaxs = "r", main="Slow-Moving Storms", 
           col = hcl.colors(12, "terrain", rev=TRUE))

# Fast storms, sketching
surf <-  mba.surf(cbind(coords[indexTest, 1:2], trueY - fastSketch), no.X=100, no.Y=100, extend=T)$xyz.est
image.plot(surf, xaxs ="r", yaxs = "r", main="Fast-Moving Storms", 
           col = hcl.colors(12, "terrain", rev=TRUE), 
           legend.lab = "Residual (meters)", legend.cex = 0.75)
 
# All storms, BART
surf <-  mba.surf(cbind(coords[indexTest, 1:2], trueY - allBART), no.X=100, no.Y=100, extend=T)$xyz.est
image(surf, xaxs ="r", yaxs = "r", main="", ylab = "BART",
      col = hcl.colors(12, "terrain", rev=TRUE))

# Slow storms, BART
surf <-  mba.surf(cbind(coords[indexTest, 1:2], trueY - slowBART), no.X=100, no.Y=100, extend=T)$xyz.est
image(surf, xaxs ="r", yaxs = "r", main="", 
      col = hcl.colors(12, "terrain", rev=TRUE))

# Fast storms, BART
surf <-  mba.surf(cbind(coords[indexTest, 1:2], trueY - fastBART), no.X=100, no.Y=100, extend=T)$xyz.est
image.plot(surf, xaxs ="r", yaxs = "r", main="", 
           col = hcl.colors(12, "terrain", rev=TRUE), 
           legend.lab = "Residual (meters)", legend.cex = 0.75)

dev.off()

