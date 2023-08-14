library(MBA)

# Read in
load("data/flood_data.RData")

# Add in elevation data
#elev_df <- readRDS("data/elev_data.RDS")
#elev_df$elev_meters[which(is.na(elev_df$elev_meters))] <- 0 # 3 NA's to get rid of

# Surface plot of elevation
pred.surf <-  mba.surf(coords, no.X=100, no.Y=100, extend=T)$xyz.est
pdf("figures/elevation.pdf")
image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
dev.off()

# Read in results and test points
flood_results <- readRDS("results/flood_final_results.RDS")
indices <- readRDS("results/data_split.RDS")
indexTrain <- indices[[1]]
indexTest <- indices[[2]]

# Surface plot for actual water level of storm 1 at all test points
trueY <- colMeans(out)[indexTest]
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], trueY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("figures/storm1_truth.pdf")
image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
dev.off()

# Surface plot for estimated water level of storm 1 at testing data points 
estY <- flood_results$predictions[2, ]
pred.surf <-  mba.surf(cbind(coords[indexTest, 1:2], estY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("figures/storm1_est.pdf")
image(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "terrain", rev = TRUE))
#points(coords[indexTest, 1:2], pch = 19, cex = 0.02)
dev.off()



