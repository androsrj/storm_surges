# SURFACE PLOTS USING MBA PACKAGE
library(MBA)
library(fields)
load("data/test.RData")
test_subj <- 1

# True spatial surface (subject 1)
trueY <- test$Y[[test_subj]]
coords <- test$S
pred.surf <-  mba.surf(cbind(coords, trueY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/simulations/true_surface.pdf")
par(cex = 1.3)
image.plot(pred.surf, xaxs ="r", yaxs = "r", main="", col = hcl.colors(12, "YlOrRd", rev=TRUE))
contour(pred.surf, add=T)
dev.off()

# Estimated spatial surface (D-and-C, full GP, stratified split)
# Passes in same coordinates (S) from original data
full_gp <- readRDS("results/d_and_c/full_gp_results.RDS")[[2]]
estY <- full_gp$predictions[[test_subj]][2, ]
pred.surf <-  mba.surf(cbind(coords, estY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/simulations/dc_full_gp.pdf")
par(cex = 1.2)
image(pred.surf, xaxs ="r", yaxs = "r", main="")
contour(pred.surf, add=T)
dev.off()

# Estimated spatial surface (D-and-C, MPP, stratified split)
# Passes in same coordinates (S) from original data
mpp <- readRDS("results/d_and_c/mpp_results.RDS")[[2]]
estY <- mpp$predictions[[test_subj]][2, ]
pred.surf <-  mba.surf(cbind(coords, estY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/simulations/dc_mpp.pdf")
par(cex = 1.2)
image(pred.surf, xaxs ="r", yaxs = "r", main="")
contour(pred.surf, add=T)
dev.off()

# Estimated spatial surface (D-and-C, sparse GP, stratified split)
# Passes in same coordinates (S) from original data
sparse_gp <- readRDS("results/d_and_c/sparse_gp_results.RDS")[[2]]
estY <- sparse_gp$predictions[[test_subj]][2, ]
pred.surf <-  mba.surf(cbind(coords, estY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/simulations/dc_sparse_gp.pdf")
par(cex = 1.2)
image(pred.surf, xaxs ="r", yaxs = "r", main="")
contour(pred.surf, add=T)
dev.off()


# Estimated spatial surface (Sketching, full GP)
# Passes in same coordinates (S) from original data
full_gp <- readRDS("results/sketching/full_gp_results.RDS")
estY <- full_gp$predictions[[test_subj]][2, ]
pred.surf <-  mba.surf(cbind(coords, estY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/simulations/sketching_full_gp.pdf")
par(cex = 1.2)
image(pred.surf, xaxs ="r", yaxs = "r", main="")
contour(pred.surf, add=T)
dev.off()

# Estimated spatial surface (Sketching, MPP)
# Passes in same coordinates (S) from original data
mpp <- readRDS("results/sketching/mpp_results.RDS")
estY <- mpp$predictions[[test_subj]][2, ]
pred.surf <-  mba.surf(cbind(coords, estY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/simulations/sketching_mpp.pdf")
par(cex = 1.2)
image(pred.surf, xaxs ="r", yaxs = "r", main="")
contour(pred.surf, add=T)
dev.off()

# Estimated spatial surface (Sketching, sparse GP)
# Passes in same coordinates (S) from original data
sparse_gp <- readRDS("results/sketching/sparse_gp_results.RDS")
estY <- sparse_gp$predictions[[test_subj]][2, ]
pred.surf <-  mba.surf(cbind(coords, estY), no.X=100, no.Y=100, extend=T)$xyz.est
pdf("../figures/simulations/sketching_sparse_gp.pdf")
par(cex = 1.2)
image(pred.surf, xaxs ="r", yaxs = "r", main="")
contour(pred.surf, add=T)
dev.off()
