library(ggplot2)
#library(ggOceanMaps)
library(interp)
library(gridExtra)
library(latex2exp)

# Read in data, results, and test points
load("data/flood_data.RData")
sketching <- readRDS("results/flood_results_sketching.RDS")
nngp <- readRDS("results/flood_results_nngp.RDS")
bass <- readRDS("results/flood_results_bass.RDS")
indexTest <- readRDS("results/test_points.RDS")

# Test subject is storm 10
test_subjects <- 6:10
which_test <- 5
test_subj <- test_subjects[which_test]
lims <- c(0, 6.7)

#### TRUE WATER LEVELS ####
df_truth <- data.frame(x = coords$x, y = coords$y, z = out[test_subj,])
df_truth <- df_truth[indexTest,]
interp_truth <- interp(df_truth$x, df_truth$y, df_truth$z, nx = 200, ny = 200) |> 
  interp2xyz() |> 
  as.data.frame()

#pts <- dist2land(interp_truth) # Requires ggOceanMap library
pts <- readRDS("results/ocean_pts.RDS")
truth_plot <- pts
truth_plot[pts$ldist > 0.25 & pts$x < -74.85, "z"] <- NA

ggplot(data = truth_plot, aes(x, y)) +
  geom_raster(aes(fill = z)) +
  scale_fill_distiller(palette = "Spectral", na.value = NA, limits = lims) + 
  theme_classic() +
  labs(fill = "Water \nLevel (m)", x = "Truth") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.75, "cm"),
        axis.ticks.length=unit(.3, "cm"),
        panel.background = element_rect(colour = "black", linewidth = 1))


#### SKETCHING ####
df_sketching <- data.frame(x = coords$x[indexTest], 
                           y = coords$y[indexTest], 
                           z = sketching$predictions[[which_test]][2,])
interp_sketching <- interp(df_sketching$x, df_sketching$y, df_sketching$z, nx = 200, ny = 200) |> 
  interp2xyz() |> 
  as.data.frame()
sketching_plot <- interp_sketching
sketching_plot[pts$ldist > 0.25 & pts$x < -74.85, "z"] <- NA
sketching_plot$z[sketching_plot$z < 0] <- 0

#### NNGP ####
df_nngp <- data.frame(x = coords$x[indexTest], 
                      y = coords$y[indexTest], 
                      z = nngp$preds)
interp_nngp <- interp(df_nngp$x, df_nngp$y, df_nngp$z, nx = 200, ny = 200) |> 
  interp2xyz() |> 
  as.data.frame()
nngp_plot <- interp_nngp
nngp_plot[pts$ldist > 0.25 & pts$x < -74.85, "z"] <- NA
nngp_plot$z[nngp_plot$z < 0] <- 0

#### BASS ####
df_bass <- data.frame(x = coords$x[indexTest], 
                      y = coords$y[indexTest], 
                      z = bass$preds[which_test,])
interp_bass <- interp(df_bass$x, df_bass$y, df_bass$z, nx = 200, ny = 200) |> 
  interp2xyz() |> 
  as.data.frame()
bass_plot <- interp_bass
bass_plot[pts$ldist > 0.25 & pts$x < -74.85, "z"] <- NA
bass_plot$z[bass_plot$z < 0] <- 0


#### PLOTS ####

ggplot(data = truth_plot, aes(x, y)) +
  geom_raster(aes(fill = z)) +
  scale_fill_distiller(palette = "Spectral", na.value = NA, limits = lims) + 
  theme_classic() +
  labs(fill = "Water \nLevel (m)", x = "Truth") +
  theme(axis.title.x = element_text(size = 35, margin = margin(t = 15)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.75, "cm"),
        axis.ticks.length=unit(.3, "cm"),
        panel.background = element_rect(colour = "black", linewidth = 1))
ggsave(filename = "../figures/surf_truth.pdf", height = 5)

p1 <- ggplot(data = sketching_plot, aes(x, y)) +
  geom_raster(aes(fill = z)) +
  scale_fill_distiller(palette = "Spectral", na.value = NA, limits = lims) + 
  theme_classic() +
  labs(fill = "Water \nLevel (m)", x = "Sketching") +
  theme(axis.title.x = element_text(size = 25, margin = margin(t = 15)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none",
        axis.ticks.length=unit(.3, "cm"),
        panel.background = element_rect(colour = "black", linewidth = 1))

p2 <- ggplot(data = nngp_plot, aes(x, y)) +
  geom_raster(aes(fill = z)) +
  scale_fill_distiller(palette = "Spectral", na.value = NA, limits = lims) + 
  theme_classic() +
  labs(fill = "Water \nLevel (m)", x = "NNGP") +
  theme(axis.title.x = element_text(size = 25, margin = margin(t = 15)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none",
        axis.ticks.length=unit(.3, "cm"),
        panel.background = element_rect(colour = "black", linewidth = 1))

p3 <- ggplot(data = bass_plot, aes(x, y)) +
  geom_raster(aes(fill = z)) +
  scale_fill_distiller(palette = "Spectral", na.value = NA, limits = lims) + 
  theme_classic() +
  labs(fill = "Water \nLevel (m)", x = "BASS") +
  theme(axis.title.x = element_text(size = 25, margin = margin(t = 15)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none",
        axis.ticks.length=unit(.3, "cm"),
        panel.background = element_rect(colour = "black", linewidth = 1))

pdf("../figures/surfplots_flood.pdf", width=10, height=4)
grid.arrange(p1, p2, p3, ncol=3)
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
