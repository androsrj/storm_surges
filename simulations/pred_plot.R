library(ggplot2)
library(ggpubr)
library(tidyr)

# Tables for paper
DC_full_gp <- readRDS("results/d_and_c/full_gp/final_results.RDS")
DC_mpp <- readRDS("results/d_and_c/mpp/final_results.RDS")
DC_sparse_gp <- readRDS("results/d_and_c/sparse_gp/final_results.RDS")
sketch_full_gp <- readRDS("results/sketching/full_gp/final_results.RDS")
sketch_mpp <- readRDS("results/sketching/mpp/final_results.RDS")
sketch_sparse_gp <- readRDS("results/sketching/sparse_gp/final_results.RDS")
load("data/test.RData")

# Check acceptance rates first
sapply(DC_full_gp, \(x) x$acc)
sapply(DC_mpp, \(x) x$acc)
sapply(DC_sparse_gp, \(x) x$acc)
sketch_full_gp$acc
sketch_mpp$acc
sketch_sparse_gp$acc

model <- rep(c("Full_GP", "MPP", "Sparse_GP"), each = 5)
splitType <- rep(c("Subdomains", "Stratified", "Multiplets", "Random", "Sketching"), 3)

#######################################################################

lowerPreds <- rbind(t(sapply(DC_full_gp, \(x) x$predictions[1, ])), 
                    sketch_full_gp$predictions[1, ],
                    t(sapply(DC_mpp, \(x) x$predictions[1, ])), 
                    sketch_mpp$predictions[1, ],
                    t(sapply(DC_sparse_gp, \(x) x$predictions[1, ])),
                    sketch_sparse_gp$predictions[1, ])

upperPreds <- rbind(t(sapply(DC_full_gp, \(x) x$predictions[3, ])), 
                    sketch_full_gp$predictions[3, ],
                    t(sapply(DC_mpp, \(x) x$predictions[3, ])), 
                    sketch_mpp$predictions[3, ],
                    t(sapply(DC_sparse_gp, \(x) x$predictions[3, ])),
                    sketch_sparse_gp$predictions[3, ])

pointPreds <- rbind(t(sapply(DC_full_gp, \(x) x$predictions[2, ])), 
                    sketch_full_gp$predictions[2, ],
                    t(sapply(DC_mpp, \(x) x$predictions[2, ])), 
                    sketch_mpp$predictions[2, ],
                    t(sapply(DC_sparse_gp, \(x) x$predictions[2, ])),
                    sketch_sparse_gp$predictions[2, ])

test_subj <- 1
MSPE <- apply(pointPreds, 1, \(x) mean((x - test$Y[[test_subj]])^2))
cvg_ind <- sapply(1:15, \(i) lowerPreds[i, ] <= test$Y[[test_subj]] & upperPreds[i, ] >= test$Y[[test_subj]])
coverage <- apply(cvg_ind, 2, mean)
length <- apply(upperPreds - lowerPreds, 1, mean)
a <- .05
score <- sapply(1:15, \(i) mean( (upperPreds[i,] - lowerPreds[i,]) + 2/a * (lowerPreds[i,] - test$Y[[test_subj]]) * (test$Y[[test_subj]] < lowerPreds[i,]) + 2/a * (test$Y[[test_subj]] - upperPreds[i,]) * (test$Y[[test_subj]] > upperPreds[i,]) ) )

df <- data.frame(model, splitType, MSPE, score)

full_gp <- gather(df[1:5, ], metric, value, MSPE:score)
mpp <- gather(df[6:10, ], metric, value, MSPE:score)
sparse_gp <- gather(df[11:15, ], metric, value, MSPE:score)

plot_f <- ggplot(full_gp, aes(splitType, value, fill = metric)) + 
  geom_bar(stat = "identity", 
           position = 'dodge', 
           colour = 'black') + 
  guides(fill=guide_legend(title = NULL)) +
  scale_fill_discrete(labels = c('MSPE', 'Interval Score')) +
  labs(x = "",y = "") + 
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(legend.position="none") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.text = element_text(size = 12), legend.text=element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Full GP")

plot_m <- ggplot(mpp, aes(splitType, value, fill = metric)) + 
  geom_bar(stat = "identity", 
           position = 'dodge', 
           colour = 'black') + 
  guides(fill=guide_legend(title = NULL)) +
  scale_fill_discrete(labels = c('MSPE', 'Interval Score')) +
  labs(x = "",y = "") + 
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(legend.position="none") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.text = element_text(size = 12), legend.text=element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("MPP")

plot_s <- ggplot(sparse_gp, aes(splitType, value, fill = metric)) + 
  geom_bar(stat = "identity", 
           position = 'dodge', 
           colour = 'black') + 
  guides(fill=guide_legend(title = NULL)) +
  scale_fill_discrete(labels = c('MSPE', 'Interval Score')) +
  labs(x = "",y = "") + 
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.text = element_text(size = 12), legend.text=element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Sparse GP")

pdf("../figures/pred_diagnostics.pdf")
ggarrange(plot_f, plot_m, plot_s, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()




