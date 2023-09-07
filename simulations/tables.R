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

sigma2Mean <- unname(c(sapply(DC_full_gp, \(x) x$means[1]), 
                       sketch_full_gp$means[1],
                       sapply(DC_mpp, \(x) x$means[1]),
                       sketch_mpp$means[1],
                       sapply(DC_sparse_gp, \(x) x$means[1]),
                       sketch_sparse_gp$means[1]))

sigma2Lower <- unname(c(sapply(DC_full_gp, \(x) x$lower[1]), 
                        sketch_full_gp$lower[1],
                        sapply(DC_mpp, \(x) x$lower[1]),
                        sketch_mpp$lower[1],
                        sapply(DC_sparse_gp, \(x) x$lower[1]),
                        sketch_sparse_gp$lower[1]))

sigma2Upper <- unname(c(sapply(DC_full_gp, \(x) x$upper[1]), 
                        sketch_full_gp$upper[1],
                        sapply(DC_mpp, \(x) x$upper[1]),
                        sketch_mpp$upper[1],
                        sapply(DC_sparse_gp, \(x) x$upper[1]),
                        sketch_sparse_gp$upper[1]))

#######################################################################

tau2Mean <- unname(c(sapply(DC_full_gp, \(x) x$means[2]), 
                     sketch_full_gp$means[2],
                     sapply(DC_mpp, \(x) x$means[2]),
                     sketch_mpp$means[2],
                     sapply(DC_sparse_gp, \(x) x$means[2]),
                     sketch_sparse_gp$means[2]))

tau2Lower <- unname(c(sapply(DC_full_gp, \(x) x$lower[2]), 
                      sketch_full_gp$lower[2],
                      sapply(DC_mpp, \(x) x$lower[2]),
                      sketch_mpp$lower[2],
                      sapply(DC_sparse_gp, \(x) x$lower[2]),
                      sketch_sparse_gp$lower[2]))

tau2Upper <- unname(c(sapply(DC_full_gp, \(x) x$upper[2]), 
                      sketch_full_gp$upper[2],
                      sapply(DC_mpp, \(x) x$upper[2]),
                      sketch_mpp$upper[2],
                      sapply(DC_sparse_gp, \(x) x$upper[2]),
                      sketch_sparse_gp$upper[2]))

#######################################################################

gammaMean <- unname(c(sapply(DC_full_gp, \(x) x$means[3]), 
                      sketch_full_gp$means[3],
                      sapply(DC_mpp, \(x) x$means[3]),
                      sketch_mpp$means[3],
                      sapply(DC_sparse_gp, \(x) x$means[3]),
                      sketch_sparse_gp$means[3]))

gammaLower <- unname(c(sapply(DC_full_gp, \(x) x$lower[3]), 
                       sketch_full_gp$lower[3],
                       sapply(DC_mpp, \(x) x$lower[3]),
                       sketch_mpp$lower[3],
                       sapply(DC_sparse_gp, \(x) x$lower[3]),
                       sketch_sparse_gp$lower[3]))

gammaUpper <- unname(c(sapply(DC_full_gp, \(x) x$upper[3]), 
                       sketch_full_gp$upper[3],
                       sapply(DC_mpp, \(x) x$upper[3]),
                       sketch_mpp$upper[3],
                       sapply(DC_sparse_gp, \(x) x$upper[3]),
                       sketch_sparse_gp$upper[3]))

#######################################################################

beta1Mean <- unname(c(sapply(DC_full_gp, \(x) x$means[4]), 
                      sketch_full_gp$means[4],
                      sapply(DC_mpp, \(x) x$means[4]),
                      sketch_mpp$means[4],
                      sapply(DC_sparse_gp, \(x) x$means[4]),
                      sketch_sparse_gp$means[4]))

beta1Lower <- unname(c(sapply(DC_full_gp, \(x) x$lower[4]), 
                       sketch_full_gp$lower[4],
                       sapply(DC_mpp, \(x) x$lower[4]),
                       sketch_mpp$lower[4],
                       sapply(DC_sparse_gp, \(x) x$lower[4]),
                       sketch_sparse_gp$lower[4]))

beta1Upper <- unname(c(sapply(DC_full_gp, \(x) x$upper[4]), 
                       sketch_full_gp$upper[4],
                       sapply(DC_mpp, \(x) x$upper[4]),
                       sketch_mpp$upper[4],
                       sapply(DC_sparse_gp, \(x) x$upper[4]),
                       sketch_sparse_gp$upper[4]))

#######################################################################

beta2Mean <- unname(c(sapply(DC_full_gp, \(x) x$means[5]), 
                      sketch_full_gp$means[5],
                      sapply(DC_mpp, \(x) x$means[5]),
                      sketch_mpp$means[5],
                      sapply(DC_sparse_gp, \(x) x$means[5]),
                      sketch_sparse_gp$means[5]))

beta2Lower <- unname(c(sapply(DC_full_gp, \(x) x$lower[5]), 
                       sketch_full_gp$lower[5],
                       sapply(DC_mpp, \(x) x$lower[5]),
                       sketch_mpp$lower[5],
                       sapply(DC_sparse_gp, \(x) x$lower[5]),
                       sketch_sparse_gp$lower[5]))

beta2Upper <- unname(c(sapply(DC_full_gp, \(x) x$upper[5]), 
                       sketch_full_gp$upper[5],
                       sapply(DC_mpp, \(x) x$upper[5]),
                       sketch_mpp$upper[5],
                       sapply(DC_sparse_gp, \(x) x$upper[5]),
                       sketch_sparse_gp$upper[5]))

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

df <- data.frame(model, splitType,
                 sigma2Mean, sigma2Lower, sigma2Upper,
                 tau2Mean, tau2Lower, tau2Upper,
                 beta1Mean, beta1Lower, beta1Upper,
                 beta2Mean, beta2Lower, beta2Upper,
		 gammaMean, gammaLower, gammaUpper,
                 MSPE, coverage, length, score)

df
