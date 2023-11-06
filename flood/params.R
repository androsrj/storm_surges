obj <- readRDS("results/params.RDS")
storms <- 1:5
sigma2 <- lapply(1:length(storms), \(i) obj[[i]]$paramSamples$sigma2)
length(sigma2)
dim(sigma2)
