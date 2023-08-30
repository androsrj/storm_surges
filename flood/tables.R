sketching <- readRDS("results/flood_results_sketching.RDS")
subdomains <- readRDS("results/flood_results_subdomains.RDS")
stratified <- readRDS("results/flood_results_stratified.RDS")
data_split <- readRDS("results/data_split.RDS")
indexTest <- data_split[[2]]
load("data/flood_data.RData")
trueTest <- out[1, indexTest]

# Parameter estimates
sigma2 <- c(sketching$means['sigma2'], 
	    subdomains$means['sigma2'],
	    stratified$means['sigma2'])
tau2 <- c(sketching$means['tau2'], 
	  subdomains$means['tau2'],
	  stratified$means['tau2'])
beta <- c(sketching$means['beta7'], 
	  subdomains$means['beta7'],
	  stratified$means['beta7'])

# Lower bounds
sigma2Lwr <- c(sketching$lower[1], 
	       subdomains$lower[1],
	       stratified$lower[1])
tau2Lwr <- c(sketching$lower[2], 
	     subdomains$lower[2],
	     stratified$lower[2])
betaLwr <- c(sketching$lower[9], 
	     subdomains$lower[9],
	     stratified$lower[9])

# Upper bounds
sigma2Upr <- c(sketching$upper[1], 
	       subdomains$upper[1],
	       stratified$upper[1])
tau2Upr <- c(sketching$upper[2], 
	     subdomains$upper[2],
	     stratified$upper[2])
betaUpr <- c(sketching$upper[9], 
	     subdomains$upper[9],
	     stratified$upper[9])

# Length
length <- c(mean(sketching$predictions[3,] - sketching$predictions[1,]),
	    mean(subdomains$predictions[3,] - subdomains$predictions[1,]),
	    mean(stratified$predictions[3,] - stratified$predictions[1,]))

# Coverage
cvg <- c(mean(sketching$predictions[3,] > trueTest & sketching$predictions[1,] < trueTest),
	 mean(subdomains$predictions[3,] > trueTest & subdomains$predictions[1,] < trueTest),
	 mean(stratified$predictions[3,] > trueTest & stratified$predictions[1,] < trueTest))

# MSPE
mspe <- c(mean((sketching$predictions[2,] - trueTest)^2), 
	  mean((subdomains$predictions[2,] - trueTest)^2),
	  mean((stratified$predictions[2,] - trueTest)^2))

# Final results table
final_df <- data.frame(sigma2, sigma2Lwr, sigma2Upr, 
		       tau2, tau2Lwr, tau2Upr, 
		       beta, betaLwr, betaUpr, 
		       mspe, cvg, length)
rownames(final_df) <- c("sketching", "subdomains", "stratified")
final_df

# Also make sure that acceptance rates are satisfactory
sketching$acc
subdomains$acc
stratified$acc
