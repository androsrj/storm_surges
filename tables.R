sketching <- readRDS("results/flood_results_sketching.RDS")
subdomains <- readRDS("results/flood_results_subdomains.RDS")
stratified <- readRDS("results/flood_results_stratified.RDS")
data_split <- readRDS("results/data_split.RDS")
indexTest <- data_split[[2]]
load("data/flood_data.RData")
trueTest <- out[1, indexTest]

# Length
mean(sketching$predictions[3,] - sketching$predictions[1,])
mean(subdomains$predictions[3,] - subdomains$predictions[1,])
mean(stratified$predictions[3,] - stratified$predictions[1,])

# Coverage
mean(sketching$predictions[3,] > trueTest & sketching$predictions[1,] < trueTest)
mean(subdomains$predictions[3,] > trueTest & subdomains$predictions[1,] < trueTest)
mean(stratified$predictions[3,] > trueTest & stratified$predictions[1,] < trueTest)
