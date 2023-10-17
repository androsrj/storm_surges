library(BASS)
load("data/train.RData")
#load("data/test.RData")
holdout.index <- 1:2
train.index <- 3:10
simInputs <- matrix(sapply(train.index, \(i) train$X[[i]][1,1]), ncol=1)
simOutputs <- t(sapply(train.index, \(i) train$Y[[i]]))
simModel <- bassPCA(simInputs, simOutputs, n.pc=5, n.cores=1)
testInputs <- matrix(sapply(holdout.index, \(i) train$X[[i]][1,1]), ncol=1)
simPredictions <- predict(simModel, testInputs)

simPreds <- apply(simPredictions, 2:3, mean)
mspe <- sapply(holdout.index, \(i) mean((simPreds[i,] - train$Y[[i]])^2) )
mean(mspe)

pct <- sapply(holdout.index, \(i) mean((simPreds[i,] > 4) == (train$Y[[i]] > 4)) )
1-mean(pct)

simPreds[1,][1:25]
train$Y[[1]][1:25]
