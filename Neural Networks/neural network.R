source("Neural Networks/Neural Net Setup.R")

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')



nnetMod <- nnet(train.active ~ train$Trans9 + train$Trans10, 
                size = 3, 
                decay = 0.1)

ctrl <- trainControl(summaryFunction= twoClassSummary,
                     classProbs = TRUE)

nnetGrid <- expand.grid(.size = 1:10, 
                        .decay = c(0,.1,1,2))

numWts <- 5 * (ncol(train + 1) + 5 + 1)

nnetBestFit

vars = which(names(train) %in% c("Trans9","Trans10","Trans11","Cust_Tenure","Trans4","Food163"
                                 ,"Trans41","Food156","Trans3","Food62","Food152","Food164"
                                 ,"Food147","Food58","Food155","Trans7"))

subTrain <- train[,vars]

nnetFit <- train(x = subTrain, 
                 y = train.active,
                 method="nnet",
                 metric="ROC",
                 preProcess = c("center","scale","spatialSign"),
                 tuneGrid = nnetGrid,
                 trace = TRUE,
                 maxit = 2000,
                 MaxNWts = numWts, 
                 trControl = ctrl)

summary(nnetFit)
plot.nnet(nnetFit)

save(nnetFit,file="Neural Networks/NNetModTunedFit.RData")



nnetFit$bestTune

fullTrain <- subTrain
fullTrain$active <- train.active

fullTrain$active <- fullTrain.active

nnetBestFit <- nnet(fullTrain$active ~.,
                    data = fullTrain,
                   size = 7, 
                   decay = 1)

save(nnetBestFit,file="Neural Networks/NNetModBestFit.RData")

plot.nnet(nnetBestFit)
summary(nnetBestFit)

nnetPreds <- predict(nnetBestFit,test,type="class")
NeuralNetPreds <- as.data.frame(cbind(test.id, nnetPreds))

write.csv(NeuralNetPreds,file="Neural Networks/May 4 Prediction.csv")


