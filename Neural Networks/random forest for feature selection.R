
library(randomForest)

p <- 260

p = sqrt(p)

rfTrain <- cbind(train,train.active)

rf.mod = randomForest(train.active~.,data=rfTrain,mtry=p,importance=TRUE,type="prob")

save(rf.mod,file="Neural Networks/RF Feature Selection Model.RData")

print(rf.mod)

varImpPlot(rf.mod,main="Random Forest Importance Plot")
importance(rf.mod)