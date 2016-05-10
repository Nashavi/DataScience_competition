source("LDA Modeling/FDA Setup.R")



train$active <- cbind(train.active)
levels(train$active) <- c("No","Yes")

fdaModel <- fda(active ~ .,
                data = train,
                method=earth)

summary(fdaModel$fit)

preds <- predict(fdaModel, eval)

eval.active <- as.factor(eval.active)
levels(eval.active) <- c("No","Yes")

confusion(preds,eval.active)
#1 - .3219829

varImp(fdaModel)





test.preds <- predict(fdaModel,test,type="class")

write.csv(test.preds,file="LDA Modeling/FDA Preds.csv")

active_customer <- train$active
active_customer <- as.factor(active_customer)
levels(active_customer) <- c("No","Yes")
train <- train[,-261]



fitControl = trainControl(method='CV', #use cross validation
                          number=5, #set the number of folds
                          summaryFunction = twoClassSummary, #use two-class classification
                          classProbs = TRUE) #return probabilities

fdaTuneGrid <- expand.grid(degree = c(2,3), 
                           nprune = seq(10,40,by=10))

fdaTuned <- train(x = train,
                  y = active_customer, 
                  method="fda", 
                  metric="ROC",
                  trControl = fitControl,
                  tuneGrid = fdaTuneGrid)

plot(fdaTuned)
save(fdaTuned,file="LDA Modeling/FDAModelCVTuned.RData")

fdaTuned$bestTune

train <- cbind(train,active_customer)

fdaModel <- fda(active_customer ~ .,
                 data = train,
                degree = 2,
                nprune = 30,
                 method=earth)

test.preds <- predict(fdaModel,test,type="class")

write.csv(test.preds,file="LDA Modeling/FDA CV Preds.csv")


