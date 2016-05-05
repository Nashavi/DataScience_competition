source("LDA Modeling/FDA Setup.R")


train <- cbind(train,train.active)

train$train.active <- as.factor(train$train.active)
levels(train$train.active) <- c("No","Yes")

fdaModel <- fda(train.active ~ .,
                data = train,
                method=earth)

summary(fdaModel$fit)

preds <- predict(fdaModel, eval)

eval.active <- as.factor(eval.active)
levels(eval.active) <- c("No","Yes")

confusion(preds,eval.active)
#1 - .3219829

varImp(fdaModel)

train(x = train[,-train$train.active],
      y = train.active, 
      method="fda", 
      metric="ROC",
      tuneGrid = )