require(elasticnet)
#library(glmnet)
library(data.table)
require(caret)

load("train.RData")
load("eval.RData")

x = data.frame(nearZeroVar(train, saveMetrics = TRUE))
zerovarcols <- which(x$zeroVar==TRUE)
train <- train[,-zerovarcols]
response<-train$Active_Customer
trainmat<- model.matrix(Active_Customer~., data = train)[,-(ncol(train))]

eval <- eval[,-zerovarcols]
erespone<-eval$Active_Customer
evalmat <- model.matrix(Active_Customer~., data = eval)[,-(ncol(eval))]

enetModel <- enet(
                    x = trainmat
                  , y = response
                  , lambda = .01
                  , trace = TRUE
                  , normalize = TRUE
                  )

enetPreds <- predict(enetModel
                     ,evalmat
                     ,s = .01
                     ,mode = "fraction"
                     ,type = "fit")