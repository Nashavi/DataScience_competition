require(nnet)
require(ggplot2)
require(caret)
set.seed(686)

load("train.RData")
load("eval.RData")
train <- rbind(train,eval)
rm(eval)
train.id <- as.numeric(train$Cust_id)
train.active <- train$Active_Customer
train <- train[,c(-1,-257)]

# remove zero variance variables 
nzv <- nearZeroVar(train,saveMetrics = TRUE)
zvcols <- which(nzv$zeroVar==TRUE)
if (length(zvcols) > 0) { 
  train <- train[,-zvcols] 
}

# change factors to dummy vars
dmyCoding <- dummyVars(~.,data=train)
train <- data.frame(predict(dmyCoding,newdata = train))

# # center and scale the variables 
# preProcessValues <- preProcess(train,method = c("center","scale"))
# train <- predict(preProcessValues,train)

preProcValues <- preProcess(train, method = c("knnImpute"))
train <- predict(preProcValues,train)

train.active <- as.factor(train.active)
levels(train.active) <- c("No","Yes")

# load("eval.RData")
# eval.id <- as.numeric(eval$Cust_id)
# eval.active <- eval$Active_Customer
# eval <- eval[,c(-1,-257)]
# 
# #remove zero variance vars
# eval <- eval[,-zvcols] 
# 
# # change factors to dummy vars
# eval <- data.frame(predict(dmyCoding,newdata = eval))
# 
# # center and scale the variables 
# # preProcessValues <- preProcess(eval,method = c("center","scale"))
# # eval <- predict(preProcessValues,eval)
# 
# preProcValues <- preProcess(eval, method = c("knnImpute"))
# eval <- predict(preProcValues,eval)
# 
# eval.active <- as.factor(eval.active)
# levels(eval.active) <- c("No","Yes")


# fullTrain <- read.csv("Train.csv")
# Fulltrain.id <- as.numeric(fullTrain$Cust_id)
# fullTrain.active <- fullTrain$Active_Customer
# fullTrain.active <- as.factor(fullTrain.active)
# levels(fullTrain.active) <- c("No","Yes")
# fullTrain <- fullTrain[,c(-1,-257)]
# 
# # remove zero variance variables 
# nzv <- nearZeroVar(fullTrain,saveMetrics = TRUE)
# zvcols <- which(nzv$zeroVar==TRUE)
# #fullTrain <- fullTrain[,-zvcols] 
# 
# # change factors to dummy vars
# dmyCoding <- dummyVars(~.,data=fullTrain)
# fullTrain <- data.frame(predict(dmyCoding,newdata = fullTrain))
# 
# # # center and scale the variables 
# # preProcessValues <- preProcess(train,method = c("center","scale"))
# # train <- predict(preProcessValues,train)
# 
# preProcValues <- preProcess(fullTrain, method = c("knnImpute"))
# fullTrain <- predict(preProcValues,fullTrain)

test <- read.csv("Test.csv")
test.id <- as.numeric(test$Cust_id)
test <- test[,c(-1)]

#remove zero variance vars
if (length(zvcols)>0){
  test <- test[,-zvcols] 
} 

# change factors to dummy vars
test <- data.frame(predict(dmyCoding,newdata = test))

# center and scale the variables 
# preProcessValues <- preProcess(test,method = c("center","scale"))
# test <- predict(preProcessValues,test)

preProcValues <- preProcess(test, method = c("knnImpute"))
test <- predict(preProcValues,test)

#cleanup environment 
rm(nzv)
rm(zvcols)
rm(dmyCoding)
rm(preProcValues)
