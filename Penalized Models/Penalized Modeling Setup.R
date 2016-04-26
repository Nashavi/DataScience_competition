
require(caret)
require(glmnet)
set.seed(686)

load("train.RData")
train.id <- as.numeric(train$Cust_id)
y.train <- as.factor(train$Active_Customer)
train <- train[,c(-1,-257)]

# # remove zero variance variables 
nzv <- nearZeroVar(train,saveMetrics = TRUE)
zvcols <- which(nzv$zeroVar==TRUE)
train <- train[,-zvcols] 

# # change factors to dummy vars
dmyCoding <- dummyVars(~.,data=train)
train <- data.frame(predict(dmyCoding,newdata = train))
# 
# # center and scale the variables 
preProcessValues <- preProcess(train,method = c("center","scale"))
train <- predict(preProcessValues,train)
x.train <- as.matrix(train)


load("eval.RData")
eval.id <- as.numeric(eval$Cust_id)
y.eval <- eval$Active_Customer
eval <- eval[,c(-1,-257)]

#remove zero variance vars
eval <- eval[,-zvcols] 

# change factors to dummy vars
eval <- data.frame(predict(dmyCoding,newdata = eval))

# center and scale the variables 
preProcessValues <- preProcess(eval,method = c("center","scale"))
eval <- predict(preProcessValues,eval)
x.eval <- as.matrix(eval)


#cleanup environment 
rm(nzv)
rm(zvcols)
rm(dmyCoding)
rm(preProcessValues)
