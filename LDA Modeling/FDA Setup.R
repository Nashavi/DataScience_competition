
require(caret)
require(mda)
require(earth)
require(MASS)
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
#If length(zvcols) > 0 {
#  train <- train[,-zvcols] 
#} 


# change factors to dummy vars
dmyCoding <- dummyVars(~.,data=train)
train <- data.frame(predict(dmyCoding,newdata = train))

# center and scale the variables 
preProcessValues <- preProcess(train,method = c("center","scale","knnImpute"))
train <- predict(preProcessValues,train)

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
# preProcessValues <- preProcess(eval,method = c("center","scale","knnImpute"))
# eval <- predict(preProcessValues,eval)
# 

test <- read.csv("Test.csv")
test.id <- as.numeric(test$Cust_id)
test <- test[,c(-1)]

#remove zero variance vars
#test <- test[,-zvcols] 

# change factors to dummy vars
test <- data.frame(predict(dmyCoding,newdata = test))

# center and scale the variables 
preProcessValues <- preProcess(test,method = c("center","scale","knnImpute"))
test <- predict(preProcessValues,test)



#cleanup environment 
rm(nzv)
rm(zvcols)
rm(dmyCoding)
rm(preProcessValues)
