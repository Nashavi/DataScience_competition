require(elasticnet)
#library(glmnet)
library(data.table)
require(caret)

load("train.RData")
load("eval.RData")

x = data.frame(nearZeroVar(train, saveMetrics = TRUE))
zerovarcols <- which(x$zeroVar==TRUE | x$nzv>0 | x$percentUnique<0.0001) #Removing zero variance and near-zero variance predictors
train <- train[,-zerovarcols]
response<-train$Active_Customer
trainmat<- model.matrix(Active_Customer~., data = train)[,-(ncol(train))]

eval <- eval[,-zerovarcols]
erespone<-eval$Active_Customer
evalmat <- model.matrix(Active_Customer~., data = eval)[,-(ncol(eval))]

# require(psych)
# describe(train)
enetModel <- enet(
                    x = trainmat
                  , y = response
                  , lambda = .01
                  , trace = TRUE
                  , normalize = FALSE
                  , max.steps = 100
                  )
#Error in enet(x = trainmat, y = response, lambda = 0.01, trace = TRUE,  : 
#Some of the columns of x have zero variance

save(enetModel,file="enetModel.RData")


enetPreds <- predict(enetModel
                     ,evalmat
                     ,s = .01
                     ,mode = "fraction"
                     ,type = "fit")

length(erespone)
length(enetPreds$fit)
Enetpreds<-data.frame(erespone,enetPreds$fit)
Enetpreds$fit<-ifelse(Enetpreds$enetPreds.fit>0.5155,1,0)
tab<-table(Enetpreds$erespone,Enetpreds$fit)
(tab[1,1]+tab[2,2])/sum(tab)


test<-read.csv("test.csv")
test <- test[,-zerovarcols]
testmat<- model.matrix(~., data = test)

predictions<-predict(enetModel
                     , testmat
                     , s = .01
                     , mode = "fraction"
                     , type = "fit")

submission<-data.frame(test$Cust_id)
submission$predictions<-predictions
names(submission)<- c("Cust_id","Active_Customer")
submission$Active_Customer<-ifelse(submission$Active_Customer>0.5155,1,0)

write.csv(submission,file="Enetsubmission.csv",row.names = F)
