#library(data.table)
#require(caret)
require(e1071)

load("train.RData")
load("eval.RData")

train<-train[,-1]
nzv <- nearZeroVar(train,saveMetrics = TRUE)
zvcols <- which(nzv$zeroVar==TRUE)
train <- train[,-zvcols]
train<- data.table(train,keep.rownames = F)

model <- svm(Active_Customer ~ .
            , scale = T
            , kernel="radial"
            , data = train)
# save(mode,file="SVMmodel.Rdata")

# svm.tune <- tune.svm(Active_Customer ~ .
#                      , data = train
#                      , gamma = 10^(-3:0)
#                      , cost = 10^(-1:1))
# 
# svm.model <- svm(Active_Customer ~ .
#                  , data = train
#                  , kernel = "radial"
#                  , gamma = svm.tune$best.parameters$gamma
#                  , cost  = svm.tune$best.parameters$cost)

pred <- predict(model, eval)
table(pred,eval$Active_Customer)
head(pred)
pred2<-ifelse(pred>.515,1,0)
tab<-table(pred2,eval$Active_Customer)
(tab[1,1]+tab[2,2])/sum(tab)
    

testdat<-read.csv("test.csv")
submission<-data.frame(testdat$Cust_id,Active_Customer=-1)
predictions<-predict(model, testdat)

predictions<-ifelse(predictions>.515,1,0)
submission$Active_Customer<-predictions

write.csv(submission,file="svmsubmit3.csv",row.names = F)


