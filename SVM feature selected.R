load("varimp.RData")
load("Train.RData")
load("eval.RData")
library(data.table)
require(Matrix)
require(e1071)
require(caret)

vi<-importance[1:20,]
names<-vi$Feature

newtrain<-data.frame(train$Cust_id,train$Active_Customer)
names(newtrain)<-c("Cust_id","Active_Customer")
for(i in 1:length(names)){
  newtrain<-cbind(newtrain,train[names[i]])
}

neweval<-data.frame(eval$Cust_id,eval$Active_Customer)
names(neweval)<-c("Cust_id","Active_Customer")
for(i in 1:length(names)){
  neweval<-cbind(neweval,eval[names[i]])
}

newtrain<-newtrain[,-1]
nzv <- nearZeroVar(newtrain,saveMetrics = TRUE)
zvcols <- which(nzv$zeroVar==TRUE)
#newtrain <- newtrain[,-zvcols]
newtrain<- data.table(newtrain,keep.rownames = F)

# model <- svm(Active_Customer ~ .
#              , scale = T
#              , kernel="radial"
#              , data = newtrain)

svm.tune <- tune.svm(Active_Customer ~ .
                     , data = newtrain
                     , gamma = 10^(-3:0)
                     , cost = 10^(-1:1))

svm.model <- svm(Active_Customer ~ .
                 , data = newtrain
                 , scale = T
                 , kernel = "radial"
                 , gamma = svm.tune$best.parameters$gamma
                 , cost  = svm.tune$best.parameters$cost)

pred <- predict(svm.model, neweval)
table(pred,neweval$Active_Customer)
head(pred)
pred2<-ifelse(pred>.515,1,0)
tab<-table(pred2,neweval$Active_Customer)
(tab[1,1]+tab[2,2])/sum(tab)


testdat<-read.csv("test.csv")
submission<-data.frame(testdat$Cust_id,Active_Customer=-1)
predictions<-predict(svm.model, testdat)

predictions<-ifelse(predictions>.515,1,0)
submission$Active_Customer<-predictions

write.csv(submission,file="svmselectedtuned.csv",row.names = F)
