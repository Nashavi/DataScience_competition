load("varimp.RData")
load("Train.RData")
load("eval.RData")

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

rm(eval,train,importance)

response<-newtrain$Active_Customer
newtrain<-newtrain[,-c(1,2)]
newtrain<- data.table(newtrain,keep.rownames = F)
newtrainmat<- sparse.model.matrix(~.-1, data = newtrain)
newdtrain <- xgb.DMatrix(data = newtrainmat, label = response)


eresponse = neweval$Active_Customer
neweval <- neweval[,-c(1,257)]
neweval <- data.table(neweval,keep.rownames = F)
newevalmat <- sparse.model.matrix(~.-1, data = neweval)
newdeval <- xgb.DMatrix(data = newevalmat, label = eresponse)

eval_watchlist <- list(train=newdtrain, test=newdeval)

newmod <- xgb.train(data=newdtrain
                 , max.depth=6
                 , eta= 0.001
                 , nthread = 2
                 , nround=220
                 , watchlist=eval_watchlist
                 , objective =  "binary:logistic" #"multi:softmax"
                 , eval_metric= "auc"
                 #, num_class=3
                 #, base_score=.9
)

tpreds<-predict(newmod,newdeval)
length(tpreds)
tpreds<-as.data.frame(tpreds)
head(tpreds)
tpreds$values<-eresponse
tpreds$tpredsB<-ifelse(tpreds$tpreds>0.515,1,0)
tab<-table(tpreds$tpredsB,tpreds$values)
tab
(tab[1,1]+tab[2,2])/sum(tab)
plot(roc(tpreds$tpredsB~tpreds$values))
