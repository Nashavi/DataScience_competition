d<-read.csv("train.csv")
require(caret)
library(data.table)
library(xgboost)
library(Matrix)
#library(pROC)
str(d)
head(d)

set.seed(66)

sum[d$Active_Customer==0]

#require(psych)
#describe<-as.data.frame(describe(d))

#Imputation
nas<-as.data.frame(colSums(is.na(d)))


inEval <- createDataPartition(d$Active_Customer,p=.4,list=FALSE)
eval <- d[ inEval,]
train <- d[ -inEval,]

save(train,file="train.RData", compress=T)
save(eval,file="eval.RData", compress=T)

response<-train$Active_Customer
train<-train[,-c(1,257)]
train<- data.table(train,keep.rownames = F)
trainmat<- sparse.model.matrix(~.-1, data = train)
dtrain <- xgb.DMatrix(data = trainmat, label = response)


eresponse = eval$Active_Customer
eval <- eval[,-c(1,257)]
eval <- data.table(eval,keep.rownames = F)
evalmat <- sparse.model.matrix(~.-1, data = eval)
deval <- xgb.DMatrix(data = evalmat, label = eresponse)


eval_watchlist <- list(eval=deval,train=dtrain)

testdat<-read.csv("test.csv")

submission<-data.frame(testdat$Cust_id,predictions=-1)

testdat<-testdat[,-1]
dim(testdat)
colSums(is.na(testdat))
preds<- data.table(testdat,keep.rownames = F)
predsmat <- sparse.model.matrix(~.-1, data = preds)
dpreds <- xgb.DMatrix(data = predsmat)

mod <- xgb.train(data=dtrain
                 , max.depth=5
                 , eta= .01
                 , nthread = 2
                 , nround=600
                 , watchlist=eval_watchlist
                 , objective =  "binary:logistic" #"multi:softmax"
                 , eval_metric= "auc"
                 #, num_class=2
                 #, base_score=.9
                 #, min_child_weight = 10
                 , early.stop.round = 10
                 #, subsample=.5
                 #, lambda=.6
                 #, colsample_bylevel=0.6
                 )

#save(mod,file="baseline_model.RData")

nrow(deval)
tpreds<-predict(mod,deval)
length(tpreds)
tpreds<-as.data.frame(tpreds)
#head(tpreds)
tpreds$values<-eresponse
tpreds$tpredsB<-ifelse(tpreds$tpreds>0.515,1,0)
tab<-table(tpreds$tpredsB,tpreds$values)
(tab[1,1]+tab[2,2])/sum(tab)

predictions<-predict(mod,dpreds)
head(predictions)

submission$predictions<-predictions
names(submission)<- c("Cust_id","Active_Customer")
submission$Active_Customer<-ifelse(submission$Active_Customer>0.515,1,0)

write.csv(submission,file="submission6.csv",row.names = F)

importance <- as.data.frame(xgb.importance(feature_names = trainmat@Dimnames[[2]], model = mod))
save(importance,file="varimp.RData",compress=T)