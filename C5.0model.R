d<-read.csv("train.csv")
require(caret)
library(data.table)
library(xgboost)
library(C50)
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

eresponse = eval$Active_Customer
eval <- eval[,-c(1,257)]
eval <- data.table(eval,keep.rownames = F)

testdat<-read.csv("test.csv")

submission<-data.frame(testdat$Cust_id,predictions=-1)

testdat<-testdat[,-1]
dim(testdat)
colSums(is.na(testdat))
test<- data.table(testdat,keep.rownames = F)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10, returnResamp="all")

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )

response<-factor(response)

c50model<-train(x= train
                , y=response
                , tuneGrid = grid
                , trControl = fitControl
                , method = "C5.0"
                , verbose = TRUE)
  
summary(c50model)


save(c50model,file="C50model.Rdata")


tpreds<-predict(c50model,eval)
length(tpreds)
tpreds<-as.data.frame(tpreds)
#head(tpreds)
tpreds$values<-eresponse
#tpreds$tpredsB<-ifelse(tpreds$tpreds>0.515,1,0)
tab<-table(tpreds$tpreds,tpreds$values)
(tab[1,1]+tab[2,2])/sum(tab)

predictions<-predict(c50model,test)
head(predictions)

submission$predictions<-predictions
#submission$Active_Customer<-predictions
names(submission)<- c("Cust_id","Active_Customer")
#submission$Active_Customer<-ifelse(submission$Active_Customer>0.515,1,0)

write.csv(submission,file="C50model2.csv",row.names = F)

plot(c50model)
