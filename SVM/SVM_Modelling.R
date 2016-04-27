#library(data.table)
#require(caret)
require(e1071)

load("train.RData")
load("eval.RData")

model <- svm(Active_Customer ~ ., data = train)

pred <- predict(model, eval)
table(pred,eval$Active_Customer)
head(pred)
pred2<-ifelse(pred>.515,1,0)
table(pred2,eval$Active_Customer)

testdat<-read.csv("test.csv")
submission<-data.frame(testdat$Cust_id,Active_Customer=-1)
predictions<-predict(model, testdat)

summary(model)
