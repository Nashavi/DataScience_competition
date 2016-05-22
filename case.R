train<-read.csv(file.choose())
str(train)
nas<-as.data.frame(colSums(is.na(train)))
nas<-nas[nas[1]!=0]
str(train$profit)

#There are following NA values in the dataset:
#Profit - 7310. This indicates 88.7% of response variable missing
#schooling - 2406
#custAge - 2014
#day_of_week - 787

require(caret)
require(RANN)
imputedvalues<-preProcess(train,method=c("center","scale","knnImpute"))
train<-predict(imputedvalues,train)

colSums(is.na(imputedtrain))
imputedvalues

require(ggplot2)
subset1<-train[!is.na(train$schooling),]
plot(subset1$schooling)
plot(subset1$responded,subset1$schooling)

subset2<-train[!is.na(train$custAge),]
qplot(subset2$custAge,subset2$profit)
plot(subset2$responded,subset2$custAge)


install.packages("VIM")
?kNN


plot(train$custAge[!is.na(train$custAge)])

require(dplyr)
subsettrain1<-train[train$responded=="yes",] #There are 928 rows with yes
subsettrain2<-sample_n(train[train$responded=="no",],(nrow(subsettrain1)*1.2)) #rows with no that will give a 60% of final dataset
balancedtrain<-rbind(subsettrain1,subsettrain2)


subset4<-train[!is.na(train$day_of_week),]
qplot(subset4$day_of_week,subset4$profit)
plot(subset2$responded,subset2$day_of_week)


?sample_n
str(train$)

test<-read.csv(file.choose())
