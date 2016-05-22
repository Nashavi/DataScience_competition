require(data.table)
require(caret)
#require(e1071)
require(kernlab)
require(Matrix)

load("train.RData")
load("eval.RData")

response<-train$Active_Customer
train<-train[,-c(1,257)]
train<- data.table(train,keep.rownames = F)
trainmat<- sparse.model.matrix(~.-1, data = train)

ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=5,		    # do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE)

response <- factor(ifelse(response==0, "Zero", "One"))

svm.tune <- train(x=trainmat,
                  y= response,
                  method = "svmRadial",   # Radial kernel
                  #tuneLength = 9,					# 9 values of the cost function
                  #preProc = c("center","scale"),  # Center and scale data
                  metric="Accuracy",
                  trControl=ctrl)

#model <- svm(train,response, kernel = "rbf")

#save(model,file="svm_model.RData")

pred <- predict(svm.tune, eval)
pred2<-ifelse(pred>.515,1,0)
tab<-table(pred2,eval$Active_Customer)
(tab[1,1]+tab[2,2])/sum(tab)


levels(train$Cust_status)
levels(testdat$Cust_status)


#load("svm_model.RData")
testdat<-read.csv("test.csv")
submission<-data.frame(testdat$Cust_id,Active_Customer=-1)
predictions<-predict(model, testdat[,2:256])


summary(model)
RocImp2 <- varImp(model, scale = FALSE)