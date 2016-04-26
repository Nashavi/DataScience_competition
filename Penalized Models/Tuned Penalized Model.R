
source("Penalized Models/Penalized Modeling Setup.R")

levels(y.train) <- c("No","Yes")

ctrl <- trainControl(summaryFunction = twoClassSummary, #use classification
                      classProbs = TRUE)

glmnGrid <- expand.grid(.alpha = c(0,.1,.2,.4,.6,.8,1),
                        .lambda = seq(.01,.2,length=40))

glmnTuned <- train(x = train,
                   y = y.train,
                   method="glmnet",
                   tuneGrid = glmnGrid,
                   preProcess = c("center","scale"),
                   metric = "ROC",
                   trControl = ctrl)

save(glmnTuned,file="Penalized Models/Tuned glmn Model.RData")
