
source("Penalized Models/Penalized Modeling Setup.R")

#train lasso model alpha = 1
LassoModel <- glmnet(x = x.train,
                      y = y.train,
                      family="binomial",
                      alpha = 1)

plot(LassoModel)

grid <- 10^seq(10,-2,length=100)

y.train <- as.numeric(as.character(y.train))

cv.out <- cv.glmnet(x = x.train, 
                    y = y.train, 
                    alpha= 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(LassoModel,s=bestlam,newx = x.eval,type="class")
table(lasso.pred,y.eval)
mean(lasso.pred==y.eval)
#(3301+3612)/length(lasso.pred)
