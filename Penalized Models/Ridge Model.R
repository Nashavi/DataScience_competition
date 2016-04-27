
source("Penalized Models/Penalized Modeling Setup.R")

#train Ridge model alpha = 0
RidgeModel <- glmnet(x = x.train,
                     y = y.train,
                     family="binomial",
                     alpha = 0)

plot(RidgeModel)

grid <- 10^seq(10,-2,length=100)

y.train <- as.numeric(as.character(y.train))

cv.out <- cv.glmnet(x = x.train, 
                    y = y.train, 
                    alpha= 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
ridge.pred <- predict(RidgeModel,s=bestlam,newx = x.eval,type="class")
table(ridge.pred,y.eval)
mean(ridge.pred==y.eval)
#(3363+3576)/length(ridge.pred)
