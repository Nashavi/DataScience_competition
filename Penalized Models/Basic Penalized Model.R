
source("Penalized Models/Penalized Modeling Setup.R")

#train the model
glmnetModel <- glmnet(x = x.train,
                      y = y.train,
                      family="binomial")

#predict class outcome
predict(glmnetModel,
          newx = x.eval,
          s = c(0.05,0.1,0.2),
          type="class")

#which predictors were used in the model?
predictors <- predict(glmnetModel,
              newx = x.eval,
              s = c(0.05,0.1,0.2),
              type="nonzero")

colnames(eval)[predictors$`1`]
colnames(eval)[predictors$`2`]
colnames(eval)[predictors$`3`]
