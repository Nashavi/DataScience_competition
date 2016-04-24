source("LDA Modeling/LDA Setup.R")

#create lda model 
lda.fit <- lda(train.active~.,data=train)
summary(lda.fit)
plot(lda.fit)

#make predictions
lda.pred <- predict(lda.fit,eval)
lda.class = lda.pred$class
table(lda.class,eval.active)
mean(lda.class==eval.active)
