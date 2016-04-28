#deleted Customer index column from dataset
data=read.csv(file.choose())
head(data)
str(data)
summary(data)
#logistic regression model
full.model = glm(Active_Customer~.-Active_Customer,data=data,family=binomial(link="logit"))
null.model = glm(Active_Customer~1,data=data,family=binomial(link="logit"))
step.model = step(null.model,scope=list(upper=full.model),data=data,direction="both")

#What data type are important vars
str(data$Food110)
str(data$Food163)
str(data$Food161)

activecustomers= table(data$Active_Customer)
barplot(activecustomers, main="Active Customers", xlab="Number of Active customers")
#50% split between active and not active customers

#trans9 
hist(data$Trans9, main="Distribution of Trans 9", xlab="Value of Trans 9", breaks=40)

#trans10 
hist(data$Trans10, main="Distribution of Trans 10", xlab="Value of Trans 10", breaks=40)

#trans11
hist(data$Trans11, main="Distribution of Trans 11", xlab="Value of Trans 11", breaks=40)

#trans8 
hist(data$Trans8, main="Distribution of Trans 8", xlab="Value of Trans 8", breaks=40)

#Distribution of Customer tenure
hist(data$Cust_Tenure, main="Distribution of Customer Tenure", xlab="Customer Tenure", breaks=40)

#trans4
hist(data$Trans4, main="Distribution of Trans 4", xlab="Value of Trans 4", breaks=40)

#Food163 
hist(data$Food163, main="Distribution of Food163", xlab="Value of Food163", breaks=40)

#trans40 is quite uniform 
hist(data$Trans40, main="Distribution of Trans 40", xlab="Value of Trans 40", breaks=40)

#trans41
hist(data$Trans41, main="Distribution of Trans 41", xlab="Value of Trans 41", breaks=40)

#trans31 .. interesting, extreme values are significantly more than the ones in the center
hist(data$Trans31, main="Distribution of Trans 31", xlab="Value of Trans 31", breaks=40)

#Food161
hist(data$Food161, main="Distribution of Food161", xlab="Value of Food161", breaks=40)


str(data$Active_Customer)
newy=as.factor(data$Active_Customer)
str(newy)

#how our x variables influence response.. here the response is newy
#trans9 versus new response
boxplot(Trans9~newy,data=data, main="Trans9 vs response",xlab="Status", ylab="Trans9")
#trans11
boxplot(Trans11~newy,data=data, main="Trans11 vs response",xlab="Status", ylab="Trans11")
#trans10
boxplot(Trans10~newy,data=data, main="Trans10 vs response",xlab="Status", ylab="Trans10")
#trans41
boxplot(Trans41~newy,data=data, main="Trans41 vs response",xlab="Status", ylab="Trans41")
#trans31
boxplot(Trans31~newy,data=data, main="Trans31 vs response",xlab="Status", ylab="Trans31")
#food vs new y
boxplot(Food161~newy,data=data, main="Food161 vs response",xlab="Status", ylab="Food161")
boxplot(Food110~newy,data=data, main="Food110 vs response",xlab="Status", ylab="Food110")
boxplot(Food46~newy,data=data, main="Food46 vs response",xlab="Status", ylab="Food46")


boxplot(Cust_Tenure~newy,data=data, main="Customer Tenure vs response",xlab="Status", ylab="Customer Tenure")




