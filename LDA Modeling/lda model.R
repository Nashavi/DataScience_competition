require(caret)
require(MASS)

d <- read.csv("Train.csv")
id <- as.numeric(d$Cust_id)
active <- d$Active_Customer
d <- d[,c(-1,-257)]
head(d)
tail(d)

set.seed(686)

#d <- model.matrix(active ~.,data=d)
nzv <- nearZeroVar(d, saveMetrics= TRUE)
zv <- which(nzv$zeroVar==TRUE)

#d$Cust_status <- as.numeric(as.character(unclass(d$Cust_status)))

preProcessValues <- preProcess(d,method = c("center","scale"))
d <- predict(preProcessValues,d)

lda.fit <- lda(active~.,data=d)

