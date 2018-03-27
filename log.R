d<-read.csv("framingham1.csv",header=TRUE)
sapply(d,sd)
sapply(d,mean)
sapply(d,median)
class(d)
d$TenYearCHD<-as.factor(d$TenYearCHD)
library(ggplot2)
# Randomly split the data into training and testing sets
library(caTools)
set.seed(1000)
split = sample.split(d$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(d, split==TRUE)
test = subset(d, split==FALSE)

#Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)

# Accuracy
(1252+13)/(1252+7+212+13)

# Baseline accuracy
(1252+7)/(1252+7+212+13) 
plot(framinghamLog)


#rounding off to 0 and 1
predictTest<-ifelse(predictTest>0.5,1,0)
table(test$TenYearCHD,predictTest)

#------------------------------------------------------------------------

index<-sample(nrow(d),nrow(d)*0.7)
train1<-d[index, ] #train set
test1<-d[-index, ]
model<-glm(TenYearCHD ~., family=binomial(link='logit'),data=train1)
model #logistic model
model1<-step(model)
model1
predictTest1 = predict(model, type="response", newdata=test1)
table(test1$TenYearCHD,predictTest1>0.5)
#accuracy
(1082+12)/(1082+7+171+12)
#baseline accuracy
(1082+7)/(1082+7+171+12)
