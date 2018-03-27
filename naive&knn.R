library(data.table)
require(e1071)
data<-read.csv("framingham1.csv",header=TRUE)
str(data)
data<-as.data.frame(data)
str(data)
data$TenYearCHD<-as.factor(data$TenYearCHD)
str(data)
index<-sample(nrow(data),nrow(data)*.7)
train<-data[index,]
test<-data[-index,]
model <- naiveBayes(TenYearCHD~., data = train)
summary(model)
pred <- predict(model,test)

pred
library(caret)
confusionMatrix(pred,test$TenYearCHD)
---------------------------------------------------------------------------
  #ROSE
  
  library(ROSE)
#data1<-ROSE(Class~.,data,N=1000,p=.5)

#data1<-ROSE(Class ~ ., data = data, seed = 1)


library(class)
model<-knn(train, test,train$TenYearCHD, k = 3, prob=TRUE)
#model <- naiveBayes(Class~., data = train)
#summary(model)
model
pred <- predict(model,test)
library(caret)
confusionMatrix(pred,test$TenYearCHD)
confusionMatrix(model, test$TenYearCHD, positive = "1")
