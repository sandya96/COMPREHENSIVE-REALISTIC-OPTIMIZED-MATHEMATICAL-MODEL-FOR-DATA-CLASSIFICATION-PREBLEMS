
#loding the data set
d<-read.csv("framingham1.csv",header=TRUE)
str(d)
dim(d)

#divide into train and test sets randomly
library(caTools)
set.seed(1000)
split = sample.split(d$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(d, split==TRUE)
test1 = subset(d, split==FALSE)

train$TenYearCHD<-as.numeric(train$TenYearCHD)
td<-data.matrix(train[,-16])
l<-data.matrix(train[,16])

#building the model
library(xgboost)
mode<-xgboost(data=td,label=l,,nrounds=100,verbose=T,
              params=list(objective="binary:logistic"))
test<-data.matrix(test1[,-16])

#predicting the TenYearCHD for the test set without the TenYearCHD coloumn
#i.e.,the test set is considered as the new incoming data
pr<-predict(mode,test)
pres<-ifelse(pr>0.5,1,0)
plot(pr)
plot(pres)#calculating the accuracy
library(caret)
confusionMatrix(pres,test1$TenYearCHD)
plot(pr,test1$TenYearCHD)

#--------------------------------------------------------------------------

#dividing the data set into train anad test sets sequentially

d<-read.csv("framingham1.csv",header=TRUE)
library(xgboost)
index<-sample(nrow(d),nrow(d)*0.7)
train1<-d[index, ] #train set
test2<-d[-index, ]

train1$TenYearCHD<-as.numeric(train1$TenYearCHD)
td1<-data.matrix(train1[,-16])
l1<-data.matrix(train1[,16])

#bulding the model
mode1<-xgboost(data=td1,label=l1,,nrounds=200,verbose=T,
              params=list(objective="binary:logistic"))
test3<-data.matrix(test2[,-16])

#predicting the TenYearCHD for the test set without the TenYearCHD coloumn
#i.e.,the test set is considered as the new incoming data
pr1<-predict(mode1,test3)
pres1<-ifelse(pr1>0.75,1,0)
plot(pr1)
plot(pres1)#calculating the accuracy
library(caret)
confusionMatrix(pres1,test2$TenYearCHD)
plot(pr1,test2$TenYearCHD)
a<-1:200
plot(a,mode1$evaluation_log$train_error)
