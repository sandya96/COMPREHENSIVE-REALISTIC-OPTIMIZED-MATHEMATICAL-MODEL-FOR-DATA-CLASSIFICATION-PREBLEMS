library(caret)

d<-read.csv(file.choose(),header=TRUE)

#divide into train and test sets randomly
library(caTools)
set.seed(1000)
split = sample.split(d$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(d, split==TRUE)
test = subset(d, split==FALSE)

#implementation of svmin Radial mode

train[["TenYearCHD"]]=factor(train[["TenYearCHD"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Radial <- train(TenYearCHD ~., data = train, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

plot(svm_Radial)

svm_pre<-predict(svm_Radial,newdata=test)
head(svm_pre)

confusionMatrix(svm_pre,test$TenYearCHD)

#---------------------------

#implementing svm in linear mode


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(TenYearCHD ~., data = train, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

plot(svm_Linear)

svm_preL<-predict(svm_Linear,newdata=test)
head(svm_preL)

confusionMatrix(svm_preL,test$TenYearCHD)
