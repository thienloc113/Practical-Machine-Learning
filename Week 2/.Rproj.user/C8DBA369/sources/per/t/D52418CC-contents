#Caret package
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list = F)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

set.seed(32343)
modelFit <- train(type~., data = training, method="glm")
modelFit
modelFit$finalModel

predictions <-predict(modelFit, newdata=testing)
confusionMatrix(predictions, testing$type)


#Data Slicing
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list = F)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)
      #Kfold
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = T)
sapply(folds, length)