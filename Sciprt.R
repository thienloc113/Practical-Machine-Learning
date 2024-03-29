## Project
## Loading libraries
library(ggplot2); library(caret); library(randomForest); library(rpart); library(rpart.plot)

##Loading data and pre-processing
training <- read.csv("pml-training.csv", na.strings = c("NA","#DIV/0!", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA","#DIV/0!", ""))
str(testing)
str(training)
#The training dataset has 19622 observations and 160 variables, and the testing data set contains 20 observations. We will build model based on training set

# Cleaning data
 #We will delete columns which include all missing value
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]
      #Then we delete columns which are irrelevant, and have little predicting power, follow me these are columns from 1 to seven
trainset <-training[, -c(1:7)]
testset <- testing[, -c(1:7)]
      #then we change variable "classe" in training set to factor.  it will help us later when we predict our test data using our predictive model.
trainset$classe <- as.factor(trainset$classe)

# Data splitting
# partition the data so that 75% of the training dataset into training and the remaining 25% to validation set compute the out-of-sample errors.
set.seed(1712)
inTrain <- createDataPartition(y=trainset$classe, p=0.75, list=FALSE)
train <- trainset[inTrain, ] 
valid <- trainset[-inTrain, ]
table(train$classe)
 ## So, the variable "classe" contains 5 levels: A, B, C, D and E. Level A is the most frequent while level D is the least frequent.
      

## Building prediction algorithms
#We will investigate the classification trees and random forests to predict the outcome.
#1. classification trees
model1 <- rpart(classe ~ ., data=train, method="class")

prediction1 <- predict(model1, valid, type = "class")
confusionMatrix(prediction1, as.factor(valid$classe))
#If we plot the classification trees
rpart.plot(model1, main="Classification Tree", extra=102)

#From the confusion matrix, the accuracy rate is 0.7445
#2. Random forest
train$classe <- as.factor(train$classe)
model2 <- randomForest(classe ~. , data=train, method="class")
prediction2 <- predict(model2, valid, type = "class")
confusionMatrix(prediction2, as.factor(valid$classe))
# Decision on which Prediction Model to Use:
# Random Forest algorithm performed better than Decision Trees. Accuracy for Random Forest model was 0.995 compared to Decision Tree model with 0.739 The expected out-of-sample error is 0.005. Although, This leads to high accuracy, although RD is difficult to interpret and computationally cost

##Prediction on Testing Set
## Here is the final outcome based on the Prediction Model 2 (Random Forest) applied for the testing set
predict(model2, testset)
