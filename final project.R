# Final project in PML
# load library needed
library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)    			# Enhanced tree plots
library(caret)					# Just a data source for this script

# read datasets from local repo or desktop
train=read.csv("pml-training.csv", header=TRUE)
test=read.csv("pml-testing.csv", header=TRUE)

# found a lot of NAs and missing values in train data. a process of data cleaning in COLUMN is necessary in the begining
badTrainind1<-sapply(train, function(x) any(is.na(x)))
badTrainind2<-sapply(train, function(x) "" %in% levels(x))
badTrainindtot <-badTrainind1 | badTrainind2
cleanTrain <- train[,-which(badTrainindtot)]

# first 7 column of cleanTrain are not important for training model. remove them
cleanTrain <- cleanTrain[,-1:-7]

# split train data into training data and testing data with the ratio of 70/30
set.seed(125)
inTrain <-createDataPartition(cleanTrain$classe, p = 0.7, list=FALSE)
training <- cleanTrain[inTrain,]
testing <- cleanTrain[-inTrain,]

# create small dataset for quick model training
# the number of observation =787
set.seed(125)
inTrainsmall <-createDataPartition(cleanTrain$classe, p = 0.04, list=FALSE)
trainingsmall <- cleanTrain[inTrainsmall,]
testingsmall <- cleanTrain[-inTrainsmall,]

# quick survey via decision tree (n=787) and check accracy of this model
rpartmodelfitsmall <- rpart(classe~., data=trainingsmall, method="class")
predictiontrainingsmall <- predict(rpartmodelfitsmall, newdata=testingsmall, type="class")
confusionMatrix(predictiontrainingsmall, testingsmall$classe)
# Accuracy is only 0.666. I choose larger dataset to see if the size of training dataset affect accuracy a lot ot not
# Need use another model with higher accuracy rate. I increase the size of dataset 10 time larger.

# resample training dataset which has number of observation equals to 7870, and use rpart to create model
inTrainsmall1 <-createDataPartition(cleanTrain$classe, p = 0.4, list=FALSE)
trainingsmall1 <- cleanTrain[inTrainsmall1,]
testingsmall1 <- cleanTrain[-inTrainsmall1,]

# quick survey via decision tree (n=7870) and check accracy of this model
rpartmodelfitsmall1 <- rpart(classe~., data=trainingsmall1, method="class")
predictiontrainingsmall1 <- predict(rpartmodelfitsmall1, newdata=testingsmall1, type="class")
confusionMatrix(predictiontrainingsmall1, testingsmall1$classe)
# the accuracy is 0.738, which means the accuracy got from rpart model is low. Need to switch to another model. I choose to use randomForest 


# use rf model and small set of training data, trainingsmall, to predict model
rf787modelfit <- randomForest(classe~., data=trainingsmall, importance = FALSE)
predictionrf787modelfit <- predict(rf787modelfit, newdata=testingsmall, type="class")
confusionMatrix(predictionrf787modelfit, testingsmall$classe)
# The accuracy is 0.9104, which is greaterly improved. Change the size of training dataset to see if the effect of size on accuracy


rf7870modelfit <- randomForest(classe~., data=trainingsmall1, importance = FALSE)
predictionrf7870modelfit <- predict(rf7870modelfit, newdata=testingsmall1, type="class")
confusionMatrix(predictionrf7870modelfit, testingsmall1$classe)
# The accuracy is 0.9906. Stick in the randomForest model for prediction

# use predictionrf7870modelfit to predict testing dataset
predictionrftesting <- predict(rf7870modelfit, newdata=testing, type="class")
confusionMatrix(predictionrftesting, testing$classe)
# got Accuracy : 0.9946

# apply model to real test dataset
predictionrftest<- predict(rf7870modelfit, newdata=test, type="class")
# predicted results
#  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
#  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
#Levels: A B C D E


