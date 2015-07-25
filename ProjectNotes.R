##The goal of your project is to predict the manner in which 
##they did the exercise. This is the "classe" variable in the 
##training set. You may use any of the other variables to predict with. 

##You should create a report describing how you built your model, 
##how you used cross validation, 
##what you think the expected out of sample error is, 
##and why you made the choices you did. 

##You will also use your prediction model to predict 20 different 
##test cases.

setwd("D:/eCourses/08_MachineLearning")


data3 <- read.csv("pml-training.csv", na.strings=c(""," ","NA"))
colSums(is.na(data3))
data3 <- data3[, colSums(is.na(data3)) < 1]
str(data3)
data3 <- data3[,8:60]


library(caret)
set.seed(815)
##intital partition into training and test sets
data3part <-createDataPartition(y=data3$classe, p=.6, list=FALSE)
traindata <- data3[data3part,]
testdata <- data3[-data3part,]

##Further split of training data into 2 sets for cross validation
data3part2 <-createDataPartition(y=traindata$classe, p=.5, list=FALSE)
traindata1 <- traindata[data3part2,]
traindata2 <- traindata[-data3part2,]


##nearZeroVar identifies predictor variables than may cause
##problems because of few unique values and high skewness
nearZeroVar(traindata1)

##this section removes highly correlated variables to
##limit potential problems with multicolinearity
ncol(traindata1)
descrCorr <- cor(traindata1[,1:52])
highCorr <- findCorrelation(descrCorr, 0.90)
traindata1 <- traindata1[, -highCorr]
##remove same variables from other training set and from test set
traindata2 <- traindata2[, -highCorr]
testdata <- testdata[, -highCorr]

dim(traindata1)
dim(traindata2)
dim(testdata)

library(caret)
rffit<- train(classe~., data=traindata1, method="rf", 
              trControl=trainControl(method="cv",number=5), 
              prox=TRUE)
##see which variables most important
varImp(rffit)
##test model with second training set
pred <- predict(rffit, traindata2)
traindata2$predRight <- pred==traindata2$classe
table(pred, traindata2$classe)

##various exploratory plots
qplot(roll_belt, pitch_forearm, color=predRight, data=traindata2, main="train2test")
qplot(yaw_belt, pitch_forearm, color=predRight, data=traindata2, main="train2test2")
qplot(roll_belt, pitch_forearm, color=classe, data=traindata1)
qplot(yaw_belt, pitch_forearm, color=classe, data=traindata1)
qplot(yaw_belt, roll_belt, color=classe, data=traindata1)

qplot(magnet_dumbbell_y, magnet_dumbbell_z, color=classe, data=traindata1)

qplot(roll_forearm, pitch_forearm, color=classe, data=traindata1)


##this boosting model also does well (.9688) but not quite as good as
##the random forest (.9766); gets 19/20 on tests
library(gbm)
library(plyr)

##this one does marginally better (.979) and gets 20/20 on tests

gbmGrid2<- expand.grid(interaction.depth = (1:5)*2, 
                       n.trees = (1:4)*50, 
                       shrinkage = .1,
                       n.minobsinnode = 20)
gbmfit2 <- train(classe~., data = traindata1, method = "gbm", 
                trControl = trainControl(method = "cv", number = 5), 
                verbose = FALSE, 
                bag.fraction = 0.5, tuneGrid = gbmGrid2)
gbmfit2

predgbm <- predict(gbmfit2, traindata2)
confusionMatrix(predgbm, traindata2$classe)

testpredgbm <- predict(gbmfit2, testdata)
##table(testpredgbm, testdata$classe)
confusionMatrix(testpredgbm, testdata$classe)

varImp(gbmfit2)

##varImp(rffit)

##table(varImp(gbmfit2), varImp(rffit))
