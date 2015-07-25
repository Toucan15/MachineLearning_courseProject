##test set
testdata <- read.csv("pml-testing.csv", na.strings=c(""," ","NA"))
colSums(is.na(testdata))
testdata <- testdata[, colSums(is.na(testdata)) < 1]
str(testdata)
testdata <- testdata[,8:60]

library(caret)

predt <- predict(rffit, testdata)
##testdata$predRight <- predt==testdata$classe
## this step didn't work because the test set doesn't have the classe varibale


predt
