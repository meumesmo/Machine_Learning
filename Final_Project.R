library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)

set.seed(31416)

trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

train <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
test <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

inTrain <- createDataPartition(train$classe, p = 0.7, list = FALSE)
myTrain <- train[inTrain,]
myTest <- train[-inTrain,]

nearZero <- nearZeroVar(myTrain, saveMetrics = TRUE)
myTrain <- myTrain[,nearZero$nzv == FALSE]

nearZero <- nearZeroVar(myTest, saveMetrics = TRUE)
myTest <- myTest[, nearZero$nzv == FALSE]

training <- myTrain
for(i in 1:length(myTrain)){
  if(sum(is.na(myTrain[,i]))/nrow(myTrain) >= .6) {
    for(j in 1:length(training)){
      if(length(grep(names(myTrain[i]), names(training)[j])) == 1){
        training <- training[, -j]
      }
    }
  }
}

myTrain <- training

