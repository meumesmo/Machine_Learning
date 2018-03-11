install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("RColorBrewer") 
install.packages("https://togaware.com/access/rattle_5.0.14.tar.gz", repos=NULL, type="source")
install.packages("randomForest")
install.packages("corrplot")
install.packages("gbm")
install.packages("e1071")

library(e1071)
library(caret)
library(rpart.plot)
library(rpart)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(corrplot)
library(gbm)

trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

train <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
test <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

inTrain <- createDataPartition(train$classe, p = 0.8, list = FALSE)
myTrain <- train[inTrain,]
myTest <- train[-inTrain,]

nearZero <- nearZeroVar(myTrain, saveMetrics = TRUE)
myTrain <- myTrain[,nearZero$nzv == FALSE]

nearZero <- nearZeroVar(myTest, saveMetrics = TRUE)
myTest <- myTest[, nearZero$nzv == FALSE]

myTrain <- myTrain[c(-1)]

#removing variables with more than 60% NA
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
dim(myTrain)

clean1 <- colnames(myTrain)
clean2 <- colnames(myTrain[, -58])
myTest <- myTest[clean1]

test <- test[clean2]

dim(myTest)
dim(test)

#Coerce os dados no mesmo tipo

for (i in 1:length(test)) {
  for (j in 1:length(myTrain)){
    if(length(grep(names(myTrain[i]), names(test)[j])) == 1) {
      class(test[j]) <- class(myTrain[i])
    }
  }
}

test <- rbind(myTrain[2, -58], test)
test <- test[-1, ]

modFit1 <- rpart(classe ~ ., data = myTrain, method = "class")
fancyRpartPlot(modFit1)

predictions1 <- predict(modFit1, myTest, type = "class")
cmtree <- caret::confusionMatrix(predictions1, myTest$classe)
cmtree

plot(cmtree$table, col = cmtree$byClass, main = paste("Decision Tree Confusion Matrix : Accuracy = ", round(cmtree$overall['Accuracy'], 4)))

modFit2 <- randomForest(classe ~., data = myTrain)
predictions2 <- predict(modFit2, myTest, type = "class")
cmrf <- confusionMatrix(predictions2, myTest$classe)
cmrf

plot(modFit2)

plot(cmrf$table, col = cmrf$byClass, main = paste("Random Forest Confusion Matrix : Accuracy = ", round(cmrf$overall['Accuracy'], 4)))

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1)

modFit3 <- train(classe ~., data = myTrain, method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)

modFit3$finalModel

modFit3f <- modFit3$finalModel

predictions3 <- predict(modFit3, newdata = myTest)
cmgbm <- confusionMatrix(predictions3, myTest$classe)
cmgbm
plot(modFit3, ylim = c(0.9, 1))

predictions_final <- predict(modFit2, test, type = "class")
predictions_final

#inutil essa funcao
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}

