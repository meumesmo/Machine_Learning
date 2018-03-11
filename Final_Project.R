install.packages("tfestimators")

library(tfestimators)
library(tensorflow)
library(keras)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(knitr)

install_tensorflow()

sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)

set.seed(31416)

trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

train <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
test <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

res <- train$classe

train$classe <- NULL

train_num <- train[sapply(train, is.numeric())] 
train_nnum <- train[sapply(train, !is.numeric())] 

train_input_fn <- function(data, num_epochs = 1){
  input_fn(data, 
           features = colnames(data),
           response = "classe",
           batch_size = 32,
           num_epochs = num_epochs)
}

cols <- feature_columns(
  column_numeric(colnames(train[sapply(train, is.numeric())])),
  column_indicator(colnames(train[sapply(train, !is.numeric())]))
  )



indices <- sample(1:nrow(train), size = 0.80 * nrow(train))

myTrain <- train[indices, ]
myTest <- train[-indices, ]

model %>% train(train_input_fn(myTrain, num_epochs = 10))  
  
  
inTrain <- createDataPartition(train$classe, p = 0.8, list = FALSE)
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

