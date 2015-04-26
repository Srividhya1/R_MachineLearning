# R_MachineLearning
Machine Learning Project using R(caret package)
---
title: "R_Practical_MachineLearning"
output: html_document
---
Step 1: Loading the necessary libraries
  1. caret package - For making use of built in functions for prediction
  2. ggplot2 - For creating visualization plots during exploratory data analysis
  3.randomForest - Prediction Algorithm used
  

```{r}
 library(caret)
 library(ggplot2)
library(rpart)
library(randomForest)
```

Step 2: Load the training and the test data

```{r}
trainingdata <- read.csv("C:/Documents and Settings/Priyadharsini/Desktop/R/pml-training.csv")
testdata <- read.csv("C:/Documents and Settings/Priyadharsini/Desktop/R/pml-testing.csv")
```

Step 2: Data Partitioning

Partitioning the data on the outcome variable "classe" into 60% of training and 40% of test data

```{r}
inTrain <-createDataPartition(y=trainingdata$classe, p=0.60, list=FALSE)
trainingdata <- trainingdata[inTrain,]
testingdata <- trainingdata[-inTrain,]
```

Step 3: Data Cleaning

The first step to data cleaning is to treat the missing values/outliers so that they dont skew the prediction model.Summary(training) will provide the details about the variables in the data and their levels.

```{r}
dim(trainingdata)
summary(trainingdata)
```

In the current data, many variables have missing values . Columns having > 80% of missing values can be excluded from the analysis. Out of the 160 variables, 67 variables have more than eighty percent of NA's. SO, these columns can be dropped.
```{r}
unnecessarycols <- sapply(colnames(trainingdata), function(x) if(sum(is.na(trainingdata[, x])) > 0.8*nrow(trainingdata)){return(T)}else{return(F)})
trainingdata <- trainingdata[, !unnecessarycols]

unnecessarycols <- sapply(colnames(testdata), function(x) if(sum(is.na(testdata[, x])) > 0.8*nrow(testdata)){return(T)}else{return(F)})
testdata <- testdata[, !unnecessarycols]

nzv <- nearZeroVar(trainingdata,saveMetrics=TRUE)
trainingdata <- trainingdata[,nzv$nzv==FALSE]

nzv <- nearZeroVar(testdata,saveMetrics=TRUE)
testdata <- testdata[,nzv$nzv==FALSE]
```

To further understand how the different variables are related to the outcome, we find the correlation

```{r}
cor <- abs(sapply(colnames(trainingdata[, -ncol(trainingdata)]), function(x) cor(as.numeric(trainingdata[, x]), as.numeric(trainingdata$classe), method = "spearman")))
```

But, the variables donot have high correlation with outcome variable.

Step 4:
Fitting a Model
Using different ML Algorithms on the training data and compare results:

1.Linear Discriminant Model

```{r}
fit.lda <- train(classe ~ . -accel_arm_x,
             method = "lda",
             trControl = ctrl,
             data = trainingdata)
pred.lda <- predict(fit.lda, newdata = testingdata)
confusionMatrix(pred.lda, testingdata$classe)$overall[1]
```

2. Boosting

```{r}

set.seed(123)
boostFit <- train(classe ~ ., method = "gbm", data = trainingdata, verbose = F, trControl = trainControl(method = "cv", number = 10))
```

3.Decision Tree
```{r}
decisionTree <- rpart(classe ~ ., data=trainingdata, method="class")
plot(decisionTree)
text(decisionTree)
```
Predicting the values

```{r}
prediction1 <- predict(decisionTree, testingdata, type = "class")
```


4.Random Forest
```{r}
set.seed(3333)
fit.rf <- train(classe ~ ., 
            method = "rf",
            data = training, 
            trControl = ctrl,
            importance = T)
pred.rf <- predict(fit.rf, newdata = testingdata)
confusionMatrix(pred.rf, testingdata$classe)$overall[1]
```

The model which performed best was a random forest model, which achieved an estimated out-of-sample accuracy of about 99.8% using both 10-fold cross-validation and a separate sub-sampled test set.

