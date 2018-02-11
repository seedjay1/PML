library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)

# load
file_train <- 'pml-training.csv'
file_test <- 'pml-testing.csv'

data_train <- read.csv(file_train, na.strings = c('NA', '#DIV/0!', ''))
data_test <- read.csv(file_test, na.strings = c('NA', '#DIV/0!', ''))


# partition training 80/20
in_train <- createDataPartition(data_train$classe, p=.8, list=FALSE)
data_train_train <- data_train[in_train, ]
data_train_test <- data_train[-in_train, ]


# data cleaning

# remove key column - no predictive value
data_train_train <- data_train_train[c(-1)]

# low variance columns don't predict much, so we remove them
lowvar <- nearZeroVar(data_train_train, saveMetrics = TRUE)
data_train_train <- data_train_train[, lowvar$nzv == FALSE]

#lowvar <- nearZeroVar(data_train_test, saveMetrics = TRUE)
#data_train_test <- data_train_test[, lowvar$nzv == FALSE]

# keep only low-NA columns (<20%)
data_train_train <- data_train_train[, lapply( data_train_train, function(x) sum(is.na(x)) / length(x) ) < .2]

# remove column being predicted
# data_train_train <- data_train_train[-58]

# remove initial metadata columns
#data_train_train <- data_train_train[-c(1:5)]

# homogenize data types
# not sure what this is about - fuck it let's do some ML


# train decision tree model
set.seed(42)
control <- trainControl(method='cv', number = 5)
fit_DecisionTrees <- train(classe ~ ., method='rpart', data=data_train_train)
#fit_DecisionTrees <- rpart(classe ~ ., data=data_train_train, method="class")
#fancyRpartPlot(fit_DecisionTrees)

# do prediction against "test", get confusion matrix
prediction_DT <- predict(fit_DecisionTrees, data_train_test, type='class')
conmat_DT <- confusionMatrix(prediction, data_train_test$classe)

# train random forest model
set.seed(42)
fit_RandomForest <- randomForest(classe ~ ., data=data_train_train, method='class', importance=TRUE)
prediction_RF <- predict(fit_RandomForest, data_train_test, type='class', importance=TRUE, ntree=1000)
conmat_RF <- confusionMatrix(prediction_DT, data_train_test$classe)

plot(fit_RandomForest)
varImpPlot(fit_RandomForest)

