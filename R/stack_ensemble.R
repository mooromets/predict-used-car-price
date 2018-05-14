library(caret)

source("R/ML_utils.R")

set.seed(1357)
trainSet <- VWSample(4000)

set.seed(7531)
testSet <- VWSample(1000)

outcome <- c("model") # Y column
predictors <- colnames(trainSet)[!colnames(trainSet) %in% outcome] # X columns

preProc <- c("center", "scale")

# train 3 models of the low level
control <- trainControl(method="cv", 
                        number=6, 
                        savePredictions = 'final', 
                        classProbs = T)

model_rf <- train(x = trainSet[, predictors], 
                  y = trainSet[, outcome], 
                  method = "rf", 
                  trControl = control)

model_knn <- train(trainSet[, predictors], 
                   trainSet[, outcome], 
                   method = "knn", 
                   trControl = control, 
                   preProcess = preProc)

model_c50 <- train(trainSet[, predictors], 
                   trainSet[, outcome], 
                   method = "C5.0", 
                   trControl = control)

#add low-level predictions to BOTH datasets
trainSet$OOF_pred_rf <- model_rf$pred$pred[order(model_rf$pred$rowIndex)]
trainSet$OOF_pred_knn <- model_knn$pred$pred[order(model_knn$pred$rowIndex)]
trainSet$OOF_pred_c50 <- model_c50$pred$pred[order(model_c50$pred$rowIndex)]

testSet$OOF_pred_rf <- predict(model_rf, testSet[, predictors])
testSet$OOF_pred_knn <- predict(model_knn, testSet[, predictors])
testSet$OOF_pred_c50 <- predict(model_c50, testSet[, predictors])

predictors_top<-c('OOF_pred_rf', 'OOF_pred_knn', 'OOF_pred_c50')

# train top-level method
model_gbm <- train(trainSet[, predictors_top], 
                   trainSet[, outcome],
                   method='gbm',
                   trControl = control,
                   tuneLength = 3)


#predict using GBM top layer model
testSet$gbm_stacked <- predict(model_gbm, testSet[, predictors_top])

#check results
cMat <- confusionMatrix(testSet$gbm_stacked, testSet$model)

print(cMat$byClass)
print(cMat$overall)