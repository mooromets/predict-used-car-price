library(randomForest)
library(caret)

source("./data-clean.R")
source("./ML_utils.R")

vw <- prepareBrandDataset(autoData(), "volkswagen")

#a smaller clean dataset
dataset <- remove0variance(fixMissingFactors(vw[sample(nrow(vw), 
                                                       size = 4000), 
                                                ]))
seed <- 8877
metric <- "Accuracy"


# Random search
control <- trainControl(method="cv", number=4, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(model~., data=dataset, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


#grid search
control <- trainControl(method="cv", number=4, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(model~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


# Algorithm Tune (tuneRF)
set.seed(seed)
bestmtry <- tuneRF(dataset[, -6], dataset[, 6], stepFactor=2, improve=1e-5, ntree=500)
print(bestmtry)
plot(bestmtry[,1], bestmtry[,2], type = "l")


# Number of trees 
control <- trainControl(method="cv", number=4)
modellist <- sapply(c(100, 500, 750, 1000, 1250, 1500), 
                    function(x) {
                      set.seed(seed)
                      print(x)
                      print(system.time(
                        fit <- train(model ~ ., 
                              data = dataset,
                              method = "rf",
                              metric = metric,
                              trControl = control,
                              ntree = x)
                      ))
                      fit
                    },
                    simplify = FALSE,
                    USE.NAMES = TRUE)
results <- resamples(modellist)
summary(results)
plot(results)
#dotplot(results)

