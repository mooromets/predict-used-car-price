library(randomForest)
library(caret)


source("./data-clean.R")
source("./ML_utils.R")

vw <- prepareBrandDataset(autoData(), "volkswagen")

#a smaller clean dataset
dataset <- remove0variance(fixMissingFactors(vw[sample(nrow(vw), 
                                                       size = 4000), 
                                                ]))

#setup
seed <- 8877
metric <- "Accuracy"
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
dotplot(results)
