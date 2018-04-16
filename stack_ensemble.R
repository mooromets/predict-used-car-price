library(caret)
library(caretEnsemble)

source("R/ML_utils.R")

set.seed(1357)
dataset <- VWSample(4000)

#setup
seed <- 798
metric <- "Accuracy"
control <- trainControl(method="cv", number=4)
preProc <- c("center", "scale")


## learning 
results <- resamples(
  c(sapply(X = c("rf", "C5.0", "treebag", "PART", "J48"),
           FUN = defModelFit,
           seed = seed,
           form = model ~ .,
           data = dataset,
           metric = metric,
           trControl = control,
           simplify = FALSE,
           USE.NAMES = TRUE),
    sapply(X = c("svmRadial", "knn"),
           FUN = defModelFit,
           seed = seed,
           form = model ~ .,
           data = dataset,
           metric = metric,
           trControl = control,
           preProcess = c("center", "scale"),
           simplify = FALSE,
           USE.NAMES = TRUE)))

#summary(results)
#dotplot(results)

# correlation between results
modelCor(results)
splom(results)


#stacking
algorithmList <- c('C5.0', 'svmRadial')
models <- caretList(model ~ ., data=dataset, 
                    trControl=control, methodList=algorithmList)

# stack using glm
stackControl <- trainControl(method="cv", number=4, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
#ERROR: Not yet implemented for multiclass problems
#print(stack.glm)

set.seed(seed)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
#ERROR: Not yet implemented for multiclass problems
#print(stack.rf)