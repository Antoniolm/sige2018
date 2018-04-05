library(caret)
library(tidyverse)
library(funModeling)
library(pROC)
library(partykit)
library(randomForest)

# Usando "German credit card data"
# http://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29
# Variable de clasificación: Class
data(GermanCredit)
data <- as_tibble(GermanCredit)
glimpse(data)
df_status(data)

set.seed(0)

## Crear modelo de predicción usando rpart
# Particiones entrenamiento / test
trainIndex <- # ...
train      <- # ...   
val        <- # ...

# Entrenar modelo para evaluación con ROC (usar probabilidades de las clases)
rpartCtrl <- trainControl( # ??? 
  )
rpartParametersGrid <- expand.grid( # ???
  )
rpartModel <- train(
  # ??? 
  data = train, 
  method = "rpart", 
  metric = "ROC", 
  trControl = rpartCtrl, 
  tuneGrid = rpartParametersGrid)

# Validacion
prediction     <- predict(rpartModel, val, type = "raw")
predictionProb <- predict(rpartModel, val, type = "prob")

auc <- roc(val$Class, predictionProb[["Good"]], levels = unique(val[["Class"]]))
roc_validation <- plot.roc(auc, ylim=c(0,1), type = "S" , print.thres = T, main=paste('Validation AUC:', round(auc$auc[[1]], 2)))

# Obtener valores de accuracy, precision, recall, f-score
results <- cbind(val, prediction)
results <- results %>%
  mutate(contingency = as.factor(
    case_when(
      ## ??? 
        ~ 'TP',
      ## ??? 
        ~ 'FP',
      ## ??? 
        ~ 'TN',
      ## ??? 
        ~ 'FN'))) 

TP <- length(which(results$contingency == 'TP'))
TN <- length(which(results$contingency == 'TN'))
FP <- length(which(results$contingency == 'FP'))
FN <- length(which(results$contingency == 'FN'))
n  <- length(results$contingency)

accuracy <- # ???
error    <- # ???

precision   <- # ???
sensitivity <- # ???
specificity <- # ???
f_measure   <- # ???

# Otro modelo utilizando rpart con cross-validation
rpartCtrl_2 <- trainControl(
  verboseIter = F, 
  classProbs = TRUE, 
  method = "repeatedcv",
  number = 10,
  repeats = 1,
  summaryFunction = twoClassSummary)
rpartModel_2 <- train(Class ~ ., data = train, method = "rpart", metric = "ROC", trControl = rpartCtrl_2, tuneGrid = rpartParametersGrid)
print(rpartModel_2)
varImp(rpartModel_2)
dotPlot(varImp(rpartModel_2))

plot(rpartModel_2)
plot(rpartModel_2$finalModel)
text(rpartModel_2$finalModel)

partyModel_2 <- as.party(rpartModel_2$finalModel)
plot(partyModel_2, type = 'simple')

## Crear modelo de predicción usando rf
# Modelo básico, ajuste manual
rfCtrl <- trainControl(verboseIter = F, classProbs = TRUE, method = "repeatedcv", number = 10, repeats = 1, summaryFunction = twoClassSummary)
rfParametersGrid <- expand.grid(.mtry = c(sqrt(ncol(train))))
rfModel <- train(Class ~ ., data = train, method = "rf", metric = "ROC", trControl = rfCtrl, tuneGrid = rfParametersGrid)
print(rfModel)
varImpPlot(rfModel$finalModel)
my_roc(val, predict(rfModel, val, type = "prob"), "Class", "Good")

# Modelo básico, ajuste manual en intervalo
rfParametersGrid <- expand.grid(.mtry = c(1:10))
rfModel <- train(Class ~ ., data = train, method = "rf", metric = "Accuracy", trControl = rfCtrl, tuneGrid = rfParametersGrid)
print(rfModel)
plot(rfModel)
plot(rfModel$finalModel)

# Modelo básico, ajuste con búsqueda aleatoria
rfCtrl <- trainControl(verboseIter = F, classProbs = TRUE, method = "repeatedcv", number = 10, repeats = 1, search = "random", summaryFunction = twoClassSummary)
rfModel <- train(Class ~ ., data = train, method = "rf", metric = "ROC", trControl = rfCtrl, tuneLength = 15)
print(rfModel)
plot(rfModel)

# Ajuste con tuneRF (Class es la columna 10)
bestmtry <- tuneRF(val[,-10], val[[10]], stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

# Ajuste del número de árboles
rfCtrl <- trainControl(verboseIter = F, classProbs = TRUE, method = "repeatedcv", number = 10, repeats = 1, summaryFunction = twoClassSummary)
rfParametersGrid <- expand.grid(.mtry = bestmtry[,1])

modellist <- list()
for (ntrees in c(1000, 1500, 2000, 2500)) {
  rfModel <- train(Class ~ ., data = train, method = "rf", metric= "ROC", tuneGrid = rfParametersGrid, trControl = rfCtrl, ntree = ntrees)
  key <- toString(ntrees)
  modellist[[key]] <- rfModel
}

results <- resamples(modellist)
summary(results)
dotplot(results)
bwplot(diff(results), metric = "ROC")

## Crear modelo de predicción usando SVM
svmCtrl <- trainControl(verboseIter = F, classProbs = TRUE, method = "repeatedcv", number = 10, repeats = 1, summaryFunction = twoClassSummary)
svmParametersGrid <- expand.grid(
  .sigma = seq(from = 0, to = 1, by = 0.5), 
  .C = seq(from = 0.1, to = 1, by = 0.5))
svmModel <- train(Class ~ ., data = train, method = "svmRadial", metric = "ROC", trControl = svmCtrl, tuneGrid = svmParametersGrid)
print(svmModel)
plot(svmModel)
my_roc(val, predict(svmModel, val, type = "prob"), "Class", "Good")



