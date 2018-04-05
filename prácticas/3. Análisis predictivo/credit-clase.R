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

