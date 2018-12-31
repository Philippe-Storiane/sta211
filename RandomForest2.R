library("caret")
library("inTree")

source("Common.R")

data.caretMissForest=data.prepare()

#data.caretMissForest[,"s_sbp"]= data.caretMissForest[,"sbp"] - data.caretMissForest[,"dbp"]
#data.caretMissForest[,"sbp"] = NULL
#data.caretMissForest[,"dbp"] = NULL
#data.caretMissForest[,"egfr"] = NULL
#data.caretMissForest[,"copd"] = NULL
train.control <- trainControl(method = "cv", number = 10)
# model <- caret::train(lvefbin ~., data = d, method = "rf", trControl = train.control)
model <- caret::train(lvefbin ~., data = data.caretMissForest, method = "rfRules", trControl = train.control,
                      ntree=50, maxdepth=5)
output <- predict(model, newdata = data.caretMissForest, type="raw")
erreur_apprentissage <- output.rate(data.caretMissForest$lvefbin, output)
print(erreur_apprentissage)
varImp(model)

file_test <- "data_test.rda"
load(file_test)
imputed_data_test <- missMDA::imputeFAMD(data_test, ncp = 5)
d_test <- imputed_data_test$completeObs # [,c("gender", "bmi", "age", "sbp", "hr", "hypertension", "previoushf", "cad")]
pred_test <- predict(model, type = "raw", newdata = d_test)
write(as.character(pred_test), file = "20181126_kfold_rf_pred.csv")
read_pred("20181126_kfold_rf_pred.csv")