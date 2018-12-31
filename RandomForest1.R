source("Common.R")

data.missForest=data.prepare()
cols.train=setdiff(cols,c("centre","lvefbin"))
print("Training with data")
print(cols.train)
data.train=data.missForest[,cols.train]
data.output=data.missForest[,c("lvefbin")]
model = randomForest(x=data.train, y=data.output, method="class",  ntree = 200)  


file_test = "data_test.rda"
load(file_test)
imputed_data_test <- missMDA::imputeFAMD(data_test, ncp = 5)
d_test <- imputed_data_test$completeObs # [,c("gender", "bmi", "age", "sbp", "hr", "hypertension", "previoushf", "cad")]
pred_test <- predict(model, type = "vote", newdata = d_test)

pred_test <- pred_test[,"good"] > 0.5
pred_test[pred_test == TRUE] <- "good"
pred_test[pred_test == FALSE] <- "bad"

write(as.character(pred_test), file = "20181126_grid_search_rf_pred.csv")
read.pred("20181126_grid_search_rf_pred.csv")