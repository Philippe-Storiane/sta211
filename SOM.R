library("missMDA")

source("Common.R")



data.som=famd.impute$completeObs
for(quali in c("gender", "copd", "hypertension", "previoushf", "afib", "cad" )) {
  data.som = encode.label(data.som, quali)
}




# c("gender", "bmi", "age", "sbp", "hr", "hypertension", "previoushf", "cad")
# sup = "egfr", "sbp", "dbp", "hr" "centre", "country", "copd",  "afib"
data.som[,"lvefbin"] = NULL
data.som[,"centre"] = NULL
data.som[,"country"] = NULL
#data.som[,"copd"] = NULL
#data.som[,"lvef"] = NULL
#data.com[,"egfr"] = NULL
data.som[,"s_dbp"] <- data.som[,"sbp"] - data.som[,"dbp"]
data.som[,"dbp"] <- NULL
data.som[,"sbp"] <- NULL



som = SOMbrero::trainSOM(x.data =data.som, dimension = c(3,5), verbose=TRUE, nb.save=50)
plot(som, what="add", type="pie", variable=X[,"lvefbin"])

# https://cran.r-project.org/web/packages/SOMbrero/vignettes/doc-numericSOM.html
plot(som, what="energy")

plot(som, what="obs", type="hitmap")
plot(som, what="prototypes", type="color", var=1, main="prototypes - x1")
plot(som, what="prototypes", type="lines", print.title=TRUE)
plot(som, what="obs", type="names", print.title=TRUE, scale=c(0.9,0.5))
plot(som, what="prototypes", type="umatrix")
plot(som, type="radar", key.loc=c(-0.5,5), mar=c(0,10,2,0))
plot(som, what="prototypes", type="3d", variable=1)
