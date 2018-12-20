library("CatEncoders")


label_encode <- function(X, name) {
  labelEncoder <- LabelEncoder.fit(X[, name])
  z <- transform(labelEncoder,X[, name])
  X[,name]<- z
  return(X)  
}

one_hot_encode <- function(X, name) {
  oneHotEncoder <- OneHotEncoder.fit(data.frame(X[, name]))
  z <- transform(oneHotEncoder,data.frame(X[, name]),sparse=FALSE)
  classes <- slot(slot(oneHotEncoder, "column_encoders")[[1]], "classes")
  colnames(z) <- classes
  X <- cbind(X, z)
  X[,name]<- NULL
  return(X)  
}


for(i in c("gender", "copd", "hypertension", "previoushf", "afib", "cad" )) {
  X_all <- label_encode(X_all, i)
}

X_all[,quanti] <- scale(X_all[,quanti])


# c("gender", "bmi", "age", "sbp", "hr", "hypertension", "previoushf", "cad")
# sup = "egfr", "sbp", "dbp", "hr" "centre", "country", "copd",  "afib"
X_all[,"lvefbin"] <- NULL
X_all[,"centre"] <- NULL
X_all[,"country"] <- NULL
X_all[,"copd"] <- NULL
X_all[,"afib"] <- NULL
X_all[,"s_dbp"] <- X_all[,"sbp"] - X_all[,"dbp"]
X_all[,"dbp"] <- NULL
X_all[,"sbp"] <- NULL



som <- SOMbrero::trainSOM(x.data = X_all, dimension = c(3, 3), verbose=TRUE, nb.save=50)
plot(som, what="add", type="pie", variable=X[,"lvefbin"])

# https://cran.r-project.org/web/packages/SOMbrero/vignettes/doc-numericSOM.html
plot(som, what="energy")

plot(som, what="obs", type="hitmap")
plot(som, what="prototypes", type="color", var=1, main="prototypes - x1")
plot(som, what="prototypes", type="lines", print.title=TRUE)
plot(som, what="obs", type="names", print.title=TRUE, scale=c(0.9,0.5))
plot(som, what="prototypes", type="umatrix")
plot(som, type="radar", key.loc=c(-0.5,5), mar=c(0,10,2,0))
