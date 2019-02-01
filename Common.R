library("CatEncoders")
library("missMDA")
library("missForest")
library("discretization")


source("Variables.R")

load("data_train.rda")
load("data_test.rda")

famd.clean= function(datafr=data_train, cols=quali_all) {
  for(var in cols) {
    if ( var %in% colnames(datafr)) {
      l <- levels(datafr[,var])
      l <- unlist(lapply(l, FUN = function(level) {gsub(".*_([0-9]*)", "\\1", level)}))
      datafr[, var] <- as.factor(as.numeric(datafr[, var]))
      levels(datafr[, var]) <- l
    }
  }
  return(datafr)
}


dump_table = function( data, file_name) {
  print(paste("Dumping table to file ", file_name))
  write.table(data, file=file_name, sep=";", row.names = FALSE, dec = ",", quote=FALSE)
}



data.clean= function (datafr=data_train) {
  bmi.q = quantile(data_train[,"bmi"],probs=c(0.01,0.99), na.rm=TRUE)
  bmi.min=bmi.q[[1]]
  bmi.max=bmi.q[[2]]
  #bmi.min=13.4
  #bmi.max=42.76
  age.q = quantile(data_train[,"age"],probs=c(0.01,0.99), na.rm=TRUE)
  age.min=age.q[[1]]
  age.max=age.q[[2]]
  #age.min=31
  #age.max=107
  egfr.q = quantile(data_train[,"egfr"],probs=c(0.01,0.99), na.rm=TRUE)
  egfr.min=egfr.q[[1]]
  egfr.max=egfr.q[[2]]
  #egfr.min=13.99
  #egfr.max=168.88
  sbp.q = quantile(data_train[,"sbp"],probs=c(0.01,0.99), na.rm=TRUE)
  sbp.min=sbp.q[[1]]
  sbp.max=sbp.q[[2]]
  #sbp.min=50.5
  #sbp.max=179.5
  dbp.q = quantile(data_train[,"dbp"],probs=c(0.01,0.99), na.rm=TRUE)
  dbp.min=dbp.q[[1]]
  dbp.max=dbp.q[[2]]
  #dbp.min=41
  #dbp.max=103
  hr.q = quantile(data_train[,"hr"],probs=c(0.01,0.99), na.rm=TRUE)
  hr.min=hr.q[[1]]
  hr.max=hr.q[[2]]
  #hr.min= 46
  #hr.max=176
  if ( ! ( "lvef" %in% colnames(datafr))) {
    quanti_all=setdiff(quanti_all,c("lvef"))
  }
  if ( ! ( "lvefbin" %in% colnames(datafr))) {
    quali_all=setdiff(quali_all,c("lvefbin"))
  }
  data_cleaned = datafr
  data_cleaned[,c("bmi")]=ifelse(
    ( is.na(data_cleaned[,c("bmi") ]))| ( data_cleaned[,c("bmi")] < bmi.min) | ( data_cleaned[, c("bmi")] > bmi.max),
    NA,
    data_cleaned[,c("bmi")])
  data_cleaned[,c("age")]=ifelse(
    ( is.na(data_cleaned[,c("age") ]))| ( data_cleaned[,c("age")] < age.min) | ( data_cleaned[, c("age")] > age.max),
    NA,
    data_cleaned[,c("age")])
  data_cleaned[,c("egfr")]=ifelse(
    ( is.na(data_cleaned[,c("egfr") ]))| ( data_cleaned[,c("egfr")] < egfr.min) | ( data_cleaned[, c("egfr")] > egfr.max),
    NA,
    data_cleaned[,c("egfr")])
  data_cleaned[,c("sbp")]=ifelse(
    ( is.na(data_cleaned[,c("sbp") ]))| ( data_cleaned[,c("sbp")] < sbp.min) | ( data_cleaned[, c("sbp")] > sbp.max),
    NA,
    data_cleaned[,c("sbp")])
  data_cleaned[,c("dbp")]=ifelse(
    ( is.na(data_cleaned[,c("dbp") ]))| ( data_cleaned[,c("dbp")] < dbp.min) | ( data_cleaned[, c("dbp")] > dbp.max),
    NA,
    data_cleaned[,c("dbp")])
  data_cleaned[,c("dbp")]=ifelse(
    ( is.na(data_cleaned[,c("dbp") ]))| ( data_cleaned[,c("dbp")] < dbp.min) | ( data_cleaned[, c("dbp")] > dbp.max),
    NA,
    data_cleaned[,c("dbp")])
  data_cleaned[,c("hr")]=ifelse(
    ( is.na(data_cleaned[,c("hr") ]))| ( data_cleaned[,c("hr")] < hr.min) | ( data_cleaned[, c("hr")] > hr.max),
    NA,
    data_cleaned[,c("hr")])
  # data_cleaned.quanti=scale(data_cleaned[quanti_all])
  # data_cleaned=cbind(data_cleaned.quanti, data_cleaned[quali_all])
  return(data_cleaned)
}

data.prepare = function(datafr=data_train,cols=union(setdiff(quali_all,c("lvefbin","country")),setdiff(quanti_all,c("lvef")))) {
  data.nrow = nrow(datafr)
  if ( nrow(datafr) != nrow(data_train)) {
    datafr=rbind(datafr, data_train[colnames(datafr)])  
  }
  data_cleaned=data.clean(datafr)
  data_cleaned=datafr
  quanti_all = intersect(quanti_all, colnames(data_cleaned))
  quali_all = intersect(quali_all, colnames( data_cleaned ))
  # data_cleaned=cbind(scale(data_cleaned[quanti_all]),data_cleaned[quali_all])
  data_cleaned=cbind(data_cleaned[quanti_all],data_cleaned[quali_all])
  data_cleaned[,"centre_country"] = as.factor(paste0(data_cleaned[,"centre"],"_",data_cleaned[,"country"]))
  famd.impute = imputeFAMD(data_cleaned[cols],ncp=5)
  data.result=famd.impute$completeObs[1:data.nrow,]
  data.result=famd.clean(data.result)
  #missForest.impute = missForest( data_cleaned[cols])
  #data.result = missForest.impute$ximp[1:data.nrow,]
  for( col in c("lvefbin", "lvef","country","centre")) {
    if ( col %in% colnames(data_cleaned)) {
      if ( ! ( col %in% colnames(data.result))) {
        data.result[,col]=data_cleaned[1:data.nrow, col]
      }      
    }
  }
  data.result[,"centre_country"] = as.factor(paste0(data.result[,"centre"],"_",data.result[,"country"]))
  data.result[,"s_sbp"] = data.result[, "sbp"] - data.result[,"dbp"]
  data.result=cbind(scale(data.result[ union(quanti_all,c("s_sbp")) ]), data.result[ union(quali_all,c("centre_country")) ])
  return(data.result)
}


extend.miss = function( vars, data_extended) {
  for(var in vars ) {
    var_miss=sprintf("%s_miss", var)
    data_extended[,c(var_miss)] = ifelse(is.na(data_extended[,c(var)]),"miss","not miss")
    data_extended[,c(var_miss)]=factor(data_extended[,c(var_miss)],levels=c("miss","not miss"))
  }
  return(data_extended)
}


extend.miss.combine = function( target_var, combined_miss, data_extended) {
  target_var=paste0(target_var,"_miss")
  data_extended[,c(target_var)] = "not miss"
  for(miss in combined_miss) {
    miss=as.character(miss)
    miss_var=paste0(miss,"_miss")
    data_extended[,c(target_var)]=ifelse(
      ( data_extended[,c(target_var)] == "miss" ) |
        ( data_extended[,c(miss_var)]=="miss"),
      "miss",
      "not miss"
    )
  }
  data_extended[,c(target_var)] = factor(data_extended[,c(target_var)],levels=c("miss","not miss"))
  return( data_extended )
}

data.extend = function (datafr=data_cleaned) {
  data_extended = extend.miss(quali_miss, datafr)
  data_extended = extend.miss(quanti_miss,data_extended)
  data_extended=extend.miss.combine("all",union(quali_miss, quanti_miss), data_extended)
  data_extended=extend.miss.combine("not_bmi",setdiff(union(quali_miss, quanti_miss),c("bmi")), data_extended)
  #  data_extended=extend.discretize.quantile(quanti_all,data_extended)
  return(data_extended)
}

#
# cut by quantile
#
extend.discretize.quantile = function(quantis, datafr, n=4) {
  data_extended = datafr
  for(quanti in quantis) {
    quanti.var=paste0(quanti,"_bin")
    quanti.breaks = quantile(datafr[,quanti], probs=seq(0,1, 1/n), na.rm=TRUE)
    data_extended[,quanti.var] = cut(datafr[,quanti],breaks=quanti.breaks, include.lowest = TRUE,ordered_result = TRUE, labels=paste0(quanti,"_",seq(1:n)))
  }
  return(data_extended)
}

extend.discretize.mdlp = function( quantis, datafr) {
  data_extended = datafr
  for(quanti in quantis) {
    quanti.data = na.omit(data_train[,c(quanti, "lvefbin")])
    x = quanti.data[,quanti]
    y = quanti.data[,"lvefbin"]
    quanti.cut = cutPoints(x,y)
    quanti.breaks=c(min(x),quanti.cut,max(x))
    quanti.var=paste0(quanti,"_bin")
    n=length(quanti.breaks) - 1
    data_extended[,quanti.var] = cut(datafr[,quanti],breaks=quanti.breaks, include.lowest = TRUE,ordered_result = TRUE, labels=paste0(quanti,"_",seq(1:n)))
  }
  return(data_extended)
}



encode.label <- function(X, name) {
  labelEncoder <- LabelEncoder.fit(X[, name])
  z <- transform(labelEncoder,X[, name])
  X[,name]<- z
  return(X)  
}

encode.one_hot <- function(X, name) {
  oneHotEncoder <- OneHotEncoder.fit(data.frame(X[, name]))
  z <- transform(oneHotEncoder,data.frame(X[, name]),sparse=FALSE)
  classes <- slot(slot(oneHotEncoder, "column_encoders")[[1]], "classes")
  colnames(z) <- paste0(name, classes)
  X <- cbind(X, z)
  X[,name]<- NULL
  return(X)  
}

output.check = function(file, n = nrow(data_test)) { 
  y_pred = scan(file, what = "character") 
  y_pred = factor(y_pred, levels = c("bad", "good")) 
  if (length(y_pred) != n) 
    stop("incorrect number of predictions") 
  if (any(is.na(y_pred))) 
    stop("predictions contain missing values (NA)") 
  return(y_pred) 
}

output.rate = 
  function(y_pred, y_test){ 
    FP = (y_pred == "bad") & (y_test == "good") 
    FN = (y_pred == "good") & (y_test == "bad") 
    return(sum(FP+FN)/length(y_test)) 
  }

output.rate.confusion = function(data, pred) {
  table_confusion <- table(data, pred)
  1.0 - (table_confusion[1,1] + table_confusion[2,2])/sum(table_confusion)
}