library("CatEncoders")

source("Variables.R")

load("data_train.rda")

famd.clean= function(datafr) {
  tt=levels(datafr$completeObs$centre)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$centre) <- tt
  
  #tt=levels(famd.data$completeObs$country)
  #tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  #eval.parent(substitute(levels(famd.data$completeObs$country) <- tt))
  
  tt=levels(datafr$completeObs$gender)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$gender)<- tt
  
  tt=levels(datafr$completeObs$hypertension)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$hypertension)<- tt
  
  tt=levels(datafr$completeObs$previoushf)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$previoushf)<- tt
  
  tt=levels(datafr$completeObs$afib)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$afib)<- tt
  
  tt=levels(datafr$completeObs$cad)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$cad)<- tt
  
  tt=levels(datafr$completeObs$copd)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$copd)<- tt
  
  return(datafr)
}


dump_table = function( data, file_name) {
  print(paste("Dumping table to file ", file_name))
  write.table(data, file=file_name, sep=";", row.names = FALSE, dec = ",", quote=FALSE)
}


  
data.clean= function (datafr=data_train) {
  bmi.min=13.4
  bmi.max=42.76
  age.min=31
  age.max=107
  egfr.min=13.99
  egfr.max=168.88
  sbp.min=50.5
  sbp.max=179.5
  dbp.min=41
  dbp.max=103
  hr.min= 46
  hr.max=176
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
  data_cleaned.quanti=scale(data_cleaned[quanti_all])
  data_cleaned=cbind(data_cleaned.quanti, data_cleaned[quali_all])
  return(data_cleaned)
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

