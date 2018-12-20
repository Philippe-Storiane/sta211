library("dplyr")
library("FactoMineR")
library("missMDA")
library("missForest")
library("VIM")
library("mice")

source("Common.R")
#
# basic analysis
#
printvariables = function( combination) {
	combination=as.character( combination )
	combination= unlist(strsplit(combination, ":"))
	value = ""

	for(index in 1:16) {
		if ( combination[ index] == "1") {
			if ( value != "") {
				value = paste(value,colnames(data_train)[index],sep="+")
			} else {
				value = colnames(data_train)[index]
			}
		}
	}
	return(value)
	
}

data_miss=aggr(data_train, combined=TRUE,prop=TRUE)
missing=data.frame(data_miss$combinations,data_miss$count,data_miss$percent)
colnames(missing)=c("combination", "count","percent")
missing[,c("variables")]=sapply(missing[, c("combination")], printvariables)
dump_table( missing, "missing.csv")




data_extended=data_train

miss.process = function( vars, data_extended) {
	for(var in vars ) {
		var_miss=sprintf("%s_miss", var)
		data_extended[,c(var_miss)] = ifelse(is.na(data_extended[,c(var)]),"miss","not miss")
		data_extended[,c(var_miss)]=factor(c("miss","not miss"))
	}
	return(data_extended)
}

outlier.dump=function(vars,dataf) {
	outliers=data.frame(
		"variable"=numeric(0),
		"boxplot_min"=numeric(0),
		"boxplot_max"=numeric(0),
		"quantile_min"=numeric(0),
		"quantile_max"=numeric(0),
		"hampel_min"=numeric(0),
		"hampel_max"=numeric(0)
	)
	for( var in vars) {
		var.quantile=quantile(dataf[,var],c(0.25,0.75), na.rm=TRUE)
		var.quantile.min=var.quantile[1]
		var.quantile.max=var.quantile[2]
		var.iqr = var.quantile.max - var.quantile.min
		var.boxplot.min = var.quantile.min - 1.5 *var.iqr
		var.boxplot.max = var.quantile.min + 1.5 *var.iqr
		var.quantile=quantile(dataf[,var],c(0.01,0.99), na.rm=TRUE)
		var.quantile.min= var.quantile[1]
		var.quantile.max= var.quantile[2]
		var.median = median(dataf[,var], na.rm=TRUE)
		var.mad=mad(dataf[,var], na.rm=TRUE)
		var.hampel.min= var.median - 3 * var.mad
		var.hampel.max= var.median + 3 * var.mad
		outliers=rbind(outliers,data.frame(
			"variable"=var,
			"boxplot_min"=var.boxplot.min,
			"boxplot_max"=var.boxplot.max,
			"quantile_min"=var.quantile.min,
			"quantile_max"=var.quantile.max,
			"hampel_min"=var.hampel.min,
			"hampel_max"=var.hampel.max
		))
	}
	dump_table(outliers, "outliers.csv")
}

data_extended = miss.process(quali_all, data_extended)
data_extended = miss.process(quanti_all,data_extended)
bmi.min=13.4
bmi.max=42.76
age.min=
age.max=
egfr.min=
egfr.max=
sbp.min=
sbp.max=
dbp.min=
dbp.max=
hr.min=
hr.max=
data_extended[,c("age_outlier")]=ifelse( is.na(data_extended[,c("age")]) | data_extended[,c("age")] > 105 ,"outlier","not outlier")
data_extended[,c("bmi_outlier")]=ifelse( (is.na(data_extended[,c("bmi")])| data_extended[,c("bmi")] > bmi.max) | (data_extended[,c("bmi")] < bmi.min) ,"outlier","not outlier")
data_extended[,c("hr_outlier")]=ifelse( is.na(data_extended[,c("hr")]) | is.na(data_extended[,c("age")])| data_extended[,c("hr")]  > 240 - data_extended[,c("age")] ,"outlier","not outlier")
data_extended[,c("asourcege_outlier")]=factor(c("outlier","not outlier"))
data_extended[,c("bmi_outlier")]=factor(c("outlier","not outlier"))
data_extended[,c("hr_outlier")]=factor(c("outlier","not outlier"))

#
# clean data in order to identity quality for inputing data
#
data_cleaned=na.omit(data_train)
data_cleaned[,c("bmi")]=ifelse(
	( data_cleaned[,c("bmi")] < bmi.min) | ( data_cleaned[, c("bmi")] > bmi.max),
	NA,
	data_cleaned[,c("bmi")])
data_cleaned.quanti=scale(data_cleaned[quanti_all])
data_cleaned=cbind(data_cleaned.quanti, data_cleaned[quali_all])
data_cleaned= na.omit( data_cleaned)


cols=union(setdiff(quali_all, c("lvefbin","country")),setdiff(quanti_all,c("lvef")))

data_cleaned=data_cleaned[cols]






impute_var= function(var){
  impute_algo=data.frame("var"=numeric(0),"test"=character(0),"method"=character(0), "NRMSE"=numeric(0), "PFC"=numeric(0))
  file_name=sprintf("impute_%s.csv", var)
  file_name_summary=sprintf("impute_%s_summary.csv", var)
  var.imp=paste(var,"imp",sep="_")
  cols=union(setdiff(quali_all, c("lvefbin","country")),setdiff(quanti_all,c("lvef")))
  cols=setdiff(cols,c(var))
  for(index in 1:50) {
    test=sprintf("test%i", index)
    print(test)
    print(miss.rate[[var]])
    data_miss= prodNA(data_cleaned, noNA = miss.rate[[var]])

    data_miss=cbind(data_cleaned[,cols], data_miss[,c(var)])
    colnames(data_miss)[length(colnames(data_miss))]=var
    data_cleaned=data_cleaned[union(cols, c(var))]
    
    print("Imputing with missMDA...")
    famd.data = imputeFAMD(data_miss, ncp = 20)
    famd.data=famd.clean(famd.data)
    err=mixError( famd.data$completeObs, data_miss, data_cleaned)
    print(err)
    impute_algo=rbind(impute_algo, data.frame("var"=var,"test"=test, "method"="missMDA", "NRMSE"=err["NRMSE"],"PFC"=err["PFC"]))
    
    
    
    #
    # missForest
    print("Imputing with missForest...")
    missForest.data = missForest(data_miss, maxiter=7)
    err=mixError(missForest.data$ximp, data_miss,data_cleaned)
    print(err)
    impute_algo=rbind(impute_algo, data.frame("var"=var,"test"=test, "method"="missForest", "NRMSE"=err["NRMSE"],"PFC"=err["PFC"]))
    
    #
    # VIM KNN
    #
    print("Imputing with VIM KNN...")
    kNN.data = kNN( data_miss, variable=c(var), dist_var=cols)
    var.imp=paste(var,"imp",sep="_")
    kNN.data=kNN.data[setdiff( colnames(kNN.data), c( var.imp))]
    err=mixError( kNN.data, data_miss, data_cleaned)
    print(err)
    impute_algo=rbind(impute_algo, data.frame("var"=var,"test"=test, "method"="missVIMKnn", "NRMSE"=err["NRMSE"],"PFC"=err["PFC"]))
    
    #
    # VIM irmi (Robust Regression Model Input)
    #
    print("Imputing with VIM irmi...")
    irmi.modelFormulas=list()
    irmi.modelFormulas[[var]]=cols
    print(cols)
    irmi.data=irmi(data_miss, modelFormulas=irmi.modelFormulas,mi=5)
    for(index in 1:5) {
      err=mixError( data.frame(irmi.data[index]), data_miss, data_cleaned)
      print(err)
      impute_algo=rbind(impute_algo, data.frame("var"=var,"test"=test, "method"="missVIMImri", "NRMSE"=err["NRMSE"],"PFC"=err["PFC"]))
    }
    
    #
    # mice
    #
    print("Imputing with mice...")
    mice.pred= quickpred(data_miss)
    #mice.pred["bmi","copd"]=0
    #mice.pred["bmi","country"]=0
    mice.data=mice(data_miss, predictorMatrix=mice.pred)
    for(index in 1:5) {
      err=mixError(complete(mice.data, index), data_miss, data_cleaned)
      print(err)
      impute_algo=rbind(impute_algo, data.frame("var"=var,"test"=test, "method"="missMice", "NRMSE"=err["NRMSE"],"PFC"=err["PFC"]))
    }
  }
  dump_table(impute_algo,file_name)
  impute_algo_summary=impute_algo %>% group_by_("method") %>% summarize(mean(PFC), sd(PFC), mean(NRMSE), sd(NRMSE))
  dump_table(impute_algo_summary,file_name_summary)
}




miss.rate=list()
miss.rate["bmi"]=0.2
miss.rate["age"]=0.02
miss.rate["egfr"]=0.02
miss.rate["sbp"]=0.02
miss.rate["dbp"]=0.02
miss.rate["hr"]=0.02
miss.rate["copd"]=0.02
miss.rate["previoushf"]=0.02
miss.rate["afib"]=0.02
miss.rate["cad"]=0.02
miss.rate["hypertension"]=0.02
for(var in names(miss.rate)) {
  print(sprintf("imputing %s", var))
  impute_var(var)
}

#
# new testing mode with all variables mixed with their simulated missing rate
#
cols=union(setdiff(quali_all, c("lvefbin","country")),setdiff(quanti_all,c("lvef")))
cols_bmi=setdiff(cols,c("bmi"))
data_cleaned=data_cleaned[cols]
data_miss_bmi=prodNA(data_cleaned, 0.2)
data_miss_other=prodNA(data_cleaned, 0.02)
data_miss=cbind(data_miss_other[cols_bmi], data_miss_bmi[c("bmi")])
data_cleaned=cbind(data_cleaned[cols_bmi],data_cleaned[c("bmi")])
impute_algo=data.frame("test"=character(0),"method"=character(0), "NRMSE"=numeric(0), "PFC"=numeric(0))
file_name=sprintf("impute_mixed.csv", var)
file_name_summary=sprintf("impute_mixed_summary.csv", var)

for(index in 1:5) {
  

  test=sprintf("test%i", index)
  print(test)
  data_miss_bmi=prodNA(data_cleaned, 0.2)
  data_miss_other=prodNA(data_cleaned, 0.02)
  data_miss=cbind(data_miss_other[cols_bmi], data_miss_bmi[c("bmi")])
  
  print("Imputing with missMDA...")
  famd.data = imputeFAMD(data_miss, ncp = 20)
  famd.data=famd.clean(famd.data)
  err=mixError( famd.data$completeObs, data_miss, data_cleaned)
  print(err)
  impute_algo=rbind(impute_algo, data.frame("test"=test, "method"="missMDA", "NRMSE"=err["NRMSE"],"PFC"=err["PFC"]))
    
    
    
  #
  # missForest
  print("Imputing with missForest...")
  missForest.data = missForest(data_miss, maxiter=7)
  err=mixError(missForest.data$ximp, data_miss,data_cleaned)
  print(err)
  impute_algo=rbind(impute_algo, data.frame("test"=test, "method"="missForest", "NRMSE"=err["NRMSE"],"PFC"=err["PFC"]))
    
    #
    # VIM KNN
    #
    print("Imputing with VIM KNN...")
    kNN.data = kNN( data_miss, dist_var=cols)
    kNN.data=kNN.data[cols]
    err=mixError( kNN.data, data_miss, data_cleaned)
    print(err)
    impute_algo=rbind(impute_algo, data.frame("test"=test, "method"="missVIMKnn", "NRMSE"=err["NRMSE"],"PFC"=err["PFC"]))
    
    #
    # VIM irmi (Robust Regression Model Input)
    #
    print("Imputing with VIM irmi...")
    print(cols)
    irmi.data=irmi(data_miss, mi=5)
    for(index in 1:5) {
      err=mixError( data.frame(irmi.data[index]), data_miss, data_cleaned)
      print(err)
      impute_algo=rbind(impute_algo, data.frame("test"=test, "method"="missVIMImri", "NRMSE"=err["NRMSE"],"PFC"=err["PFC"]))
    }
    
    #
    # mice
    #
    print("Imputing with mice...")
    mice.pred= quickpred(data_miss)
    #mice.pred["bmi","copd"]=0
    #mice.pred["bmi","country"]=0
    mice.data=mice(data_miss, predictorMatrix=mice.pred)
    for(index in 1:5) {
      err=mixError(complete(mice.data, index), data_miss, data_cleaned)
      print(err)
      impute_algo=rbind(impute_algo, data.frame("test"=test, "method"="missMice", "NRMSE"=err["NRMSE"],"PFC"=err["PFC"]))
    }

}
dump_table(impute_algo,file_name)
impute_algo_summary=impute_algo %>% group_by_("method") %>% summarize(mean(PFC), sd(PFC), mean(NRMSE), sd(NRMSE))
dump_table(impute_algo_summary,file_name_summary)


#
# VIM output
#
#
# layout of missing (all combined) for a given variable
# here 'bmi' (index 4 in variables for data_train)
#
# histMiss(data_train,pos=4)

#
# missing dependancy between two quantitative variables
# here 'bmi' vs 'egfr'
#
# marginplot(data_train[,c("egfr","bmi")])

#
# missing dependancy with two other qualitative variables
# here 'bmi' vs 'copd' et 'afib'
#
# mosaicMiss(data_train,plotvars=c("copd","afib"), highlight=c("bmi"))

#
# influence variable globale
# Here 'bmi'
#
# pbox(data_train,pos=4)

