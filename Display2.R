library("dplyr") 
library("ggplot2") 
library("ggpubr") 
library("GGally") 
library("PerformanceAnalytics")
library("grid")
library("gridExtra")

  
source("Common.R")
quanti_expl=c("bmi","age","egfr","sbp","dbp", "hr") 
quanti_all=c("bmi","age","egfr","sbp","dbp", "hr", "lvef")
quali_expl=c("centre","country","gender","copd","hypertension","previoushf","afib","cad")
quali_all=c("centre","country","gender","copd","hypertension","previoushf","afib","cad", "lvefbin")



quali.pie = function ( quali ) { 
    quali.data = data_train %>% group_by_(quali ) %>% summarise(count=n()) %>% mutate(prop=round(count*100/sum(count), 1))  
    ggpie(quali.data, x="prop",label="prop", lab.pos="out",fill=quali, color="white")
} 

  

quali.histo = function( quali ) { 
    quali.data = data_train %>% group_by_(quali ) %>% summarise(count=n()) %>% mutate(prop=round(count*100/sum(count), 1))  
    ggplot(quali.data, aes_string(x=quali, y="prop"))+geom_bar(fill = "#0073C2FF", stat = "identity") + geom_text(aes_string(label = "prop"), vjust = -0.3) + theme_pubclean()  
} 

 

 

quanti.histo = function( quanti, bin ) { 
    number_of_missigin_values =  sum(is.na(data_train[, quanti]))  
    vector_size = length(data_train[,quanti])  
    quanti.mean=round( mean(data_train[,quanti], na.rm=TRUE), 2)  
    quanti.sd=round( sd(data_train[,quanti], na.rm=TRUE), 2) 
    quanti.miss= round(100 * number_of_missigin_values/vector_size, 2)  
    quanti.title=paste("Moyenne: " , quanti.mean, ", Ecart Type: " , quanti.sd, ", Manquant: ", quanti.miss, " %" )  
    gghistogram(data_train, x=quanti, y="..density..", add="mean", binwidth=bin,add_density=TRUE, title=quanti.title, rug=TRUE, fill="lightgray", add.params=list(size=0.8, linetype="dashed") ) 
} 

quanti.correlation = function() {
	ggcorr(data_train, label = TRUE, nbreak = 7)
}

quanti.pairs = function() {
##	ggpairs(data_train, columns=quanti_all)
	chart.Correlation(data_train[,quanti_expl], histogram=TRUE)	
}


quanti.scatter= function( var1, var2) {
	ggscatter(data_train,x=var1,y=var2, add="reg.line", conf.int=TRUE, cor.coefficient=TRUE, cor.method="pearson", add.params=list(color="red"))
}


quali.correlation = function(quali = quali_all, datafr=data_train) {
	quali_test=data.frame("quali1"=character(0), "quali2"=character(0), "chi-test"=numeric(0))
	#
	# Using Yvan code...
	#	
	for(i in seq(1,length(quali) -1)) { 
	    for(j in seq(i+1, length(quali))) { 
	    	u = quali[i] 
		v = quali[j] 

		# Tableau de contingence 
	    	tbl = table(datafr[,c(u)], datafr[,c(v)]) 

		# Test du X2 d'independance 
		# (H0) = les deux variables X et Y sont independantes. 
	    	# (H1) = les deux variables ne sont pas independantes 

	    	# L'hypothese (H0) peut etre rejete avec un risque d'erreur de p < 5% 
	    	res = chisq.test(tbl)
	    	quali_test=rbind(quali_test, data_frame("quali1" = u,"quali2" = v,"chi-test" = res$p.value))
	     }
	}
	dump_table( quali_test, "quali.csv")
	# write.table( quali_test, file="quali.csv", sep=";", quote=FALSE, row.names=FALSE,dec=",")
} 


# var1="centre"
# var2="bmi_miss"
# data1=data_extended
# quali_table=table(data1[,var1], data1[,var2], dnn=list(var1,var2))
# quali_frame = as.data.frame(quali_table)
# ggballoonplot( quali_frame, x=var1, y=var2, fill="Freq", ggtheme= theme_bw())+ scale_fill_viridis_c( option="C")
quali.balloon = function(var1, var2,data1) {
	print(sprintf("%s / %s", var1,var2))
	quali_table=table(data1[,var1], data1[,var2], dnn=list(var1,var2))
	quali_frame = as.data.frame(quali_table)
	print(summary( quali_frame))
	ggballoonplot( quali_frame, x=var1, y=var2, fill="Freq", ggtheme= theme_bw()) # + scale_fill_viridis_c( option="C")	
	file_name = sprintf("%s_%s.csv", var1,var2)
	dump_table( quali_frame, file_name)
}

quali.mosaicplot = function(var1, var2,data1) {
	print(sprintf("%s / %s", var1,var2))
	quali_table=table(data1[,var1], data1[,var2], dnn=list(var1,var2))
	mosaicplot( quali_table, shade=TRUE)
}

quali_quanti.correlation= function( quali=quali_all, quanti=quanti_all,datafr=data_train) {
	# Lien entre variables quali & quanti 
	# (H0) = {le facteur n?a pas d?effet sur Y }  
	# (H1) = {il existe un effet du facteur sur Y} 
	# (H0) est rejet?e si la probabilit? est plus petite que le seuil a 
	quali_quanti =data.frame("quali"=character(0), "quanti"=character(0), "p-test"=numeric(0))
	for(i in quali) { 
	  for(j in quanti) { 
	    group = datafr[, i] 
	    fit = aov(datafr[,j] ~ datafr[,i], datafr[,c(i, j)]) 
	    # print(paste(i, "-", j)) 	    
    	# print(summary(fit))
    	summ = summary( fit)
    	quali_quanti = rbind( quali_quanti, data.frame("quali"=i, "quanti"=j, "p-test"=summ[[1]]$'Pr(>F)'[1]))
	  } 
	}
	dump_table( quali_quanti, "quali_quanti.csv")
	# write.table(quali_quanti, file="quali_quanti.csv", sep=";", quote=FALSE, row.names=FALSE,dec=",")
}

quali_quanti.boxplot = function(quali, quanti) {
	ggboxplot(data_train, x=quali, y=quanti, color=quali)
}

