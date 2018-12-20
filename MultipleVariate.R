library("FactoMineR")
library("factoextra")
library("missMDA")
library("dplyr")

source("Common.R")

cols=union(setdiff(quali_all, c("lvefbin","country")),setdiff(quanti_all,c("lvef")))
data_miss=data_train[cols]
#
# Imputing withh missForrest
#

missForest.data = missForest(data_miss, maxiter=7)
data.muvar=cbind(missForest.data$ximp.data,data_train[c("country", "lvefbin", "lvef")])
save(data.muvar, file="data_muv.rda")

#
# Imputing with missMDA
#
famd.data = imputeFAMD(data_miss, ncp = 20)
famd.data=famd.clean(famd.data)
data.muvar=cbind(famd.data$completeObs,data_train[c("country", "lvefbin", "lvef")])
save(data.muvar, file="data_muvar.rda")

#
# Principal Component Analysis
#
# remove varibale to explain and bmi because of high miss
load("data_muvar.rda")
pca=PCA(data_train, quali.sup=c(1,2,3,11,12,13,14,15,16),quanti.sup=10, scale.unit=TRUE,graph=FALSE)
pca=PCA(data.muvar,quali.sup=c(1,2,3,4,5,6,7,14,15), quanti.sup=c(16),scale.unit=TRUE, graph=FALSE)
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(pca, col.var="cos2",axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)

centre_pca=data.frame("centre"=character(0),"var"=character(0),"dim"=numeric(0), "cos2"=numeric(0))
file_centre_pca=sprintf("centre_pca.csv")
for(centre in levels(data.muvar$centre)) {
  var=as.character(centre)
  print(var)
  centre.data = data.muvar %>% filter( data.muvar[c("centre")]==var)
  centre.pca= PCA(centre.data,quali.sup=c(1,2,3,4,5,6,7,14,15), quanti.sup=c(16),scale.unit=TRUE, graph=FALSE)
  centre.dim = get_pca(centre.pca)
  print(centre.data[1,])
  for( quanti in setdiff(quanti_all,c("lvef"))) {
    for(index in 1:5) {
      dim=sprintf("Dim.%i",index)
      centre_pca=rbind(centre_pca,data.frame("centre"=centre,"var"=quanti,"dim"=index,"cos2"=centre.dim$cos2[quanti,dim]))
    }
  }
}
dump_table(centre_pca,file_centre_pca)

data1=imputePCA(data_train[,c(4:9)])
data2=imputePCA(data_train[,c(4:10)])
datar=cbind(data1$completeObs,data2$completeObs[,c("lvef")])
pca_completed=PCA( datar, quanti.sup=7, graph=FALSE)
fviz_screeplot(pca_completed, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(pca_completed, col.var="cos2",axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)


#
# Multiple Correspondance Analysis
#
data1=imputeMCA(data_train[c(1,2,3,12,13,14,15,16)])
datar=cbind(data1$completeObs,data_train[,c("lvefbin")])
mca=MCA( datar, quali.sup(11), graph=FALSE)
fviz_mca_var(mca, col.var="cos2",axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)

#
# Multiple Data Analysis
#
fviz_screeplot(famd, addlabels = TRUE, ylim = c(0, 50))
centre_pca=data.frame("centre"=character(0),"var"=character(0),"dim"=numeric(0), "cos2"=numeric(0))
file_centre_famd="centre_famd.csv"
for(centre in levels(data.muvar$centre)) {
  var=as.character(centre)
  print(var)
  centre.data = data.muvar %>% filter( data.muvar[c("centre")]==var)
  centre.famd= FAMD(centre.data,sup.var=c(1,14,15,16), graph=FALSE)
  centre.dim = get_famd(centre.famd, element=c("var"))
  print(centre.data[1,])
  for( var in setdiff(cols,c("centre"))) {
    for(index in 1:5) {
      dim=sprintf("Dim.%i",index)
      centre_famd=rbind(centre_famd,data.frame("centre"=centre,"var"=var,"dim"=index,"cos2"=centre.dim$cos2[var,dim]))
    }
  }
}
dump_table(centre_famd,file_centre_famd)