library("FactoMineR")
library("factoextra")
library("missMDA")
library("dplyr")

source("Common.R")

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
# remove varibale to explain
load("data_muvar.rda")
data_cleaned=data.clean(data.muvar)
data1=imputePCA(data_cleaned[setdiff(quanti_all,c("lvef"))], ncp=5)
mca.data=cbind(data1$completeObs,data_cleaned[union(quali_all,c("lvef"))])
pca=PCA(mca.data, quali.sup=c(7,8,9,10,11,12,13,14,15),quanti.sup=16, scale.unit=TRUE,graph=FALSE)
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 50))
fviz_contrib(pca, choice=c("var"), axe=1)
fviz_pca_var(pca, select.var=list("cos2"=0.5),col.var="cos2",axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)

#
# Dump variable cos2 per center and dimension
#
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


#
# Dump variable PCA coordinatez per center
#
centre_pca=data.frame("centre"=character(0),"var"=character(0),"Dim.1"=numeric(0), "Dim.2"=numeric(0), "Dim.3"=numeric(0), "Dim.4"=numeric(0), "Dim.5"=numeric(0))
file_centre_pca_dacp="centre_pca_dacp.csv"
for(centre in levels(data.muvar$centre)) {
  var=as.character(centre)
  print(var)
  centre.data = data.muvar %>% filter( data.muvar[c("centre")]==var)
  centre.pca= PCA(centre.data,quali.sup=c(1,2,3,4,5,6,7,14,15), quanti.sup=c(16),scale.unit=TRUE, graph=FALSE)
  centre.dim = get_pca(centre.pca)
  print(centre.data[1,])
  
  for( quanti in setdiff(quanti_all,c("lvef"))) {
    line_pca=data.frame(
      "centre"=centre,
      "var"=quanti,
      "Dim.1"=centre.dim$coord[quanti,1],
      "Dim.2"=centre.dim$coord[quanti,2],
      "Dim.3"=centre.dim$coord[quanti,3],
      "Dim.4"=centre.dim$coord[quanti,4],
      "Dim.5"=centre.dim$coord[quanti,5])
    centre_pca=rbind(centre_pca,line_pca)
  }
}
dump_table(centre_pca,file_centre_pca_dacp)
pca_dacp=PCA(centre_pca,quali.sup=c(1,2), graph=FALSE)

data1=imputePCA(data_train[,c(4:9)])
data2=imputePCA(data_train[,c(4:10)])
datar=cbind(data1$completeObs,data2$completeObs[,c("lvef")])
pca_completed=PCA( datar, quanti.sup=7, graph=FALSE)
fviz_screeplot(pca_completed, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(pca_completed, col.var="cos2",axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)


#
# Multiple Correspondance Analysis
#
load("data_muvar.rda")
data_cleaned=data.clean()
data1=imputeMCA(data_cleaned[setdiff(quali_all,c("lvefbin","country"))])
data2=imputeMCA(data_cleaned[setdiff(quali_all,c("lvefbin"))])
data3=imputeMCA(data_cleaned[setdiff(quali_all,c("lvefbin","country","centre"))])

mca.data=cbind(data2$completeObs[setdiff(quali_all, c("lvefbin"))],data_cleaned[c("lvefbin")])
# country has not  been used in impuation model
mca.data_no_country=cbind(data1$completeObs[setdiff(quali_all, c("lvefbin","country"))],data_cleaned[c("lvefbin")])
# country has been used in impuation model
mca.data_no_country1=cbind(data2$completeObs[setdiff(quali_all, c("lvefbin","country"))],data_cleaned[c("lvefbin")])

# centre has been used in impuation model
mca.data_no_centre=cbind(data1$completeObs[setdiff(quali_all, c("centre","lvefbin","country"))],data_cleaned[c("lvefbin")])
# centre has not been used in imputation model
mca.data_no_centre1=cbind(data3$completeObs[setdiff(quali_all, c("centre","lvefbin","country"))],data_cleaned[c("lvefbin")])
# all data have been used in imputation model
mca.data_no_centre2=cbind(data2$completeObs[setdiff(quali_all, c("centre","lvefbin","country"))],data_cleaned[c("lvefbin")])


# Analysis without all variables excluding to explain
mca=MCA( mca.data_no_country, quali.sup=c(8), graph=FALSE, ncp=10)

# Analysis without country and center

mca_no_country=MCA( mca.data_no_country, quali.sup=c(8), graph=FALSE, ncp=20)
mca_no_centre=MCA( mca.data_no_centre, quali.sup=c(7), graph=FALSE, ncp=10)
mca_no_centre1=MCA( mca.data_no_centre1, quali.sup=c(7), graph=FALSE, ncp=10)

mca=MCA( mca.data, quali.sup=c(9), graph=FALSE, ncp=20)
fviz_screeplot(mca, addlabels = TRUE, ylim = c(0, 50), title="Toutes variables explicatives hors pays et centre")
fviz_contrib(mca,choice=c("var"),axe=1)
# Complemental variables removed (only describing variable with cos2 > 0.5)
fviz_mca_var(mca, select.var=list("cos2"=0.5),  invisible=c("quali.sup"), col.var="cos2",axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)
# without removing complemental variable
fviz_mca_var(mca, select.var=list("cos2"=0.4),   col.var="cos2",axes=c(1,2), title="Axe 1-2 Toutes variables explicatives hors pays et centre - 10 premières contributions") + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)


#
# Multiple Data Analysis
#
cols=union(setdiff(quali_all,c("lvefbin")),setdiff(quanti_all,c("lvef")))
data_cleaned=data.clean()
famd.impute = imputeFAMD(data_cleaned[cols],ncp=20)
famd.impute_no_centre=imputeFAMD(data_cleaned[setdiff(cols, c("centre","country"))], ncp=15)
famd.data=cbind(famd.impute$completeObs[setdiff(cols,c("centre","country"))],data_cleaned[c("lvef","lvefbin")])
famd.data_no_centre=cbind(famd.impute_no_centre$completeObs[setdiff(cols,c("centre","country"))],data_cleaned[c("lvef","lvefbin")])

famd=FAMD(famd.data, sup.var=c(13,14),ncp=15, graph=FALSE)
famd_no_centre=FAMD(famd.data_no_centre, ncp=15, graph=FALSE)
fviz_screeplot(famd_no_centre, addlabels = TRUE, ylim = c(0, 50))
fviz_contrib(famd,choice=c("var"),axe=1)
fviz_famd_var(famd, choice=c("quali.var"), select.var=list("contrib"=5),   col.var="cos2",axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)


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