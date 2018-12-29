library("discretization")
library("FactoMineR")
library("missMDA")
library("factoextra")

source("Common.R")

cols.bin=c(paste0(quanti_all,"_bin"))

data_cleaned=data.clean()
famd.impute=imputeFAMD(data_cleaned[cols],ncp=20)
famd.data=cbind(famd.impute$completeObs[cols],data_cleaned[c("lvef","lvefbin")])
data_extended=data.extend(famd.data)
data_extended=extend.discretize.quantile(quanti_all,data_extended,2)


#
# Perfome MCA analysis
#
# remove variable without statistic dependance
discretize.mca=MCA(data_extended[union(union(cols.bin,quali_miss),c("gender","lvefbin"))],quali.sup=c(3,7,8,14),ncp=20,graph=FALSE)
# variable added as explaining
discretize.mca=MCA(data_extended[union(union(cols.bin,quali_miss),c("lvefbin"))],quali.sup=c(3,7,8),ncp=20,graph=FALSE)


fviz_screeplot(discretize.mca, addlabels = TRUE, ylim = c(0, 50), title="Qualitatives et qantitatives binarisées")
fviz_contrib(discretize.mca,choice=c("var"),axe=1)
# Complemental variables removed (only describing variable with cos2 > 0.5)
fviz_mca_var(discretize.mca, select.var=list("contrib"=10),col.var="cos2",axes=c(1,2),repel=TRUE) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)
