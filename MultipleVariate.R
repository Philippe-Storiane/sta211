library("FactoMineR")
library("factoextra")



#
# Principal Component Analysis
#
# remove varibale to explain and bmi because of high miss
pca=PCA(data_train, quali.sup=c(1,2,3,11,12,13,14,15,16),quanti.sup=10, scale.unit=TRUE,graph=FALSE)
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(pca, col.var="cos2",axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)

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
famd.data = imputeFAMD(data_miss)
famd=FAMD(famd.data$completeObs, ncp=20)
fviz_screeplot(famd, addlabels = TRUE, ylim = c(0, 50))