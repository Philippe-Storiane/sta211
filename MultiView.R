source("Common.R")

library("ade4")
library("FactoMineR")
library("factoextra")

# imputation toutes variables
# expl.cols=union(setdiff(quali_all,c("lvefbin")),setdiff(quanti_all,c("lvef")))
# imputation toutes variables hors pays
expl.cols.quali = setdiff(quali_all,c("lvefbin","country"))
expl.cols.quanti = setdiff(quanti_all,c("lvef"))
expl.cols=union( expl.cols.quali, expl.cols.quanti)
# imutation toutes variables hors centre et pays
# expl.cols=union(setdiff(quali_all,c("lvefbin","country","centre")),setdiff(quanti_all,c("lvef")))
# imputation toutes variables centre et pays combiné
# expl.cols=union(setdiff(quali_all,c("lvefbin","centre_country")),setdiff(quanti_all,c("lvef")))
data.miss = extend.miss(expl.cols, data_train)
data.miss = extend.miss.combine( "quanti", expl.cols.quanti, data.miss)
data.miss = extend.miss.combine( "quali", expl.cols.quali, data.miss)
data.test=data.prepare(data_train, expl.cols)
data.test[,"quanti_miss"]=data.miss[,"quanti_miss"]
data.test[,"quali_miss"]=data.miss[,"quali_miss"]



data.pca.cols=setdiff(quanti_all, "lvef")
data.pca= dudi.pca(data.test[data.pca.cols],nf=5, scannf=FALSE)
data.pca.within= wca(data.pca, data.test$centre, scannf = FALSE)
data.pca.between = bca(data.pca, data.test$centre, scannf = FALSE)
data.pca.ktab = ktab.within(data.pca.within)
data.pca.statis = statis(data.pca.ktab, scann = FALSE)


# data.mca.cols=setdiff(quali_all, c("lvefbin"))
# data.mca.cols=setdiff(quali_all, c("lvefbin","country"))
data.mca.cols=setdiff(quali_all, c("lvefbin","country","centre"))
data.mca = dudi.acm(data.test[data.mca.cols], nf = 10, scannf = FALSE)
data.mca.within = wca( data.mca, data.test$centre, scannf = FALSE)
data.mca.between = bca(data.mca, data.test$centre, scannf = FALSE)
data.mca.ktab = ktab.within(data.mca.within)
data.mca.statis = statis(data.mca.ktab,scann=FALSE)

data.famd.cols=union(data.pca.cols,data.mca.cols)
# data.famd.cols=union(data.famd.cols, c("quanti_miss", "quali_miss"))
data.famd = dudi.mix(data.test[data.famd.cols], nf = 15, scannf = FALSE)
data.famd.within = wca( data.famd, data.test$centre, scannf = FALSE)
data.famd.between = bca(data.famd, data.test$centre, scannf = FALSE)
data.famd.ktab = ktab.within(data.famd.within)
data.famd.statis = statis(data.famd.ktab,scann=FALSE)


data.split= split(data.test, data_train$centre)
data.split.pca =  lapply(data.split, function(x) data.frame(t(scalewt(x[data.pca.cols]))))
data.split.pca.ktab = ktab.list.df(data.split.pca)
data.split.pca.statis = statis(data.split.pca.ktab)

data.centre = data.frame("centre"=character(0),"C1.1"= numeric(0), "C2.1"=numeric(0), "C3.1"=numeric(0))
# data.centre = data.frame("centre"=character(0),"C1.1"= numeric(0), "C2.1"=numeric(0), "C3.1"=numeric(0), "C1.2"= numeric(0), "C2.2"=numeric(0), "C3.2"=numeric(0)) # ,  "C1.3"= numeric(0), "C2.3"=numeric(0), "C3.3"=numeric(0)  , "C1.4"= numeric(0), "C2.4"=numeric(0), "C3.4"=numeric(0))
for ( centre in levels(data_train$centre)) {
  centre.name1=paste0(centre,".1")
  centre.name2=paste0(centre,".2")
  centre.name3=paste0(centre,".3")
  centre.name4=paste0(centre,".4")
  data.centre.line = data.frame(
    "centre"=centre,
    "C1.1" = data.famd.statis$C.T4[ centre.name1, "C1"],
    "C2.1" = data.famd.statis$C.T4[ centre.name1, "C2"],
    "C3.1" = data.famd.statis$C.T4[ centre.name1, "C3"] #,
    #        "C1.2" = data.famd.statis$C.T4[ centre.name2, "C1"],
    #        "C2.2" = data.famd.statis$C.T4[ centre.name2, "C2"],
    #        "C3.2" = data.famd.statis$C.T4[ centre.name2, "C3"],
    #        "C1.3" = data.famd.statis$C.T4[ centre.name3, "C1"],
    #        "C2.3" = data.famd.statis$C.T4[ centre.name3, "C2"],
    #        "C3.3" = data.famd.statis$C.T4[ centre.name3, "C3"],
    #        "C1.4" = data.famd.statis$C.T4[ centre.name4, "C1"],
    #        "C2.4" = data.famd.statis$C.T4[ centre.name4, "C2"],
    #        "C3.4" = data.famd.statis$C.T4[ centre.name4, "C3"]
  )
  data.centre = rbind(data.centre, data.centre.line)
}
data.centre.pca = PCA(data.centre, quali.sup=c(1), graph= FALSE)
data.centre.hcpc=HCPC(data.centre.pca,graph=FALSE)
fviz_cluster(data.centre.hcpc)