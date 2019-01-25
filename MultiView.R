source("Common.R")

library("ade4")
library("factoextra")

# imputation toutes variables
expl.cols=union(setdiff(quali_all,c("lvefbin")),setdiff(quanti_all,c("lvef")))
# imputation toutes variables hors pays
expl.cols=union(setdiff(quali_all,c("lvefbin","country")),setdiff(quanti_all,c("lvef")))
# imutation toutes variables hors centre et pays
expl.cols=union(setdiff(quali_all,c("lvefbin","country","centre")),setdiff(quanti_all,c("lvef")))
# imputation toutes variables centre et pays combiné
expl.cols=union(setdiff(quali_all,c("lvefbin","centre_country")),setdiff(quanti_all,c("lvef")))
data.test=data.prepare(data_train, expl.cols)


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
data.famd = dudi.mix(data.test[data.famd.cols], nf = 15, scannf = FALSE)
data.famd.within = wca( data.famd, data.test$centre, scannf = FALSE)
data.famd.between = bca(data.famd, data.test$centre, scannf = FALSE)
data.famd.ktab = ktab.within(data.famd.within)
data.famd.statis = statis(data.famd.ktab,scann=FALSE)


data.split= split(data.test, data_train$centre)
data.split.pca =  lapply(data.split, function(x) data.frame(t(scalewt(x[data.pca.cols]))))
data.split.pca.ktab = ktab.list.df(data.split.pca)
data.split.pca.statis = statis(data.split.pca.ktab)