source("Common.R")

library("ade4")
library("factoextra")

data.test = data.prepare(data_train)
data.pca.cols=setdiff(quanti_all, "lvef")
data.pca= dudi.pca(data.test[data.pca.cols],nf=5, scannf=FALSE)
data.pca.within= wca(data.pca, data.test$centre, scannf = FALSE)
data.pca.between = bca(data.pca, data.test$centre, scannf = FALSE)
data.pca.ktab = ktab.within(data.pca.within)
data.pca.statis = statis(data.pca.ktab, scann = FALSE)


data.mca.cols=setdiff(quali_all, c("lvefbin","centre"))
data.mca = dudi.acm(data.test[data.mca.cols], nf = 10, scannf = FALSE)
data.mca.within = wca( data.mca, data.test$centre, scannf = FALSE)
data.mca.between = bca(data.mca, data.test$centre, scannf = FALSE)
data.mca.ktab = ktab.within(data.mca.within)
data.mca.statis = ktab.within(data.mca.within)

data.famd.cols=union(data.pca.cols,data.mca.cols)
data.famd = dudi.mix(data.test[data.famd.cols], nf = 10, scannf = FALSE)
data.famd.within = wca( data.famd, data.test$centre, scannf = FALSE)
data.famd.between = bca(data.famd, data.test$centre, scannf = FALSE)
data.famd.ktab = ktab.within(data.famd.within)
data.famd.statis = ktab.within(data.famd.within)




