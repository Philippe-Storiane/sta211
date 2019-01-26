source("Common.R")

library("homals")

extend.discretize.cols=setdiff(quanti_all, c("egfr","lvef"))
extend.discretize.cols=setdiff(quanti_all, c("lvef"))
# imputation toutes variables
expl.cols=union(setdiff(quali_all,c("lvefbin")),setdiff(quanti_all,c("lvef")))
# imputation toutes variables hors pays
expl.cols=union(setdiff(quali_all,c("lvefbin","country")),setdiff(quanti_all,c("lvef")))
# imutation toutes variables hors centre et pays
expl.cols=union(setdiff(quali_all,c("lvefbin","country","centre")),setdiff(quanti_all,c("lvef")))
# imputation toutes variables centre et pays combiné
expl.cols=union(setdiff(quali_all,c("lvefbin","centre","country")),setdiff(quanti_all,c("lvef")))
expl.cols=union(expl.cols,c("centre_country"))
# imutation toutes variables hors centre
expl.cols=union(setdiff(quali_all,c("lvefbin","centre")),setdiff(quanti_all,c("lvef")))

data.train=data.prepare(data_train,expl.cols)
data.train.extend = extend.discretize.quantile(extend.discretize.cols, data.train, 10)
data.test=data.prepare(data_test,expl.cols)
data.test.extend = extend.discretize.quantile(extend.discretize.cols, data.test, 10)
# data.extend=extend.discretize.mdlp(extend.discretize.cols, data.test)
# homals.cols = union(paste0(extend.discretize.cols,"_bin"), setdiff(quali_all, c("centre","country","lvefbin")))
# toutes variables
homals.cols = union(paste0(extend.discretize.cols,"_bin"), setdiff(quali_all, c("lvefbin")))
homals.levels=c("numerical","numerical","numerical","numerical","numerical","numerical","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal")
# toutes variables hors pays
homals.cols = union(paste0(extend.discretize.cols,"_bin"), setdiff(quali_all, c("lvefbin","country")))
homals.levels=c("numerical","numerical","numerical","numerical","numerical","numerical","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal")
# toutes variables hors centre et pays
homals.cols = union(paste0(extend.discretize.cols,"_bin"), setdiff(quali_all, c("lvefbin","centre","country")))
homals.levels=c("numerical","numerical","numerical","numerical","numerical","numerical","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal")
# toutes variables centre et pays combiné
homals.cols = union(paste0(extend.discretize.cols,"_bin"), setdiff(quali_all, c("lvefbin","centre","country")))
homals.cols=union(homals.cols,c("centre_country"))
homals.levels=c("numerical","numerical","numerical","numerical","numerical","numerical","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal")
# toutes variables hors centre
homals.cols = union(paste0(extend.discretize.cols,"_bin"), setdiff(quali_all, c("lvefbin","centre")))
homals.levels=c("numerical","numerical","numerical","numerical","numerical","numerical","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal")


homals.res=homals(data.train.extend[homals.cols],rank=5, level=homals.levels)

extend.score = function(homals.res, datafr) {
    extend.data = datafr
    for( col in colnames(homals.res$scoremat)) {
        print( col )
        col.name = paste0(col,"_score")
        col.index = as.integer(datafr[,col])
        extend.data[,col.name]= homals.res$low[[col]][ col.index,1]
    }
    return(extend.data)
}
data.train.extend=extend.score(homals.res,data.train.extend)
data.test.extend = extend.score(homals.res, data.test.extend)
test.name="ImputeFAMD-Var-CentreCountry-bin-10"
test.timestamp = format(Sys.time(), format="%Y%m%d%H%M")
dump_table(data.train.extend,paste0("data.train.", test.timestamp, test.name,".csv"))
dump_table(data.test.extend,paste0("data.test.", test.timestamp, test.name,".csv"))