source("Common.R")

library("homals")

extend.discretize.cols=setdiff(quanti_all, c("egfr","lvef"))
data.test=data.prepare()
# data.extend=extend.discretize.mdlp(extend.discretize.cols, data.test)
data.extend = extend.discretize.quantile(extend.discretize.cols, data.test, 10)
homals.cols = union(paste0(extend.discretize.cols,"_bin"), setdiff(quali_all, c("centre","country","lvefbin")))
homals.levels=c("numerical","numerical","numerical","numerical","numerical","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal")
homals.res=homals(data.extend[homals.cols],rank=2, level=homals.levels)

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
data.extend=extend.score(homals.res,data.test)