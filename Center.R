library("dplyr")
library("reshape2")
library("ggplot2")
library("FactoMineR")
library("factoextra")

source("Common.R")





centre.freq = function( var, datafr=data_cleaned ) {
  var=as.character(var)
  mutate_expr = paste0(var,"_freq = 1 - (n / sum(n))")
  filter_expr = paste0( "as.numeric(",var, ") == 2")
  var_freq=paste0(var,"_freq")
  centre_freq = datafr %>%
    group_by_("centre", var) %>%
    summarize( n = n()) %>%
    mutate_( mutate_expr ) %>%
    filter_( filter_expr)
  colnames(centre_freq)[[4]] = var_freq
  centre_freq = centre_freq %>%
    select_( "centre", var_freq)
  return(centre_freq)
}


data_cleaned=data.clean()
data_extended = extend.miss(quali_miss, data_cleaned)
data_extended = extend.miss(quanti_miss,data_extended)
data_extended=extend.miss.combine("all",union(quali_miss, quanti_miss), data_extended)
data_extended=extend.miss.combine("not_bmi",setdiff(union(quali_miss, quanti_miss),c("bmi")), data_extended)



centre.data= data_cleaned %>%
  group_by(centre) %>%
  summarize(
    bmi_mean=mean(bmi,na.rm=T),
    bmi_sd=sd(bmi,na.rm=T),
    age_mean=mean(age,na.rm=T),
    age_sd=sd(age,na.rm=T),
    egfr_mean=mean(egfr,na.rm=T),
    egfr_sd=sd(egfr,na.rm=T),
    sbp_mean=mean(sbp,na.rm=T),
    sbp_sd=sd(sbp,na.rm=T),
    dbp_mean=mean(dbp,na.rm=T),
    dbp_sd=sd(dbp,na.rm=T),
    hr_mean=mean(hr,na.rm=T),
    hr_sd=sd(hr,na.rm=T),
    lvef_mean=mean(lvef,na.rm=T),
    lvef_sd=sd(lvef,na.rm=T)
  )

for(quali in setdiff(quali_all,c("centre","country"))) {
  quali = as.character( quali )
  centre_freq = centre.freq(quali, data_extended)
  centre.data = centre.data %>% inner_join(centre_freq)
}

for (var in union(quali_miss,quanti_miss)) {
  var = sprintf("%s_miss", as.character(var))
  centre_freq = centre.freq(var, data_extended)
  centre.data = centre.data %>% inner_join(centre_freq)
}
centre_freq= centre.freq("all_miss",data_extended)
centre.data = centre.data %>% inner_join(centre_freq)
centre_freq= centre.freq("not_bmi_miss",data_extended)
centre.data = centre.data %>% inner_join(centre_freq)


#
# Vizualizing centre_data
#
centre.melted.mean=melt(centre.data[c("centre","bmi_mean","age_mean","sbp_mean","dbp_mean","egfr_mean")],id="centre")
centre.melted.sd=melt(centre.data[c("centre","bmi_sd","age_sd","sbp_sd","dbp_sd","egfr_sd")],id="centre")
centre.melted.quali_freq=melt(centre.data[c("centre","gender_freq","copd_freq","hypertension_freq","previoushf_freq","afib_freq","cad_freq")],id="centre")
centre.melted.quali_miss_freq=melt(centre.data[c("centre","copd_miss_freq","hypertension_miss_freq","previoushf_miss_freq","afib_miss_freq","cad_miss_freq")],id="centre")
centre.melted.quanti_miss_freq=melt(centre.data[c("centre","bmi_miss_freq","age_miss_freq","sbp_miss_freq","dbp_miss_freq","egfr_miss_freq")],id="centre")
ggplot(data=centre.melted.mean, aes(x=as.double(centre),y=value,color=variable)) + geom_line()
ggplot(data=centre.melted.sd, aes(x=as.double(centre),y=value,color=variable)) + geom_line()
ggplot(data=centre.melted.quali_freq, aes(x=as.double(centre),y=value,color=variable)) + geom_line()
ggplot(data=centre.melted.quali_miss_freq, aes(x=as.double(centre),y=value,color=variable)) + geom_line()
ggplot(data=centre.melted.quanti_miss_freq, aes(x=as.double(centre),y=value,color=variable)) + geom_line()
ggplot(data=centre.melted.quanti_miss_freq, aes(x=as.double(centre),y=value,,color=variable)) + geom_line() + geom_smooth(method="loess")



centre.pca=PCA(centre.data,quali.sup=c(1,3,5,7,9,11,13,15,25,26,27,28,29,30,31,32,33,34,35),graph=FALSE)
centre.pca=PCA(centre.data,quali.sup=c(1,3,5,7,9,11,13,15,25,26,27,28,29,30,31,32,33,34),graph=FALSE)

centre.pca=PCA(centre.data,quali.sup=c(1,34,35),graph=FALSE)
centre.pca=PCA(centre.data,quali.sup=c(1), quanti.sup=c(34),graph=FALSE)

fviz_screeplot(centre.pca, addlabels = TRUE, ylim = c(0, 50), title="Toutes variables explicatives hors pays et centre")
fviz_contrib(centre.pca,choice=c("var"),axe=1)
# Complemental variables removed (only describing variable with cos2 > 0.5)
fviz_pca_ind(centre.pca, axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)
fviz_pca_var(centre.pca, select.var=list("cos2"=0.5), axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)
#
# cluster analysis
#
centre.hcpc=HCPC(centre.pca,graph=FALSE)
centre.kmeans=kmeans(get_pca_ind(centre.pca)$coord, centers=5)
fviz_cluster(centre.hcpc)
fviz_cluster(object=centre.kmeans,data=get_pca_ind(centre.pca)$coord)