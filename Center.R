library("dplyr")
library("FactoMineR")

source("Common.R")




centre.freq = function( var, datafr=data_cleaned ) {
  var=as.character(var)
  mutate_expr = paste0(var,"_freq = n / sum(n)")
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

centre_data= data_cleaned %>%
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
  centre_data = centre_data %>% inner_join(centre_freq)
}

for (var in union(quali_miss,quanti_miss)) {
  var = sprintf("%s_miss", as.character(var))
  centre_freq = centre.freq(var, data_extended)
  centre_data = centre_data %>% inner_join(centre_freq)
}
centre_pca=PCA(centre_data,quali.sup=c(1),graph=FALSE)
fviz_screeplot(centre_pca, addlabels = TRUE, ylim = c(0, 50), title="Toutes variables explicatives hors pays et centre")
fviz_contrib(centre_pca,choice=c("var"),axe=1)
# Complemental variables removed (only describing variable with cos2 > 0.5)
fviz_pca_ind(centre_pca, select.var=list("cos2"=0.5),  col.var="cos2",axes=c(1,2)) + theme_minimal() + scale_color_gradient2(low="white", mid="blue", high="red",midpoint=0.1)
#
