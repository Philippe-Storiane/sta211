library("dplyr")

source("Common.R")


miss.process = function( vars, data_extended) {
	for(var in vars ) {
		var_miss=sprintf("%s_miss", var)
		data_extended[,c(var_miss)] = ifelse(is.na(data_extended[,c(var)]),"miss","not miss")
		data_extended[,c(var_miss)]=factor(c("miss","not miss"))
	}
	return(data_extended)
}

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
}




data_cleaned=data.clean()
data_extended = miss.process(setdiff(quali_all,c("centre","country")), data_cleaned)
data_extended = miss.process(quanti_all,data_extended)

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
    centre_freq = centre.freq(quali)
    print(centre_freq)
    centre_data = centre_data %>% inner_join(centre_freq)
}

for (var in setdiff(union(quali_all,quanti_all), c("centre", "country","gender"))) {
    var = sprintf("%s_miss", as.character(var))
    centre_freq = centre.freq(var, data_extended)
    print(centre_freq)
    centre_data = centre_data %>% inner_join(centre_freq)
}


