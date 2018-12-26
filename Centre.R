library("dplyr")

source("Common.R")
data_cleaned=data.clean()
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

centre_freq= data_cleaned %>%
  group_by(centre,copd) %>%
  summarize(n = n()) %>%
  mutate(copd_freq=n / sum(n)) %>%
  filter( copd==1) %>%
  select(centre, copd_freq)

centre_data %>% inner_join(centre_data,centre_fre)
  