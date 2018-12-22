quanti_expl=c("bmi","age","egfr","sbp","dbp", "hr") 
quanti_all=c("bmi","age","egfr","sbp","dbp", "hr", "lvef")
quali_expl=c("centre","country","gender","copd","hypertension","previoushf","afib","cad")
quali_all=c("centre","country","gender","copd","hypertension","previoushf","afib","cad", "lvefbin")
quanti_miss=c("bmi_miss","age_miss","egfr_miss","sbp_miss","dbp_miss", "hr_miss")
quali_miss=c("copd_miss","hypertension_miss","previoushf_miss","afib_miss","cad_miss")
cols=union(setdiff(quali_all, c("lvefbin","country")),setdiff(quanti_all,c("lvef")))


load("data_train.rda")

 