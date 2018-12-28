quanti_expl=c("bmi","age","egfr","sbp","dbp", "hr") 
quanti_all=c("bmi","age","egfr","sbp","dbp", "hr", "lvef")
quali_expl=c("centre","country","gender","copd","hypertension","previoushf","afib","cad")
quali_all=c("centre","country","gender","copd","hypertension","previoushf","afib","cad", "lvefbin")
quali_miss=setdiff(quali_all,c("centre","country","gender","lvefbin"))
quanti_miss=setdiff(quanti_all,c("lvef"))
cols=union(setdiff(quali_all, c("lvefbin","country")),setdiff(quanti_all,c("lvef")))


load("data_train.rda")

 