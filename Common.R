
source("Variables.R")

famd.clean= function(datafr) {
  tt=levels(datafr$completeObs$centre)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$centre) <- tt

  #tt=levels(famd.data$completeObs$country)
  #tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  #eval.parent(substitute(levels(famd.data$completeObs$country) <- tt))
  
  tt=levels(datafr$completeObs$gender)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$gender)<- tt

  tt=levels(datafr$completeObs$hypertension)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$hypertension)<- tt
  
  tt=levels(datafr$completeObs$previoushf)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$previoushf)<- tt
  
  tt=levels(datafr$completeObs$afib)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$afib)<- tt
  
  tt=levels(datafr$completeObs$cad)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$cad)<- tt
  
  tt=levels(datafr$completeObs$copd)
  tt=sub('[a-z]*_([0-9]*)','\\1',tt)
  levels(datafr$completeObs$copd)<- tt
  
  return(datafr)
}


dump_table = function( data, file_name) {
	print(paste("Dumping table to file ", file_name))
	write.table(data, file=file_name, sep=";", row.names = FALSE, dec = ",", quote=FALSE)
}