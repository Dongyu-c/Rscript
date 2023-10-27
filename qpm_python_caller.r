custom_data_prep =  function(input_path, output_path){

  print(ResultsPath)
  
  INP_ResultsPath =  file.path(ResultsPath, 'E1072320', 'RDA_MOD')
  SPEC_ResultsPath =  file.path(ResultsPath, 'E1082262', 'QPM_transfer')
  INP_Sample_ResultsPath = file.path(ResultsPath, 'E1065238', 'RDA_DATA')
  
  print(getwd())
  
  Current_ResultsPath = file.path(ResultsPath, 'E1118762')

  file.copy(file.path(SPEC_ResultsPath, "QPM_CTQ_LIST.csv"), Current_ResultsPath)
  
  f_list = c('QPM_SPEC.rda', 'INP_QPM_DASHBOARD_ACT.rda', 'INP_QPM_DASHBOARD_CLAMP.rda', 'INP_QPM_DASHBOARD_DSP.rda', 'INP_QPM_DASHBOARD_MOTOR.rda', 'INP_QPM_DASHBOARD_VCMA.rda', 'INP_QPM_DASHBOARD_STMCV.rda', 'INP_QPM_DASHBOARD_SPACER.rda', 'INP_QPM_DASHBOARD_RAMP.rda') 

  for(f in f_list) {
	
	print(paste0("Processing: ", f))
	
	if(f == 'INP_QPM_DASHBOARD_MOTOR.rda') {
		temp = readRDS(file.path(INP_Sample_ResultsPath, f))
	} else if (f == 'QPM_SPEC.rda') {
		temp = readRDS(file.path(SPEC_ResultsPath, f))
	} else {
		temp = readRDS(file.path(INP_ResultsPath, f))
	}
	
	temp = temp %>% ungroup()
	
	write.csv(temp, file = file.path(Current_ResultsPath, stringr::str_replace(f, '.rda', '.csv')), row.names = F)
  
	print("Convert RDS to CSV")
	
	rm(temp)
  
  }

  system(paste0('python3 ',file.path(UploadPath, GID, "Rscript/qpm_config.py")))
  
}