custom_data_prep =  function(input_path, output_path){
  
  library(dplyr)
  
  MSL_webfact = readRDS(file.path(input_path, "msl_webfact.rda"))
  LIMS_data = readRDS(file.path(input_path, "E1076138.rda"))
  
  MSL_webfact = left_join(MSL_webfact, LIMS_data)
  
  write.csv(MSL_webfact, file.path(output_path, "MSL_LIMS_Webfact_link.csv"), row.names = F)
  
}