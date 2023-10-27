custom_data_prep = function(input_path, output_path, file_name){

  library(reshape2)
  library(dplyr)
  
  odt_param_file = list.files(file.path(input_path, 'PROC'))
  
  print(odt_param_file)
  
  odt_param_file = odt_param_file[grepl('CIMARRON', odt_param_file) & !grepl('CIMARRONBP', odt_param_file)]
  
  for (f in odt_param_file) {
    
    tryCatch({
      
      print(paste("Processing file:",f))
      
      data_set = readRDS(file.path(input_path, 'PROC', f))
	  
	  data_set = data_set[data_set$FISCAL_YEAR_WEEK %in% '2022-01',]
	  
	  saveRDS(data_set, file.path(output_path, f))
	  
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
    
    gc()
    
  }
  
  output_path_files = paste(output_path, odt_param_file, sep="/")
  
  zip(zipfile = file.path(input_path, "CIMARRON_2201_Request_data.zip"), files = output_path_files, extras = '-j')
  
}