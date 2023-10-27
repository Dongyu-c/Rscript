custom_data_prep =  function(input_path, output_path){

  system(paste0('python3 ',file.path(UploadPath, GID, "Rscript/shell_caller.py")))
  
}