custom_data_prep = function(input_path, output_path) {
  
  ifelse(!dir.exists(output_path), dir.create(file.path(transfer_path)), FALSE)
  
  input_file = file.path(input_path, "QITAll.rda")
  output_file = file.path(output_path, paste0(substring(Sys.time(), 1, 7), ".rda"))
  
  print(paste("input_file:", input_file))
  print(paste("output_file:", output_file))
  
  file.copy(from = input_file, to = output_file, overwrite = T)
  
}