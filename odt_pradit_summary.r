custom_data_prep =  function(input_path, output_path){
  
	library(dplyr)

	file_df = data.frame(org_name = c("Raw_data_ODT.csv", "Raw_data_HLT4.csv"),
						 tab_name = c("ODT_dashboard@Raw_data_ODT_dashboard.csv", "HLT4_dashboard@Raw_data_HLT4_dashboard.csv"))

	for(f in file_df$org_name){
	  
	  print(paste0("Processing: ", f))
	  
	  file_info = file_df[file_df$org_name == f,]
	  
	  data <- read.csv(file.path(input_path, file_info$org_name))
	  
	  if (f == 'Raw_data_ODT.csv') {
	  
		names(data)[grepl('_passfail', names(data))] <- 'passfail'
		names(data)[grepl('combine_product', names(data))] <- 'product_name'
	  
		data_summary <- data %>%
			group_by(fiscal_year_week, location_code, product_name) %>%
			summarise(tested = sum(qty), fail = sum(qty[passfail %in% 'FAIL']), DPPM = round((fail / tested) * 10^6)) %>% 
			ungroup()

		trigger_data = data %>%
		  group_by(location_code, product_name) %>%
		  dplyr::summarise(trigger = ifelse(min(odt_trigger_limit) != 0, min(odt_trigger_limit), NA)) %>%
		  ungroup()

		data_summary = left_join(data_summary, trigger_data)

	  } else {
		data_summary = data
	  
	  }

	  print(paste0("Writing Result to: ", file.path(output_path, file_info$tab_name)))

	  write.csv(data_summary, file.path(output_path, file_info$tab_name), row.names = F)
	  
	} 

}