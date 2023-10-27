custom_data_prep =  function(input_path, output_path){
    
    library(openxlsx)
    library(dplyr)
	options(digits=6)

	spec_file = c('QPM CTQ - SPEC Centralized Document.xlsx', 'QPM IDM CTQ - SPEC Centralized Document.xlsx')
	
	spec_keeper <- data.frame()
	nospec_keeper <- data.frame()
	
    for(f in spec_file){
	
		# wb  <- xlsx::loadWorkbook(file.path(input_path, f))

		# sheet_list <- names(getSheets(wb))
							
		# sheet_list <- sort(sheet_list)

		# rm(wb)

		sheet_list <- openxlsx::getSheetNames(file = file.path(input_path, f))

		sheet_list <- sort(sheet_list[!(sheet_list %in% c("Summary", "Supplier name", "CTQ_list"))])
		
		# rm_sheet <- c("Summary", "Supplier name", "CTQ_list")

		# sheet_list <- sheet_list[!(sheet_list %in% rm_sheet)]

		# spec_keeper <- data.frame()

		if('QPM CTQ - SPEC Centralized Document.xlsx' == f){
			# sheet_list = c('DESICCANT', 'HGA', 'HSA', 'PVA')
			sheet_list = c('HGA', 'HSA')
		} else {
			sheet_list = c('BAGS')
		}

		for(sh in sheet_list){
		  
		  tryCatch({
		  
			  print(paste("Reading sheet:", sh))
			  
			  # com_spec = xlsx::read.xlsx(file = file.path(input_path, f), sheetName = sh, stringsAsFactors=FALSE)
			  
			  com_spec = openxlsx::readWorkbook(xlsxFile = file.path(input_path, f), sheet = sh)
			  
			  com_spec = com_spec[-1,]
			  
			  com_spec = com_spec[complete.cases(com_spec$PART_NUM),names(com_spec) != 'UPDATE_DATE']
			  
			  com_spec$PART_NUM = as.character(com_spec$PART_NUM)
			  com_spec$QPM.trigger.level = as.character(com_spec$QPM.trigger.level)
			  
			  if("CL_type" %in% names(com_spec)){
				com_spec$CL_type = as.character(com_spec$CL_type)
			  }
			  
			  com_spec$Target = as.double(com_spec$Target)
			  com_spec$USL = as.double(com_spec$USL)
			  com_spec$LSL = as.double(com_spec$LSL)
			  
			  com_spec <- com_spec[,colSums(is.na(com_spec)) < nrow(com_spec)]
			  
			  spec_keeper <- bind_rows(spec_keeper, com_spec)
			  
			  rm(com_spec)
			  gc()
		  
		  }, error = function(e) {
			cat("ERROR :", conditionMessage(e), "\n")
		  })
		  
		}
	
	}
	
	spec_keeper = spec_keeper[!is.na(spec_keeper$PART_NUM),!grepl("NA\\.", names(spec_keeper))]
	
	# spec_keeper = spec_keeper[spec_keeper$COMMODITY %in% c('BAGS', 'HGA', 'HSA'),]
	
	spec_keeper = spec_keeper[,!(names(spec_keeper) %in% c("Is.spec.limit.in.dashboard...Yes.No.","Is.trigger.set.in.dashboard...Yes.No.","Is.auto.trigger.set.up...Yes.No.","X.", "Is.dashboard.setup...Yes.No.",
															"Is.spec.limit.in.dashboard?.(Yes/No)","Is.trigger.set.in.dashboard?.(Yes/No)","Is.auto.trigger.set.up?.(Yes/No)","Sample_size","Is.dashboard.setup?.(Yes/No)",""))]
															
	spec_keeper_col = names(spec_keeper)[names(spec_keeper) %in% c("UCL_XChart", "CL_XChart", "LCL_XChart", "UCL_SChart", "CL_SChart", "LCL_SChart")]
	
	spec_keeper = spec_keeper %>% mutate_at(spec_keeper_col, as.numeric, na.rm = TRUE)
	
	sip_spec = read.csv(file.path(input_path, "QPM_SIP_SPEC.csv"), stringsAsFactors = F)

	qpm_sup_dic <- read.csv(file.path(input_path, "QPM_SUP_DIC.csv"), stringsAsFactors = F)[,c("SUPPLIER_NAME","SUPPLIER_CODE")]

	names(qpm_sup_dic)[1] <- "EDW_SUPPLIER_NAME"

	sip_spec <- left_join(sip_spec, qpm_sup_dic, by = "SUPPLIER_CODE")

	sip_spec <- sip_spec %>% dplyr::mutate(SUPPLIER_NAME = case_when(.$SUPPLIER_NAME != .$EDW_SUPPLIER_NAME & complete.cases(.$SUPPLIER_CODE) ~ .$EDW_SUPPLIER_NAME,
																	 TRUE ~ .$SUPPLIER_NAME))

	sip_spec <- sip_spec[,-which(names(sip_spec) %in% "EDW_SUPPLIER_NAME")]

	# sip_spec <- anti_join(sip_spec, spec_keeper, by = c("COMMODITY", "PART_NUM", "SUPPLIER_NAME", "QPM_PARAMETER"))
	
	spec_keeper$PART_NUM <- as.character(spec_keeper$PART_NUM)
	sip_spec$PART_NUM <- as.character(sip_spec$PART_NUM)
	
	spec_keeper <- anti_join(spec_keeper, sip_spec, by = c("COMMODITY", "PART_NUM", "SUPPLIER_NAME", "QPM_PARAMETER"))
	
	# nospec_keeper  <- bind_rows(spec_keeper, nospec_keeper)

	spec_keeper <- bind_rows(sip_spec, spec_keeper)

	spec_keeper$Key_CTQ <- stringr::str_remove_all(spec_keeper$Key_CTQ, " ")

	spec_keeper <- spec_keeper[complete.cases(spec_keeper$Key_CTQ),]
	
    saveRDS(unique(spec_keeper), file = file.path(output_path,'QPM_SPEC.rda'))
	
	key_ctq_list = unique(spec_keeper[spec_keeper$Key_CTQ %in% c("YES", "Yes", "Y"),c("COMMODITY", "Key_CTQ", "QPM_PARAMETER")])
	
	write.csv(unique(key_ctq_list), file = file.path(output_path,'QPM_CTQ_LIST.csv'), row.names = FALSE)
	
	trigger_spec = xlsx::read.xlsx(file = file.path(input_path,'QPM Trigger Criteria.xlsx'), sheetName = "Trigger Criteria", stringsAsFactors=FALSE)

	trigger_spec = melt(trigger_spec, id.vars = 'Trigger_Level')

	trigger_spec = trigger_spec[complete.cases(trigger_spec$value),]

	trigger_spec = trigger_spec %>% tidyr::separate(value, into = c("Equation", "VALUE"), sep = " ")

	names(trigger_spec)[1:2] = c("TRIGGER_LEVEL", "Parameter")

	trigger_spec = data.frame(trigger_spec, stringsAsFactors = F)
	
	saveRDS(unique(trigger_spec), file = file.path(output_path,'QPM_TRIGGER.rda'))
	
	# saveRDS(unique(nospec_keeper), file = file.path(output_path,'No_spec.rda'))
	
}