custom_data_prep =  function(input_path, output_path){
  
	data_temp <- readRDS(file.path(input_path, "CCP_DASHBOARD_RAW.rda"))
	data_dic <- readRDS(file.path(input_path, "CCP_DASHBOARD_DIC.rda"))
	# names(data_dic) <- c("PRODUCT_PART_NUM", "COMPONENT_TYPE", "PRODUCT_INTERNAL_NAME")
	names(data_dic) <- c("PRODUCT_INTERNAL_NAME", "PRODUCT_PART_NUM", "COMPONENT_TYPE")
	
	dm_ccp <- readRDS(file.path(input_path, "DMCCP_RAW.rda"))

	dm_ccp <- dm_ccp[,c('PROGRAM', 'PART_NUM', 'COMMODITY_NAME')]
	
	dm_ccp_sub <- dm_ccp[grepl("_", dm_ccp$PROGRAM),]

	dm_ccp_sub$PROGRAM <- sub("_.*", "", dm_ccp_sub$PROGRAM)
	
	dm_ccp <- unique(rbind(dm_ccp, dm_ccp_sub))
	
	names(dm_ccp) <- c("PRODUCT_INTERNAL_NAME", "PRODUCT_PART_NUM", "COMPONENT_TYPE")

	data_dic <- unique(rbind(data_dic, dm_ccp))
	
	data_dic <- data_dic %>% mutate(COMPONENT_TYPE = ifelse(grepl(pattern = "CCP", COMPONENT_TYPE), COMPONENT_TYPE, paste0(COMPONENT_TYPE, "CCP")))
	
	data_temp <- left_join(data_temp, data_dic)
	
	# print(unique(data_temp$PRODUCT_INTERNAL_NAME))

	select_col = c("PRODUCT_INTERNAL_NAME", "COMPONENT_TYPE", "PRODUCT_PART_NUM", "PARTICLE_NAME", "SUPPLIER_NAME", "GROUP_DATETIME", "PARTICLE_VAL")

	# library(reshape2)
	# library(dplyr)
	# library(lazyeval)

	# groupBy <- c("PRODUCT_INTERNAL_NAME","COMPONENT_TYPE", "PRODUCT_PART_NUM", "PARTICLE_NAME", "SUPPLIER_NAME")
	# sumVal <- "PARTICLE_VAL"

	# data_temp = data_temp %>% 
	  # group_by_(.dots=groupBy) %>% 
	  # summarise_("N Rows" = ~n(),
				 # "Mean(PARTICLE_VALUE)" = interp(~mean(x), x=as.name(sumVal)), 
				 # "Std Dev(PARTICLE_VALUE)" = interp(~sd(x), x=as.name(sumVal)),
				 # "CV(PARTICLE_VALUE)" = interp(~((sd(x) * 100)/ mean(x)), x=as.name(sumVal)),
				 # "MeanPluskSigma" = interp(~((sd(x) * 4.5) + mean(x)), x=as.name(sumVal))
	  # )

	# data_temp = left_join(data_temp, data_temp %>% 
		# group_by(PRODUCT_INTERNAL_NAME, PRODUCT_PART_NUM, PARTICLE_NAME) %>% 
		# summarise_("ProposedSpec" = interp(~(max(x)), x=as.name("MeanPluskSigma"))))
		
	# data_temp["Cpk"] = (data_temp$ProposedSpec - data_temp$`Mean(PARTICLE_VALUE)`) / 3*data_temp$`Std Dev(PARTICLE_VALUE)`

	# data_temp["StdDevProblem"] = ifelse(data_temp$`Std Dev(PARTICLE_VALUE)` == 0, "No Data", " ")

	# data_temp["PartNum_Analyte"] = p	aste0(data_temp$PRODUCT_PART_NUM,"-",data_temp$PARTICLE_NAME)

	# data_temp[is.na(data_temp$StdDevProblem),]$StdDevProblem <- "No Variability"

	# data_temp["SampleProblem"] = ifelse(data_temp$`N Rows` <= 5, "le5", ifelse(data_temp$`N Rows` <= 10, "le10", ifelse(data_temp$`N Rows` <= 15, "le15"," ")))

	# data_temp <- data_temp[,c("PRODUCT_INTERNAL_NAME", "COMPONENT_TYPE", "PRODUCT_PART_NUM", "PARTICLE_NAME", "PartNum_Analyte", "SUPPLIER_NAME", "N Rows", "Mean(PARTICLE_VALUE)", "Std Dev(PARTICLE_VALUE)", "CV(PARTICLE_VALUE)", "SampleProblem", "StdDevProblem", "MeanPluskSigma", "ProposedSpec", "Cpk")]

	names(data_temp)[4] <- "Analyte"

	data_temp = data_temp[,select_col]

	data_temp = unique(data_temp)

	# data_temp$GROUP_DATETIME <- as.Date(data_temp$GROUP_DATETIME)
	
	# data_temp = data_temp[,colSums(is.na(data_temp))<nrow(data_temp)]
	
	saveRDS(data_temp, file = file.path(output_path, "Raw_data.rda"))
	
}