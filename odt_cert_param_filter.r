
custom_data_prep = function(input_path, output_path, file_name){
  
  #####
  
  param_by_pro_processer <- function(data_set, file_path, file_name){
    
    pro_list = unique(data_set$PRODUCT_NAME)
    
    for(pro in pro_list) {
      
	  tryCatch({
	  
		  save_path  = file.path(file_path, paste0(pro, "_", file_name))
		  
		  print(save_path)
		  
		  sub_set_date = data_set[data_set$PRODUCT_NAME %in% pro,]
		  
		  saveRDS(sub_set_date, file = save_path)
		  
		  rm(sub_set_date)
		  gc()
	  
	  }, error = function(e) {
		cat("ERROR :", conditionMessage(e), "\n")
	  })
	  
    }
	
  }
  
  #####
  
  library(reshape2)
  library(dplyr)
  
  odt_param_file = list.files(input_path)
  
  file_remover = c("filter.rda")
  
  odt_param_file = odt_param_file[!grepl(".gz", odt_param_file) & !(odt_param_file %in% file_remover)]
  
  odt_param_file = sort(odt_param_file)
  
  param_dic = data.frame()
  
  for (f in odt_param_file) {
    
    tryCatch({
      
      print(paste("Processing file:",f))
      
      data_set = readRDS(file.path(input_path, f))
      
      # if (f == "P321_MRDR_BIAS_CAL.rda") {
        
        # add_data_set = readRDS(file.path(input_path, "P321_BIAS_CAL2.rda"))
        
        # data_set = bind_rows(data_set, add_data_set)
        
        # rm(add_data_set)
        
      # }
      
      names(data_set) = toupper(names(data_set))
      
      names(data_set)[names(data_set) %in% "OPERATION"] <- "CERT_OPERATION"
      names(data_set)[names(data_set) %in% "EVENT_DATE"] <- "CERT_EVENT_DATE"
      
      names(data_set)[names(data_set) %in% "ODT_PRODUCT_NAME"] <- "PRODUCT_NAME"
      names(data_set)[names(data_set) %in% "ODT_OPERATION"] <- "ODT_GROUP"
      names(data_set)[names(data_set) %in% "ODT_EVENT_DATE"] <- "EVENT_DATE"
      
      if (f == "P107_VERIFIED_FLAWS.rda") {
        
        print("Running VRFD_FLAWS data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("VRFD_FLAWS")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P126_SRVO_FLAW_HD.rda") {
        
        print("Running RAW_SRVO_FLAW_CNT data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$RAW_SRVO_FLAW_CNT),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("RAW_SRVO_FLAW_CNT")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P134_TA_SUM_HD2.rda") {
        
        print("Running BIG_TA_CNT + TA_CNT data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$BIG_TA_CNT),])
        param_temp = unique(data_set[complete.cases(data_set$TA_CNT),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("BIG_TA_CNT", "TA_CNT")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P297_HD_INSTBY_SUM_FULL_SIGMA.rda") {
        
        print("Running FULL_SIGMA data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("FULL_SIGMA")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P297_HD_INSTBY_SUM_Z_SCORE.rda") {
        
        print("Running Z_SCORE data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("Z_SCORE")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P315_INSTABILITY_METRIC.rda") {
        
        print("Running HD_INSTABILITY_METRIC data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("HD_INSTABILITY_METRIC")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P321_BIAS_CAL2.rda") {
        
        print("Running MRE_RESISTANCE data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c('MRE_RESISTANCE')) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P321_MRDR_BIAS_CAL.rda") {
        
        print("Running MRE_RESISTANCE data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c('MRE_RESISTANCE')) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P345_RESULT_SUMMARY.rda") {
        
        print("Running MINIMUM + MAXIMUM data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("MINIMUM", "MAXIMUM")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P_FORMAT_ZONE_ERROR_RATE.rda") {
        
        print("Running RAW_ERROR_RATE data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("RAW_ERROR_RATE")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P_VBAR_FORMAT_SUMMARY.rda") {
        
        print("Running ADC data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$ADC),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("ADC")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P056_U_ACTUATOR_GAIN_CAL.rda") {
        
        print("Running UACT_GAIN data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$UACT_GAIN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("UACT_GAIN")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P232_UACT_CAL_VALUES.rda") {
        
        print("Running K_DAC data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$K_DAC),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("K_DAC")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P461_STAT_DATA.rda") {
        
        print("Running BER_MEAN data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("BER_MEAN")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P180_RSNC_SUMMARY.rda") {
        
        print("Running MAX_FFT_RRO_AMP + MAX_FFT_NRRO_AMP data prep")
        
        # param_temp = unique(data_set[complete.cases(data_set$MAX_FFT_RRO_AMP),])
        # param_temp = unique(data_set[complete.cases(data_set$MAX_FFT_NRRO_AMP),])
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("MAX_FFT_RRO_AMP", "MAX_FFT_NRRO_AMP")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P180_RSNC_SUMMARY2.rda") {
        
        print("Running MAX_FFT_RRO_AMP + MAX_FFT_NRRO_AMP data prep")
        
        # param_temp = unique(data_set[complete.cases(data_set$MAX_FFT_RRO_AMP),])
        # param_temp = unique(data_set[complete.cases(data_set$MAX_FFT_NRRO_AMP),])
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("MAX_FFT_RRO_AMP", "MAX_FFT_NRRO_AMP")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P250_ERROR_RATE_BY_ZONE.rda") {
        
        print("Running RAW_ERROR_RATE data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        param_by_pro_processer(param_temp, output_path, f)
        
        for (p in c("RAW_ERROR_RATE")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "ODT_GROUP", "LOCATION_CODE", "DRIVE_SBR_NUM",
                                                                          "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY", "RUN_TYPE_PRIME_REWORK", "TEMP_PROFILE")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      }
      
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
    
	rm(data_set)
	
    gc()
    
  }
  
  ######### Filter data processing
  
  filter_set = readRDS(file.path(input_path, "filter.rda"))
  
  names(filter_set) = toupper(names(filter_set))
  names(filter_set)[names(filter_set) %in% "ODT_PRODUCT_NAME"] <- "PRODUCT_NAME"
  names(filter_set)[names(filter_set) %in% "ODT_OPERATION"] <- "ODT_GROUP"
  
  filter_set = unique(filter_set[, c(
    "PRODUCT_NAME",
    "ODT_GROUP",
    "LOCATION_CODE",
    "DRIVE_SBR_NUM",
    "PRODUCT_INTERFACE",
    "PRODUCT_FORMAT_CAPACITY",
    "RUN_TYPE_PRIME_REWORK",
    "TEMP_PROFILE"
  )])
  
  filter_set = inner_join(filter_set, param_dic)
  
  saveRDS(param_dic, file.path(output_path, "param_dic.rda"))
  saveRDS(filter_set, file.path(output_path, "filter.rda"))
  
  gc()
  
  ######### Failure data processing
  
  # library(dplyr)
  # library(tidyr)
  
  # failure_set <- readRDS(file.path(input_path, "fail_detail.rda"))
  
  # names(failure_set) <- toupper(names(failure_set))
  
  # names(failure_set)[names(failure_set) %in% "AR_FAILING_HEAD_NUM"] <- "HD_PHYS_PSN"
  
  # failure_set <- failure_set[,c("FISCAL_YEAR_WEEK", "DRIVE_SERIAL_NUM", "HD_PHYS_PSN", "FAILURE_MODE", "FAIL_CODE", "TIME_TO_FAILURE")]
  
  # if (any(failure_set$HD_PHYS_PSN %in% c('<NA>', 'n/a', 'NA'))) {
  
	# failure_set[failure_set$HD_PHYS_PSN %in% c('<NA>', 'n/a', 'NA'),]$HD_PHYS_PSN <- NA
  
  # }
  
  # failure_set$HD_PHYS_PSN <- stringr::str_replace_all(failure_set$HD_PHYS_PSN, " ", "")
  
  # failure_set <- failure_set %>% separate_rows(HD_PHYS_PSN)
  
  # saveRDS(failure_set, file.path(output_path, "fail_detail.rda"))
  
  # gc()
                         
}