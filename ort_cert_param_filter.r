custom_data_prep = function(input_path, output_path){
  
  library(reshape2)
  library(dplyr)
  
  ort_param_file = list.files(input_path)
  
  print(ort_param_file)
  
  file_remover = c("filter.rda")
  
  ort_param_file = ort_param_file[!grepl(".gz", ort_param_file) & !(ort_param_file %in% file_remover)]
  
  ort_param_file = sort(ort_param_file)
  
  param_dic = data.frame()
  
  for (f in ort_param_file) {
    
    tryCatch({
      
      print(paste("Processing file:",f))
      
      data_set = readRDS(file.path(input_path, f))
	  
	  if (f == "P321_MRDR_BIAS_CAL.rda") {
	  
		add_data_set = readRDS(file.path(input_path, "P321_BIAS_CAL.rda"))
		
		data_set = bind_rows(data_set, add_data_set)
		
		rm(add_data_set)
	  
	  }
	  
      names(data_set) = toupper(names(data_set))
      
      names(data_set)[names(data_set) %in% "OPERATION"] <- "CERT_OPERATION"
      names(data_set)[names(data_set) %in% "EVENT_DATE"] <- "CERT_EVENT_DATE"
	 
      names(data_set)[names(data_set) %in% "ORT_SUB_TYPE"] <- "TEST_TYPE"
	  names(data_set)[names(data_set) %in% "ORT_TEST_UID"] <- "TEST_UID"
	  names(data_set)[names(data_set) %in% "ORT_LOCATION"] <- "LOCATION_CODE"
      names(data_set)[names(data_set) %in% "ORT_PRODUCT_NAME"] <- "PRODUCT_NAME"
	  
      if (f == "P107_VERIFIED_FLAWS.rda") {
        
        print("Running VRFD_FLAWS data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("VRFD_FLAWS")) {
          
		  dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE","PRODUCT_FORMAT_CAPACITY")])
		  
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P126_SRVO_FLAW_HD.rda") {
        
        print("Running RAW_SRVO_FLAW_CNT + SKIP_TRACKS data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$RAW_SRVO_FLAW_CNT),])
        
        param_temp =  param_temp[,unique(names(param_temp))]

		col_list = c("PRODUCT_INTERFACE", "DRIVE_SERIAL_NUM", "CERT_EVENT_DATE", "HD_PHYS_PSN", 
					 "CERT_OPERATION", "TEST_UID", "PRODUCT_NAME", "TEST_TYPE", "LOCATION_CODE", "TEST_WORK_WEEK", 
					 "DRIVE_PART_NUM", "PRODUCT_FORMAT_CAPACITY")
					 
		v_product_raw_srvo_flaw_cnt = param_temp %>% 
		  filter(grepl("V9|V11" ,PRODUCT_NAME)) %>%
		  group_by(PRODUCT_INTERFACE, DRIVE_SERIAL_NUM, CERT_EVENT_DATE, HD_PHYS_PSN, CERT_OPERATION, TEST_UID, 
				   PRODUCT_NAME, TEST_TYPE, LOCATION_CODE, TEST_WORK_WEEK, 
				   DRIVE_PART_NUM, PRODUCT_FORMAT_CAPACITY) %>% 
		  slice(which.max(RAW_SRVO_FLAW_CNT))
		  
		v_product_skip_tracks = param_temp %>% 
		  filter(grepl("V9|V11" ,PRODUCT_NAME)) %>%
		  group_by(PRODUCT_INTERFACE, DRIVE_SERIAL_NUM, CERT_EVENT_DATE, HD_PHYS_PSN, CERT_OPERATION, TEST_UID, 
				   PRODUCT_NAME, TEST_TYPE, LOCATION_CODE, TEST_WORK_WEEK, 
				   DRIVE_PART_NUM, PRODUCT_FORMAT_CAPACITY) %>% 
		  slice(which.max(SKIP_TRACKS))

		v_product = left_join(v_product_skip_tracks[,c(col_list, "SKIP_TRACKS")], v_product_raw_srvo_flaw_cnt[,c(col_list, "RAW_SRVO_FLAW_CNT")])

		rm(v_product_raw_srvo_flaw_cnt)
		rm(v_product_skip_tracks)

		other_product_con = param_temp %>% 
		  filter(!grepl("V9|V11" ,PRODUCT_NAME) & STATE_NAME == 'SPFS') %>% 
		  group_by(DRIVE_SERIAL_NUM, HD_PHYS_PSN, CERT_EVENT_DATE, STATE_NAME, TEST_UID, PRODUCT_NAME) %>% 
		  summarise(SPC_ID_RANK = min(SPC_ID_RANK))

		other_product = left_join(other_product_con, param_temp)

		other_product = other_product[,c(col_list, "RAW_SRVO_FLAW_CNT", "SKIP_TRACKS")]
		
		rm(other_product_con)

		param_temp = bind_rows(other_product, v_product)
        
		rm(v_product)
		rm(other_product)
		
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("RAW_SRVO_FLAW_CNT", "SKIP_TRACKS")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
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
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("BIG_TA_CNT", "TA_CNT")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P297_HD_INSTBY_SUM.rda") {
        
        print("Running FULL_SIGMA, SIGMA an SIGMA_LOSS and data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("FULL_SIGMA", "SIGMA", "SIGMA_LOSS")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
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
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("Z_SCORE")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
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
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("HD_INSTABILITY_METRIC")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
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
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c('MRE_RESISTANCE')) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
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
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("MINIMUM", "MAXIMUM")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
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
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("RAW_ERROR_RATE")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
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
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("ADC")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
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
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("UACT_GAIN")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
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
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("K_DAC")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P461_STAT_DATA_FACT.rda") {
		
        print("Running BER_MEAN data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("BER_MEAN")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
		
      } else if (f == "P107_UNVER_HD_TOTAL.rda") {
		
        print("Running UNVER data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("UNVER")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
		
      }  else if (f == "P_AFH_DH_BURNISH_CHECK.rda") {
		
        print("Running DELTA_BURNISH_CHECK_R + DELTA_BURNISH_CHECK_W data prep")
        
		Write_set = data_set[data_set$ACTIVE_HEATER == 'W', !(names(data_set) %in% 'ACTIVE_HEATER')]
		names(Write_set)[names(Write_set) %in% 'DELTA_BURNISH_CHECK'] <- "DELTA_BURNISH_CHECK_W"

		Read_set = data_set[data_set$ACTIVE_HEATER == 'R', !(names(data_set) %in% 'ACTIVE_HEATER')]
		names(Read_set)[names(Read_set) %in% 'DELTA_BURNISH_CHECK'] <- "DELTA_BURNISH_CHECK_R"

		if (nrow(Read_set) > nrow(Write_set)) {
		  data_set = left_join(Read_set, Write_set)
		} else {
		  data_set = left_join(Write_set, Read_set)
		}

		rm("Write_set", "Read_set")
		
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c('DELTA_BURNISH_CHECK_R', 'DELTA_BURNISH_CHECK_W')) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
		
      } else if (f == "P_TCS_SUMMARY.rda") {
		
        print("Running HMS_CAP_COLD + HMS_CAP_HOT data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c('HMS_CAP_COLD', 'HMS_CAP_HOT')) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
		
      } else if (f == "P_SETTLING_SUMMARY.rda") {
		
        print("Running HARD_ERR_RATE_TIME_0, RAW_ERR_RATE_TIME_0 and HMS_CAP_1 data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
		        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c('HARD_ERR_RATE_TIME_0', 'RAW_ERR_RATE_TIME_0', 'HMS_CAP_1')) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
		
      } else if (f == "P033_PES_HD2.rda") {
		
        print("Running RRO_FNC2 + NRRO_FNC2 data prep")
		
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
		        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c('RRO_FNC2', 'NRRO_FNC2')) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
		
      } else if (f == "P180_RSNC_SUMMARY2_ALL.rda") {
        
        print("Running MAX_FFT_RRO_AMP + MAX_FFT_NRRO_AMP data prep")
        
        # param_temp = unique(data_set[complete.cases(data_set$MAX_FFT_RRO_AMP),])
		# param_temp = unique(data_set[complete.cases(data_set$MAX_FFT_NRRO_AMP),])
		
		param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, "P180_RSNC_SUMMARY2.rda"))
        
        for (p in c("MAX_FFT_RRO_AMP", "MAX_FFT_NRRO_AMP")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P180_RSNC_SUMMARY2_EVANS.rda") {
        
        print("Running MAX_FFT_RRO_AMP + MAX_FFT_NRRO_AMP data prep")
        
        # param_temp = unique(data_set[complete.cases(data_set$MAX_FFT_RRO_AMP),])
		# param_temp = unique(data_set[complete.cases(data_set$MAX_FFT_NRRO_AMP),])
		
		param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, "P180_RSNC_SUMMARY2.rda"))
        
        for (p in c("MAX_FFT_RRO_AMP", "MAX_FFT_NRRO_AMP")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
          dic_temp =
            merge(
              dic_temp,
              data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "P025_LOAD_UNLOAD_PARAMS.rda") {
        
        print("Running LOAD_BEMF_CAL_RESID_ERR_MEAN data prep")
        
        # param_temp = unique(data_set[complete.cases(data_set$MAX_FFT_RRO_AMP),])
		# param_temp = unique(data_set[complete.cases(data_set$MAX_FFT_NRRO_AMP),])
		
		param_temp = unique(data_set[complete.cases(data_set$LOAD_BEMF_CAL_RESID_ERR_MEAN),])
        
        param_temp =  param_temp[,unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, "P025_LOAD_UNLOAD.rda"))
        
        for (p in c("LOAD_BEMF_CAL_RESID_ERR_MEAN")) {
          
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c("PRODUCT_NAME", "TEST_TYPE", "TEST_UID", "TEST_WORK_WEEK", "LOCATION_CODE", "PRODUCT_INTERFACE", "PRODUCT_FORMAT_CAPACITY")])
          
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
    
    gc()
    
  }
  
  filter_set = readRDS(file.path(input_path, "filter.rda"))
  
  names(filter_set) = toupper(names(filter_set))

  names(filter_set)[names(filter_set) %in% "ORT_SUB_TYPE"] <- "TEST_TYPE"
  names(filter_set)[names(filter_set) %in% "ORT_TEST_UID"] <- "TEST_UID"
  names(filter_set)[names(filter_set) %in% "ORT_LOCATION"] <- "LOCATION_CODE"
  names(filter_set)[names(filter_set) %in% "ORT_PRODUCT_NAME"] <- "PRODUCT_NAME"
  
  filter_set = unique(filter_set[, c(
	"PRODUCT_NAME", 
	"TEST_TYPE", 
	"TEST_UID", 
	"TEST_WORK_WEEK", 
	"LOCATION_CODE", 
	"PRODUCT_INTERFACE",
	"PRODUCT_FORMAT_CAPACITY"
  )])
  
  filter_set = inner_join(filter_set, param_dic)
  
  saveRDS(param_dic, file.path(output_path, "param_dic.rda"))
  saveRDS(filter_set, file.path(output_path, "filter.rda"))
  
  ### FAILURE #####
  
  # library(RPostgreSQL)
  # library(DBI)
  # library(dplyr)
  # library(tidyr)
  
  # con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                        # host   = "tep-rshiny1.tep.thai.seagate.com",
                        # db = "Factory",
                        # user     = "postgres",
                        # password = "1qa2ws3ed",
                        # port     = 5432)
  
  # res <- dbSendQuery(con, "SELECT * FROM odt_failure_tracker_view;")
  
  # clca_data <- as.data.frame(dbFetch(res), stringsAsFactors = FALSE)
  
  # DBI::dbDisconnect(con)
  
  ## names(clca_data)[names(clca_data) == 'combine_product'] <- "PRODUCT_NAME"
  # names(clca_data)[names(clca_data) == 'ar_failing_head_num'] <- "HD_PHYS_PSN"
  
  # names(clca_data) <- toupper(names(clca_data))
  
  # if(sum(clca_data$HD_PHYS_PSN %in% c("NA", "n/a")) > 0){
	# clca_data[clca_data$HD_PHYS_PSN %in% c("NA", "n/a"),]$HD_PHYS_PSN  <- NA
  # }
  
  # clca_data = clca_data %>% 
    # mutate(HD_PHYS_PSN = strsplit(as.character(HD_PHYS_PSN), ",")) %>%
    # unnest(HD_PHYS_PSN)
    
  # clca_data = clca_data[,c("FISCAL_YEAR_WEEK","DRIVE_SERIAL_NUM","HD_PHYS_PSN","FAILURE_MODE","FAIL_CODE","TIME_TO_FAILURE")]
  
  # saveRDS(clca_data, file.path(output_path, "fail_detail.rda"))
  
  # gc()
  
  library(tidyr)
  
  clca_data <- readRDS(file.path(input_path, "webfact_sof_doc.rda"))
  
  clca_data <- clca_data[,c("DRIVE_SERIAL_NUM", "DOCFAILHD_FAIL_HEADS", "PRIMARY_SYMPTOM", "STATE", "STATUS", "TEST_NAME", "TIME_TO_FAILURE", "PRODUCT_INTERNAL_NAME")]

  names(clca_data)[names(clca_data) %in% "DOCFAILHD_FAIL_HEADS"] <- "HD_PHYS_PSN"
  names(clca_data)[names(clca_data) %in% "PRODUCT_INTERNAL_NAME"] <- "PRODUCT_NAME"

  if(sum(clca_data$HD_PHYS_PSN %in% c("NA", "n/a")) > 0){
	clca_data[clca_data$HD_PHYS_PSN %in% c("NA", "n/a"),]$HD_PHYS_PSN  <- NA
  }

  clca_data = clca_data %>%
	mutate(HD_PHYS_PSN = strsplit(as.character(HD_PHYS_PSN), ",")) %>%
	unnest(HD_PHYS_PSN)
  
  saveRDS(clca_data, file.path(output_path, "fail_detail.rda"))
  
  gc()
  
  # library(RPostgreSQL)
  # library(DBI)
  # library(dplyr)
  # library(tidyr)
  
  # con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                        # host   = "tep-rshiny1.tep.thai.seagate.com",
                        # db = "Factory",
                        # user     = "postgres",
                        # password = "1qa2ws3ed",
                        # port     = 5432)
  
  # res <- dbSendQuery(con, "SELECT * FROM odt_failure_tracker_view;")
  
  # clca_data <- as.data.frame(dbFetch(res), stringsAsFactors = FALSE)
  
  # DBI::dbDisconnect(con)
  
  # names(clca_data)[names(clca_data) == 'combine_product'] <- "PRODUCT_NAME"
  # names(clca_data)[names(clca_data) == 'ar_failing_head_num'] <- "HD_PHYS_PSN"
  
  # names(clca_data) <- toupper(names(clca_data))
  
  # if(sum(clca_data$HD_PHYS_PSN %in% c("NA", "n/a")) > 0){
    # clca_data[clca_data$HD_PHYS_PSN %in% c("NA", "n/a"),]$HD_PHYS_PSN  <- NA
  # }
  
  # clca_data = clca_data %>% 
    # mutate(HD_PHYS_PSN = strsplit(as.character(HD_PHYS_PSN), ",")) %>%
    # unnest(HD_PHYS_PSN)
  
  # clca_data = clca_data[,c("FISCAL_YEAR_WEEK","DRIVE_SERIAL_NUM","HD_PHYS_PSN","FAILURE_MODE","FAIL_CODE","TIME_TO_FAILURE")]
  
  # saveRDS(clca_data, file.path(output_path, "fail_detail.rda"))
  
  # gc()
  
}