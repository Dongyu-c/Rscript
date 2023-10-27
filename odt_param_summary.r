
custom_data_prep = function(input_path, output_path) {
  library(reshape2)
  library(dplyr)
  
  odt_param_file = list.files(input_path)
  
  file_remover = c("filter.rda")
  
  odt_param_file = odt_param_file[!grepl(".gz", odt_param_file) &
                                    !(odt_param_file %in% file_remover)]
									
  param_dic = data.frame()
  
  for (f in odt_param_file) {
    tryCatch({
      print(paste("Processing file:", f))
      
      data_set = readRDS(file.path(input_path, f))
      
      names(data_set) = toupper(names(data_set))
      
      names(data_set)[names(data_set) %in% "PRODUCT_INTERNAL_NAME"] <-
        "PRODUCT_NAME"
      names(data_set)[names(data_set) %in% "ODT_OPERATION"] <-
        "ODT_GROUP"
      
      # data_set = data_set %>% mutate(ODT_GROUP = ifelse(OPERATION == "CST2", "WEEKLY", "EVAL"))
      
	  param_temp = data.frame()
	  
      if (f == "p_defect.rda") {
        print("Running P-Defect data prep")
        
        data_set = data_set[complete.cases(data_set$DEFECT_TYPE), ]
        
        for (i in unique(data_set$DEFECT_TYPE)) {
          temp_data = data_set[data_set$DEFECT_TYPE %in% i, ]
          
          temp_data = temp_data[, c(
            'FISCAL_YEAR_WEEK',
            'PRODUCT_NAME',
            'DRIVE_SERIAL_NUM',
            "LBA_FORMAT",
            "ODT_GROUP",
            "EVENT_DATE",
            "DEFECT_TYPE",
            "DEFECT_COUNT"
          )]
          
          temp_data = melt(
            temp_data,
            id.vars = c(
              'FISCAL_YEAR_WEEK',
              'PRODUCT_NAME',
              'DRIVE_SERIAL_NUM',
              'LBA_FORMAT',
              'ODT_GROUP',
              'EVENT_DATE',
              'DEFECT_TYPE'
            )
          )
          
          temp_data = unique(temp_data)
          
          temp_data = dcast(
            temp_data,
            FISCAL_YEAR_WEEK + PRODUCT_NAME + DRIVE_SERIAL_NUM + LBA_FORMAT + ODT_GROUP + EVENT_DATE ~ DEFECT_TYPE
          )
          
          if (nrow(param_temp) > 0) {
            param_temp <- left_join(param_temp, temp_data)
          } else {
            param_temp <- temp_data
          }
          
          rm(temp_data)
          
        }
        
        join_set = unique(data_set[, names(data_set) %in% c(
          "DRIVE_SERIAL_NUM",
          "DRIVE_PART_NUM",
          "DRIVE_SBR_NUM",
          "LOCATION_CODE",
          "EVENT_DATE",
          "PRODUCT_INTERFACE",
          "PRODUCT_FORMAT_CAPACITY",
          "RUN_TYPE_PRIME_REWORK",
          "LBA_FORMAT",
          "TEMP_PROFILE",
          "TEST_TIME_CUM"
        )])
        
        join_set = unique(join_set %>% group_by(TEST_TIME_CUM = max(TEST_TIME_CUM, na.rm = TRUE)))
        
        param_temp = left_join(param_temp, join_set)
        
        rm(join_set)
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("GLIST", "RLIST")) {
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c(
            "PRODUCT_NAME",
            "ODT_GROUP",
            "LOCATION_CODE",
            "DRIVE_SBR_NUM",
            "PRODUCT_INTERFACE",
            "PRODUCT_FORMAT_CAPACITY",
            "RUN_TYPE_PRIME_REWORK",
            "TEMP_PROFILE"
          )])
          
          dic_temp =
            merge(dic_temp,
                  data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "p_delta_mre.rda") {
        print("Running MRE data prep")
        
        data_set = data_set[complete.cases(data_set$DELTA_MRE), ]
        
        data_set = data_set[data_set$TEST_SEQ_EVENT == 1, ]
        
        PRE_TEST = data_set %>% group_by(
          FISCAL_YEAR_WEEK,
          PRODUCT_NAME,
          DRIVE_SERIAL_NUM,
          HD_PHYS_PSN,
          TEST_SEQ_EVENT,
          ODT_GROUP,
          EVENT_DATE
        ) %>% arrange(EVENT_DATE, SPC_ID) %>% filter(row_number() == 1) %>% summarise(TEST = "PRE", DELTA_MRE = as.numeric(DELTA_MRE))
        POST_TEST = data_set %>% group_by(
          FISCAL_YEAR_WEEK,
          PRODUCT_NAME,
          DRIVE_SERIAL_NUM,
          HD_PHYS_PSN,
          TEST_SEQ_EVENT,
          ODT_GROUP,
          EVENT_DATE
        ) %>% arrange(EVENT_DATE, SPC_ID) %>% filter(row_number() == n() &
                                                       row_number() != 1) %>% summarise(TEST = "POST", DELTA_MRE = as.numeric(DELTA_MRE))
        
        param_temp = bind_rows(PRE_TEST, POST_TEST)
        
        rm(PRE_TEST, POST_TEST)
        
        join_set = unique(data_set[, names(data_set) %in% c(
          "DRIVE_SERIAL_NUM",
          "DRIVE_PART_NUM",
          "DRIVE_SBR_NUM",
          "LOCATION_CODE",
          "PRODUCT_INTERFACE",
          "PRODUCT_FORMAT_CAPACITY",
          "RUN_TYPE_PRIME_REWORK",
          "TEMP_PROFILE"
        )])
        
        param_temp = left_join(param_temp, join_set)
		param_temp =  param_temp[, unique(names(param_temp))]
        
        rm(join_set)
        
        param_temp = param_temp %>% ungroup()
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        dic_temp =
          merge(
            data.frame(
              PRODUCT_NAME = unique(param_temp[complete.cases(param_temp$DELTA_MRE),]$PRODUCT_NAME),
              stringsAsFactors = F
            ),
            data.frame(PARAM_NAME = c("DELTA_MRE"))
          )
        
        dic_temp = unique(param_temp[complete.cases(param_temp$DELTA_MRE), c(
          "PRODUCT_NAME",
          "ODT_GROUP",
          "LOCATION_CODE",
          "DRIVE_SBR_NUM",
          "PRODUCT_INTERFACE",
          "PRODUCT_FORMAT_CAPACITY",
          "RUN_TYPE_PRIME_REWORK",
          "TEMP_PROFILE"
        )])
        
        dic_temp =
          merge(dic_temp,
                data.frame(PARAM_NAME = "DELTA_MRE"))
        
        param_dic <- rbind(dic_temp, param_dic)
        
        rm(param_temp, dic_temp)
        
      } else if (f == "p_format_zone.rda") {
        print("Running RRAW data prep")
        
        param_temp = data_set[complete.cases(data_set$HD_PHYS_PSN), ]
		
		join_set = unique(data_set[, names(data_set) %in% c(
          "DRIVE_SERIAL_NUM",
          "DRIVE_PART_NUM",
          "DRIVE_SBR_NUM",
          "LOCATION_CODE",
          "EVENT_DATE",
          "PRODUCT_INTERFACE",
          "PRODUCT_FORMAT_CAPACITY",
          "RUN_TYPE_PRIME_REWORK",
          "TEMP_PROFILE"
        )])
		
		param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN), c(
          "FISCAL_YEAR_WEEK",
          "PRODUCT_NAME",
          "DRIVE_SERIAL_NUM",
          "HD_PHYS_PSN",
          "ODT_GROUP",
          "EVENT_DATE",
          "DATA_ZONE",
          "RAW_ERROR_RATE_OD",
          "RAW_ERROR_RATE_ID"
        )])
        
        # OD_ZONE = data_set %>% group_by(FISCAL_YEAR_WEEK,PRODUCT_NAME,DRIVE_SERIAL_NUM,HD_PHYS_PSN,ODT_GROUP,EVENT_DATE) %>% arrange(EVENT_DATE, DATA_ZONE) %>% filter(row_number()==1) %>% summarise(RAW_ERROR_RATE_OD = as.numeric(RAW_ERROR_RATE))
        # ID_ZONE = data_set %>% group_by(FISCAL_YEAR_WEEK,PRODUCT_NAME,DRIVE_SERIAL_NUM,HD_PHYS_PSN,ODT_GROUP,EVENT_DATE) %>% arrange(EVENT_DATE, DATA_ZONE) %>% filter(row_number()==n() & row_number()!=1) %>% summarise(RAW_ERROR_RATE_ID = as.numeric(RAW_ERROR_RATE))
        
        # param_temp <- left_join(OD_ZONE, ID_ZONE)
        
        # rm(OD_ZONE, ID_ZONE)
        
        # join_set = unique(data_set[, names(data_set) %in% c(
          # "DRIVE_SERIAL_NUM",
          # "DRIVE_PART_NUM",
          # "DRIVE_SBR_NUM",
          # "LOCATION_CODE",
          # "EVENT_DATE",
          # "PRODUCT_INTERFACE",
          # "PRODUCT_FORMAT_CAPACITY",
          # "RUN_TYPE_PRIME_REWORK",
          # "TEMP_PROFILE"
        # )])
        
        param_temp = left_join(param_temp, join_set)
        
        rm(join_set)
        
        param_temp = param_temp %>% ungroup()
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("RAW_ERROR_RATE_ID", "RAW_ERROR_RATE_OD")) {
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c(
            "PRODUCT_NAME",
            "ODT_GROUP",
            "LOCATION_CODE",
            "DRIVE_SBR_NUM",
            "PRODUCT_INTERFACE",
            "PRODUCT_FORMAT_CAPACITY",
            "RUN_TYPE_PRIME_REWORK",
            "TEMP_PROFILE"
          )])
          
          dic_temp =
            merge(dic_temp,
                  data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "p_sdat_pes.rda") {
        print("Running RRO + NRRO data prep")
        
        join_set = unique(data_set[, names(data_set) %in% c(
          "DRIVE_SERIAL_NUM",
          "DRIVE_PART_NUM",
          "DRIVE_SBR_NUM",
          "LOCATION_CODE",
          "EVENT_DATE",
          "PRODUCT_INTERFACE",
          "PRODUCT_FORMAT_CAPACITY",
          "RUN_TYPE_PRIME_REWORK",
          "TEMP_PROFILE"
        )])
        
        param_temp = unique(data_set[complete.cases(data_set$HD_PHYS_PSN), c(
          "FISCAL_YEAR_WEEK",
          "PRODUCT_NAME",
          "DRIVE_SERIAL_NUM",
          "HD_PHYS_PSN",
          "ODT_GROUP",
          "EVENT_DATE",
          "TEST",
          "NRRO_OD",
          "RRO_OD",
          "NRRO_ID",
          "RRO_ID"
        )])
        
        # rm(data_set)
        
        # param_temp = param_temp %>% group_by(FISCAL_YEAR_WEEK,PRODUCT_NAME,DRIVE_SERIAL_NUM,HD_PHYS_PSN,ODT_GROUP,EVENT_DATE) %>% arrange(EVENT_DATE, OCCURRENCE, CYL) %>% dplyr::mutate(ID = row_number(), TEST = ifelse(ID < 3, "PRE", "POST"))
        
        # OD_ZONE = param_temp %>% group_by(FISCAL_YEAR_WEEK,PRODUCT_NAME,DRIVE_SERIAL_NUM,HD_PHYS_PSN,ODT_GROUP,EVENT_DATE,TEST) %>% arrange(EVENT_DATE, OCCURRENCE, CYL, ID) %>% filter(row_number()==1) %>% summarise(NRRO_OD = as.numeric(NRRO), RRO_OD = as.numeric(RRO))
        # ID_ZONE = param_temp %>% group_by(FISCAL_YEAR_WEEK,PRODUCT_NAME,DRIVE_SERIAL_NUM,HD_PHYS_PSN,ODT_GROUP,EVENT_DATE,TEST) %>% arrange(EVENT_DATE, OCCURRENCE, CYL, ID) %>% filter(row_number()==2) %>% summarise(NRRO_ID = as.numeric(NRRO), RRO_ID = as.numeric(RRO))
        
        # param_temp <- left_join(OD_ZONE, ID_ZONE)
        
        # rm(OD_ZONE, ID_ZONE)
        
        param_temp = left_join(param_temp, join_set)
        param_temp =  param_temp[, unique(names(param_temp))]
        
        rm(join_set)
        
        param_temp = param_temp %>% ungroup()
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("NRRO_ID", "RRO_ID", "NRRO_OD", "RRO_OD")) {
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c(
            "PRODUCT_NAME",
            "ODT_GROUP",
            "LOCATION_CODE",
            "DRIVE_SBR_NUM",
            "PRODUCT_INTERFACE",
            "PRODUCT_FORMAT_CAPACITY",
            "RUN_TYPE_PRIME_REWORK",
            "TEMP_PROFILE"
          )])
          
          dic_temp =
            merge(dic_temp,
                  data.frame(PARAM_NAME = p))
          
          param_dic <- rbind(dic_temp, param_dic)
          
        }
        
        rm(param_temp, dic_temp)
        
      } else if (f == "p528_timed_pwrup.rda") {
        print("Running POWERUP_TIME data prep")
        
        param_temp = unique(data_set[complete.cases(data_set$POWERUP_TIME), c(
          "FISCAL_YEAR_WEEK",
          "DRIVE_SERIAL_NUM",
          "DRIVE_PART_NUM",
          "DRIVE_SBR_NUM",
          "LOCATION_CODE",
          "EVENT_DATE",
          "PRODUCT_INTERFACE",
          "PRODUCT_FORMAT_CAPACITY",
          "RUN_TYPE_PRIME_REWORK",
          "TEMP_PROFILE",
          "PRODUCT_NAME",
          "ODT_GROUP",
          "POWERUP_TIME"
        )])
        
        param_temp =  param_temp[, unique(names(param_temp))]
        
        saveRDS(param_temp, file = file.path(output_path, f))
        
        for (p in c("POWERUP_TIME")) {
          dic_temp = unique(param_temp[complete.cases(param_temp[[p]]), c(
            "PRODUCT_NAME",
            "ODT_GROUP",
            "LOCATION_CODE",
            "DRIVE_SBR_NUM",
            "PRODUCT_INTERFACE",
            "PRODUCT_FORMAT_CAPACITY",
            "RUN_TYPE_PRIME_REWORK",
            "TEMP_PROFILE"
          )])
          
          dic_temp =
            merge(dic_temp,
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
  names(filter_set)[names(filter_set) %in% "PRODUCT_INTERNAL_NAME"] <-
    "PRODUCT_NAME"
  names(filter_set)[names(filter_set) %in% "ODT_OPERATION"] <-
    "ODT_GROUP"
  
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
  
  saveRDS(filter_set, file.path(output_path, "filter.rda"))
  
  ### FAILURE #####
  
  library(dplyr)
  library(tidyr)
  
  failure_set <- readRDS(
    file.path(input_path, "fail_detail.rda"))
    
    names(failure_set) <-
      toupper(names(failure_set))
    
    names(failure_set)[names(failure_set) %in% "AR_FAILING_HEAD_NUM"] <-
      "HD_PHYS_PSN"
    
    failure_set <-
      failure_set[, c(
        "FISCAL_YEAR_WEEK",
        "DRIVE_SERIAL_NUM",
        "HD_PHYS_PSN",
        "FAILURE_MODE",
        "FAIL_CODE",
        "TIME_TO_FAILURE"
      )]
    
    if (any(failure_set$HD_PHYS_PSN %in% c('<NA>', 'n/a', 'NA'))) {
      failure_set[failure_set$HD_PHYS_PSN %in% c('<NA>', 'n/a', 'NA'), ]$HD_PHYS_PSN <-
        NA
      
    }
    
    failure_set$HD_PHYS_PSN <-
      stringr::str_replace_all(failure_set$HD_PHYS_PSN, " ", "")
    
    failure_set <-
      failure_set %>% separate_rows(HD_PHYS_PSN)
    
    saveRDS(filter_set, file.path(output_path, "fail_detail.rda"))
    
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
    
}