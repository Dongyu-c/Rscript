custom_data_prep = function(input_path, output_path) {
  
  matchColClasses <- function(main_data, sub_data) {
    
    sharedColNames <- names(sub_data)[names(sub_data) %in% names(main_data)]
    
    col_checker <- data.frame(sharedColNames = sharedColNames)
    col_checker$main_ColTypes <- sapply(main_data[,sharedColNames], class)
    col_checker$sub_ColTypes <- sapply(sub_data[,sharedColNames], class)
    
    con_check <- as.character(col_checker$main_ColTypes) == as.character(col_checker$sub_ColTypes)
    col_checker$match_ColTypes <- con_check
    
    col_checker <- col_checker[col_checker$match_ColTypes %in% FALSE,]
    
    col_checker$main_is_numeric <- ifelse(col_checker$main_ColTypes == "numeric", TRUE, FALSE)
    col_checker <- col_checker[col_checker$main_is_numeric %in% TRUE,]
    
    print("Looping Convert Data")
    
    if(nrow(col_checker) > 0){
      
      for (n in col_checker$sharedColNames) {
        print(paste0("##### ", n))
        sub_data[[n]] <- as.numeric(sub_data[[n]])
        
      }
      
    }
    
    return(sub_data)
  }
  
  inp_qpm_sampling = function(latest_data, com) {
    samping_checker <- FALSE
    
    if(com != "HGA"){
      
      latest_data <- latest_data %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_NUM, DATA_SOURCE_TYPE) %>% mutate(FIRST_ETL_DATE = min(ETL_LOAD_DATE))
      
      print(paste0("Subsetting the date set"))
      
      latest_data <- latest_data[latest_data$FIRST_ETL_DATE < as.POSIXct.Date(Sys.Date() -3),]
      
    } else if (com != "HGA" & com == "HOOKUP") {
      
      latest_data <- latest_data %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_DATETIME, DATA_SOURCE_TYPE) %>% mutate(FIRST_ETL_DATE = min(ETL_LOAD_DATE))
      
      print(paste0("Subsetting the date set"))
      
      latest_data <- latest_data[latest_data$FIRST_ETL_DATE < as.POSIXct.Date(Sys.Date() -3),]
      
    }
    
    library(dplyr)
    
    print(paste0("Checking for sampling info"))
    
    qpm_sample_folder <- file.path(Results, "SAMPLE_INFO")
    qpm_sample_file_info <- paste0(tolower(com), "_sample_info.csv")
    qpm_sample_file_rda <- paste0(tolower(com), "_sample_info.rda")
    
    samping_checker <- FALSE
    
    # print("file.exists(file.path(qpm_sample_folder, qpm_sample_file_rda))")
    # print(file.exists(file.path(
    # qpm_sample_folder, qpm_sample_file_rda
    # )))
    
    if (file.exists(file.path(qpm_sample_folder, qpm_sample_file_rda))) {
      print("Removing exists combonations")
      
      # print(file.path(qpm_sample_folder, qpm_sample_file_rda))
      
      sample_log = readRDS(file.path(qpm_sample_folder, qpm_sample_file_rda))
      
      if (com == "HGA") {
        sample_prep_grouping <-
          latest_data[, c("PRODUCT_INTERNAL_NAME",
                          "PART_NUM",
                          "READER_INDEX",
                          "TEST_DATE")]
        sample_log$PART_NUM <- as.character(sample_log$PART_NUM)
        sample_prep_grouping <-
          anti_join(
            sample_prep_grouping,
            sample_log,
            by = c(
              "PRODUCT_INTERNAL_NAME",
              "PART_NUM",
              "READER_INDEX",
              "TEST_DATE"
            )
          )
        
      } else if (com == "HOOKUP") {
        sample_prep_grouping <-
          latest_data[, c("PART_NUM",
                          "SUPPLIER_NAME",
                          "GROUP_DATETIME",
                          "DATA_SOURCE_TYPE")]
        
        sample_log$PART_NUM <- as.character(sample_log$PART_NUM)
        
        sample_prep_grouping <-
          anti_join(
            sample_prep_grouping,
            sample_log,
            by = c(
              "PART_NUM",
              "SUPPLIER_NAME",
              "GROUP_DATETIME",
              "DATA_SOURCE_TYPE"
            )
          )
        
      } else {
        sample_prep_grouping <-
          latest_data[, c("PART_NUM",
                          "SUPPLIER_NAME",
                          "GROUP_NUM",
                          "DATA_SOURCE_TYPE")]
        
        sample_log$PART_NUM <- as.character(sample_log$PART_NUM)
        sample_log$GROUP_NUM <- as.integer(sample_log$GROUP_NUM)
        sample_prep_grouping$GROUP_NUM <-
          as.integer(sample_prep_grouping$GROUP_NUM)
        
        sample_prep_grouping <-
          anti_join(
            sample_prep_grouping,
            sample_log,
            by = c(
              "PART_NUM",
              "SUPPLIER_NAME",
              "GROUP_NUM",
              "DATA_SOURCE_TYPE"
            )
          )
        
      }
      
      print(paste0("#Rows for sampling = ", nrow(sample_prep_grouping)))
      
      if (nrow(sample_prep_grouping) > 0) {
        print("There are data that need to be sample")
        
        if (com == "HGA") {
          sample_size_group <-
            sample_prep_grouping %>% group_by(PRODUCT_INTERNAL_NAME,
                                              PART_NUM,
                                              READER_INDEX,
                                              TEST_DATE) %>% dplyr::summarise(QTY = n())
        } else if (com == "HOOKUP"){
          sample_size_group <-
            sample_prep_grouping %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_DATETIME, DATA_SOURCE_TYPE) %>% dplyr::summarise(QTY = n())
        } else {
          sample_size_group <-
            sample_prep_grouping %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_NUM, DATA_SOURCE_TYPE) %>% dplyr::summarise(QTY = n())
        }
        
        rm(sample_prep_grouping)
        
        if (com == "HGA") {
          etl_need_sampling <- sample_size_group[sample_size_group$QTY > 500, ]
          etl_no_need_sampling <-
            sample_size_group[sample_size_group$QTY <= 500, ]
          
        } else if (com == "HOOKUP") {
          etl_need_sampling <-
            sample_size_group[sample_size_group$QTY > 500, ]
          etl_no_need_sampling <-
            sample_size_group[sample_size_group$QTY <= 500, ]
          
        } else {
          etl_need_sampling <-
            sample_size_group[sample_size_group$QTY > 1000, ]
          etl_no_need_sampling <-
            sample_size_group[sample_size_group$QTY <= 1000, ]
          
        }
        
        temp_set <- data.frame()
        
        if (nrow(etl_need_sampling) > 0) {
          if (com == "HGA") {
            large_sample <-
              inner_join(
                latest_data,
                etl_need_sampling,
                by = c(
                  "PRODUCT_INTERNAL_NAME",
                  "PART_NUM",
                  "READER_INDEX",
                  "TEST_DATE"
                )
              )
			  
            large_sample <-
              large_sample %>% group_by(PRODUCT_INTERNAL_NAME,
                                        PART_NUM,
                                        READER_INDEX,
                                        TEST_DATE) %>% sample_n(500)
            
          } else if (com == "HOOKUP") {
            etl_need_sampling <- etl_need_sampling[-5]
            
            large_sample <-
              inner_join(
                latest_data,
                etl_need_sampling,
                by = c(
                  "PART_NUM",
                  "SUPPLIER_NAME",
                  "GROUP_DATETIME",
                  "DATA_SOURCE_TYPE"
                )
              )
            
            ##### SAMPLING FOR THE MOST COMPLETED DATA
            
            large_sample$COMPLETED_COLUMNS <- rowSums(!is.na(large_sample))
            
            first_sample_con <- large_sample %>% group_by(PART_NUM,
                                                          SUPPLIER_NAME,
                                                          GROUP_DATETIME,
                                                          DATA_SOURCE_TYPE,
                                                          COMPLETED_COLUMNS) %>% dplyr::summarise(N = n())
            
            first_sample_con <- as.data.frame(first_sample_con)
            
            first_sample_con <- first_sample_con %>% arrange(PART_NUM, SUPPLIER_NAME, GROUP_DATETIME, DATA_SOURCE_TYPE, desc(COMPLETED_COLUMNS)) %>%
              group_by(PART_NUM, SUPPLIER_NAME, GROUP_DATETIME, DATA_SOURCE_TYPE) %>% 
              mutate(Rank = order(COMPLETED_COLUMNS, decreasing = T), Cum_N = cumsum(N), Checker = Cum_N < 500, ALL_Checker = length(unique(Checker)) == 1)
            
            normal_sample_con <- first_sample_con %>% filter(Checker %in% FALSE & ALL_Checker %in% TRUE & Rank %in% 1)
            
            if (nrow(normal_sample_con) > 0) {
              
              normal_sample_set = normal_sample_con[, c("PART_NUM", "SUPPLIER_NAME", "GROUP_DATETIME", "DATA_SOURCE_TYPE", "COMPLETED_COLUMNS")]
              
              normal_sample_set = left_join(normal_sample_set, large_sample, by = c("PART_NUM", "SUPPLIER_NAME", "GROUP_DATETIME", "DATA_SOURCE_TYPE", "COMPLETED_COLUMNS"))
              
              large_sample = anti_join(large_sample, normal_sample_set, by = c("PART_NUM", "SUPPLIER_NAME", "GROUP_DATETIME", "DATA_SOURCE_TYPE", "COMPLETED_COLUMNS"))
              
              normal_sample_set = normal_sample_set %>% group_by(PART_NUM,
                                                                 SUPPLIER_NAME,
                                                                 GROUP_DATETIME,
                                                                 DATA_SOURCE_TYPE) %>% sample_n(500)
              
            }
            
            special_sample_con <- first_sample_con %>% filter(!Checker %in% FALSE && !ALL_Checker %in% TRUE) %>% arrange(PART_NUM, SUPPLIER_NAME, GROUP_DATETIME, DATA_SOURCE_TYPE, desc(COMPLETED_COLUMNS))
            
            if (nrow(special_sample_con) > 0) {
              
              no_sample_con = special_sample_con[special_sample_con$Checker %in% TRUE,]
              
              no_sample_con = no_sample_con %>% ungroup()
              
              sample_con = no_sample_con %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_DATETIME, DATA_SOURCE_TYPE) %>% dplyr::mutate(Rank = max(Rank) +  1, Sample_size = 500 - sum(N)) %>% ungroup()
              
              sample_con = unique(sample_con[,c("PART_NUM", "SUPPLIER_NAME", "GROUP_DATETIME", "DATA_SOURCE_TYPE", "Rank", "Sample_size")])
              
              sample_con = inner_join(sample_con, special_sample_con)
              
              sample_con = unique(sample_con[,c("PART_NUM", "SUPPLIER_NAME", "GROUP_DATETIME", "DATA_SOURCE_TYPE", "COMPLETED_COLUMNS", "Sample_size")])
              
              no_sample_con = unique(no_sample_con[,c("PART_NUM", "SUPPLIER_NAME", "GROUP_DATETIME", "DATA_SOURCE_TYPE", "COMPLETED_COLUMNS")])
              
              no_sample_set = inner_join(no_sample_con, large_sample, by = c("PART_NUM", "SUPPLIER_NAME", "GROUP_DATETIME", "DATA_SOURCE_TYPE", "COMPLETED_COLUMNS"))
              
			  sample_set = data.frame()
			  
              for(i in 1:nrow(sample_con)) {
                
                current_sample_con = sample_con[i,]
                
                current_sample_set = inner_join(current_sample_con, large_sample, by = c("PART_NUM", "SUPPLIER_NAME", "GROUP_DATETIME", "DATA_SOURCE_TYPE", "COMPLETED_COLUMNS"))
                
                current_sample_set = sample_n(current_sample_set, size = current_sample_con$Sample_size)
                
                sample_set = bind_rows(current_sample_set, sample_set)
                
              }
			  
			  sample_set = data.frame(sample_set)
			  
              no_sample_set = no_sample_set[,!names(no_sample_set) %in% c("COMPLETED_COLUMNS", "Sample_size")]
			  
			  print(names(no_sample_set))
			  
              sample_set = sample_set[,!names(sample_set) %in% c("COMPLETED_COLUMNS", "Sample_size")]
			  
			  print(names(sample_set))
              
              special_sample_set = bind_rows(sample_set, no_sample_set)
              
			  if (exists("normal_sample_set")) {
			  
				  if (nrow(normal_sample_set) > 0) {
					normal_sample_set = normal_sample_set[,!names(normal_sample_set) %in% c("COMPLETED_COLUMNS", "Sample_size")]
					special_sample_set  = bind_rows(normal_sample_set, special_sample_set)
					
				  }
			  
			  }
			  
            }
			
            large_sample <- special_sample_set
            
            #####
			
			 print("HOOKUP SAMPLING DONE")
            
          } else {
            etl_need_sampling <- etl_need_sampling[-5]
            
            etl_need_sampling$GROUP_NUM <-
              as.integer(etl_need_sampling$GROUP_NUM)
            latest_data$GROUP_NUM <- as.integer(latest_data$GROUP_NUM)
            
            large_sample <-
              inner_join(
                latest_data,
                etl_need_sampling,
                by = c(
                  "PART_NUM",
                  "SUPPLIER_NAME",
                  "GROUP_NUM",
                  "DATA_SOURCE_TYPE"
                )
              )
			  
            large_sample <-
              large_sample %>% group_by(PART_NUM,
                                        SUPPLIER_NAME,
                                        GROUP_NUM,
                                        DATA_SOURCE_TYPE) %>% sample_n(1000)
            
          }
		  
          temp_set <- bind_rows(large_sample, temp_set)
          
        }
        
        if (nrow(etl_no_need_sampling) > 0) {
          if (com == "HGA") {
            etl_no_need_sampling <- etl_no_need_sampling[-5]
            
            small_sample <-
              inner_join(
                latest_data,
                etl_no_need_sampling,
                by = c(
                  "PRODUCT_INTERNAL_NAME",
                  "PART_NUM",
                  "READER_INDEX",
                  "TEST_DATE"
                )
              )
            
          } else if (com == "HOOKUP") {
            etl_no_need_sampling <- etl_no_need_sampling[-5]
            
            small_sample <-
              inner_join(
                latest_data,
                etl_no_need_sampling,
                by = c(
                  "PART_NUM",
                  "SUPPLIER_NAME",
                  "GROUP_DATETIME",
                  "DATA_SOURCE_TYPE"
                )
              )
            
          } else {
            etl_no_need_sampling <- etl_no_need_sampling[-5]
            
            etl_no_need_sampling$GROUP_NUM <-
              as.integer(etl_no_need_sampling$GROUP_NUM)
            latest_data$GROUP_NUM <- as.integer(latest_data$GROUP_NUM)
            
            small_sample <-
              inner_join(
                latest_data,
                etl_no_need_sampling,
                by = c(
                  "PART_NUM",
                  "SUPPLIER_NAME",
                  "GROUP_NUM",
                  "DATA_SOURCE_TYPE"
                )
              )
            
          }
          
          temp_set <- bind_rows(small_sample, temp_set)
          
        }
        
        latest_data <- temp_set
        
        latest_data$SAMPLE_ON <- Sys.time()
		
        if (com == "HGA") {
          latest_sample_log <-
            latest_data %>% group_by(PRODUCT_INTERNAL_NAME,
                                     PART_NUM,
                                     READER_INDEX,
                                     TEST_DATE,
                                     SAMPLE_ON) %>% dplyr::summarise(QTY = n())
        } else if (com == "HOOKUP") {
          latest_sample_log <-
            latest_data %>% group_by(PART_NUM,
                                     SUPPLIER_NAME,
                                     GROUP_DATETIME,
                                     DATA_SOURCE_TYPE,
                                     SAMPLE_ON) %>% dplyr::summarise(FIRST_ETL_LOAD_DATE = min(ETL_LOAD_DATE),
                                                                     QTY = n())
        } else {
          latest_sample_log <-
            latest_data %>% group_by(PART_NUM,
                                     SUPPLIER_NAME,
                                     GROUP_NUM,
                                     DATA_SOURCE_TYPE,
                                     SAMPLE_ON) %>% dplyr::summarise(FIRST_ETL_LOAD_DATE = min(ETL_LOAD_DATE),
                                                                     QTY = n())
        }
        
        if (nrow(latest_sample_log) > 0) {
          if (!(com %in% c("HGA", "HOOKUP"))) {
            latest_sample_log$GROUP_NUM <-
              as.integer(latest_sample_log$GROUP_NUM)
          }
          
          # print("sample_log <- bind_rows(sample_log, latest_sample_log)")
          
          sample_log <- bind_rows(latest_sample_log, sample_log)
          
        }
        
        samping_checker <- TRUE
        
      }
      
    } else {
      if (com == "HGA") {
        sample_size_group <-
          latest_data %>% group_by(PRODUCT_INTERNAL_NAME,
                                   PART_NUM,
                                   READER_INDEX,
                                   TEST_DATE) %>% dplyr::summarise(QTY = n())
        
        etl_need_sampling <- sample_size_group[sample_size_group$QTY > 500, ]
      } else if (com == "HOOKUP") {
        sample_size_group <-
          latest_data %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_DATETIME, DATA_SOURCE_TYPE) %>% dplyr::summarise(QTY = n())
        etl_need_sampling <-
          sample_size_group[sample_size_group$QTY > 500, ]
      } else {
        sample_size_group <-
          latest_data %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_NUM, DATA_SOURCE_TYPE) %>% dplyr::summarise(QTY = n())
        etl_need_sampling <-
          sample_size_group[sample_size_group$QTY > 1000, ]
      }
      
      if (com == "HGA") {
        etl_need_sampling <- etl_need_sampling[-5]
        first_sampling <-
          inner_join(
            latest_data,
            etl_need_sampling,
            by = c(
              "PRODUCT_INTERNAL_NAME",
              "PART_NUM",
              "READER_INDEX",
              "TEST_DATE"
            )
          )
        first_sampling <-
          first_sampling %>% group_by(PRODUCT_INTERNAL_NAME,
                                      PART_NUM,
                                      READER_INDEX,
                                      TEST_DATE) %>% sample_n(500)
        latest_data <-
          anti_join(
            latest_data,
            etl_need_sampling,
            by = c(
              "PRODUCT_INTERNAL_NAME",
              "PART_NUM",
              "READER_INDEX",
              "TEST_DATE"
            )
          )
      } else if (com == "HOOKUP") {
        etl_need_sampling <- etl_need_sampling[-5]
        first_sampling <-
          inner_join(
            latest_data,
            etl_need_sampling,
            by = c(
              "PART_NUM",
              "SUPPLIER_NAME",
              "GROUP_DATETIME",
              "DATA_SOURCE_TYPE"
            )
          )
        
        first_sampling <-
          first_sampling %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_DATETIME, DATA_SOURCE_TYPE) %>% sample_n(500)

		latest_data <-
          anti_join(
            latest_data,
            etl_need_sampling,
            by = c(
              "PART_NUM",
              "SUPPLIER_NAME",
              "GROUP_DATETIME",
              "DATA_SOURCE_TYPE"
            )
          )
        
      } else {
        etl_need_sampling <- etl_need_sampling[-5]
        first_sampling <-
          inner_join(
            latest_data,
            etl_need_sampling,
            by = c(
              "PART_NUM",
              "SUPPLIER_NAME",
              "GROUP_NUM",
              "DATA_SOURCE_TYPE"
            )
          )
        first_sampling <-
          first_sampling %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_NUM, DATA_SOURCE_TYPE) %>% sample_n(1000)
        latest_data <-
          anti_join(
            latest_data,
            etl_need_sampling,
            by = c(
              "PART_NUM",
              "SUPPLIER_NAME",
              "GROUP_NUM",
              "DATA_SOURCE_TYPE"
            )
          )
      }
      
      latest_data <- bind_rows(latest_data, first_sampling)
      
      latest_data$SAMPLE_ON <- Sys.time()
      
      if (com == "HGA") {
        sample_log <-
          latest_data %>% group_by(PRODUCT_INTERNAL_NAME,
                                   PART_NUM,
                                   READER_INDEX,
                                   TEST_DATE,
                                   SAMPLE_ON) %>% dplyr::summarise(QTY = n())
      } else if (com == "HOOKUP") {
        sample_log <-
          latest_data %>% group_by(PART_NUM,
                                   SUPPLIER_NAME,
                                   GROUP_DATETIME,
                                   DATA_SOURCE_TYPE,
                                   SAMPLE_ON) %>% dplyr::summarise(FIRST_ETL_LOAD_DATE = min(ETL_LOAD_DATE),
                                                                   QTY = n())
      } else {
        sample_log <-
          latest_data %>% group_by(PART_NUM,
                                   SUPPLIER_NAME,
                                   GROUP_NUM,
                                   DATA_SOURCE_TYPE,
                                   SAMPLE_ON) %>% dplyr::summarise(FIRST_ETL_LOAD_DATE = min(ETL_LOAD_DATE),
                                                                   QTY = n())
      }
      
      gc()
      
      samping_checker <- TRUE
      
    }
    
    print("Writing data_set sampling log")
    
    print(head(samping_checker))
    
    if (samping_checker) {
      sample_log = sample_log %>% ungroup()
      saveRDS(sample_log, file  = file.path(qpm_sample_folder, qpm_sample_file_rda))
      
      write.csv(
        sample_log,
        file  = file.path(qpm_sample_folder, qpm_sample_file_info),
        row.names = FALSE
      )
      
      print(paste0("data_set after sampling: ", nrow(latest_data)))
      latest_data <- latest_data[, !(names(latest_data) %in% "FIRST_ETL_DATE")]
      
    }  else {
      print(paste0("No sampling was done"))
      latest_data <- data.frame()
      
    }
    
    return(latest_data)
    
  }
  
  strt <<- Sys.time()
  
  print("Running INP QPM EDW naming convertion to DFD")
  
  library(stringr)
  library(data.table)
  
  qpm_rda_folder <- file.path(ResultsPath, "E1065238", "RDA_DATA")
  qpm_mod_folder <- file.path(ResultsPath, "E1065238", "RDA_MOD")
  
  remove_list <-
    c(
      "QPM_OOC_RANKING_NEW@E1033851.csv",
      'INP_QPM_DASHBOARD_DIC.rda',
      'INP_QPM_DASHBOARD_DO.rda',
      'INP_QPM_DASHBOARD_CHART_KEY.rda',
      'INP_QPM_DASHBOARD_SQE_CTQ.rda',
      'INP_QPM_DASHBOARD_SPEC.rda',
      'INP_QPM_DASHBOARD_TRIGGER.rda'
    )
  
  qpm_file_list = list.files(qpm_rda_folder, pattern = "INP_QPM_DASHBOARD_")
  qpm_file_list = qpm_file_list[!(qpm_file_list %in% remove_list)]
  qpm_file_list = qpm_file_list[!grepl(".rda.gz|.csv|.csv.gz", qpm_file_list)]
  
  print(qpm_file_list)
  
  par_dic <- readRDS(file.path(ResultsPath, "E1065238", "QPM_PARAMETER_DIC.rda"))
  par_dic <- par_dic[!grepl('CCP', par_dic$COMMODITY_TYPE),-4]
  par_dic[grepl("MOTOR",par_dic$COMMODITY_TYPE),]$COMMODITY_TYPE <- "MO"
  par_dic <- par_dic[par_dic$`PARARMETER_NAME.IN.EDW` != 'SUPPLIER_KEY',]
  
  par_change_keeper <- data.frame()
  
  # qpm_file_list <- "INP_QPM_DASHBOARD_HOOKUP.rda"
  
  for (com in qpm_file_list) {
    tryCatch({
      commodity = stringr::str_remove_all(com, "INP_QPM_DASHBOARD_|.rda")
      
      print(commodity)
      
      data_set = readRDS(file.path(qpm_rda_folder, com))
      
      names(data_set) = toupper(names(data_set))
      
      if (commodity %in% c('HOOKUP', 'MOTOR', 'HGA')) {
        print(paste0("Sampling of ", commodity))
        
        print("Data sample starting")
        
        data_sample_set = inp_qpm_sampling(data_set, commodity)
        
        print("Data sample completed")
        
        if (nrow(data_sample_set) > 0) {
          
          if("PRODUCT_TYPE" %in% names(data_sample_set)){
            com_par_dic = par_dic[par_dic$COMMODITY_TYPE %in% unique(data_sample_set$PRODUCT_TYPE),]
          } else {
            com_par_dic = data.frame()
          }
          
          if(nrow(com_par_dic) > 0) {
            
            col_match = names(data_sample_set) %in% com_par_dic$`PARARMETER_NAME.IN.EDW`
            
            com_par_dic = com_par_dic[com_par_dic$`PARARMETER_NAME.IN.EDW` %in% names(data_sample_set)[col_match], ]
            
            com_par_dic = com_par_dic[match(names(data_sample_set)[col_match],
                                            com_par_dic$`PARARMETER_NAME.IN.EDW`), ]
            
            print("Parameter name with in the DFD dic")
            print(com_par_dic$`PARARMETER_NAME.IN.DFD`)
            
            print("Column which  match with the DFD dic")
            print(names(data_sample_set)[col_match])
            
            names(data_sample_set)[col_match] = com_par_dic$`PARARMETER_NAME.IN.DFD`
            
            print(paste0("COMMODITY : ", com))
            print(names(data_sample_set))
            
          }
          
          if (file.exists(file.path(qpm_mod_folder, com))) {
            old_data_set = readRDS(file.path(qpm_mod_folder, com))
            
            if (commodity %in% 'MOTOR') {
              # old_data_set$GROUP_NUM <- as.integer(old_data_set$GROUP_NUM)
              # data_sample_set$GROUP_NUM <-
              # as.integer(data_sample_set$GROUP_NUM)
              
              # data_sample_set$RAMP_CAVITY_NUM <-
              # as.integer(data_sample_set$RAMP_CAVITY_NUM) 
              
              # data_sample_set$ACOUSTIC_SOUND_POWER <-
              # as.integer(data_sample_set$ACOUSTIC_SOUND_POWER)
              
              old_data_set <- matchColClasses(data_sample_set, old_data_set)
              data_sample_set <- matchColClasses(old_data_set, data_sample_set)
              
              print("##### OLD DATA #####")
              str(old_data_set)
              
              print("##### SAMPLE DATA #####")
              str(data_sample_set)
              
            }
            
            data_set <- bind_rows(data_sample_set, old_data_set)
            
          } else {
            data_set <- data_sample_set
            
          }
          
          if (commodity == "HGA") {
            data_set <- data_set[data_set$TEST_DATE >= (as.Date(Sys.Date()) - 92),]
          } else {
            data_set <- data_set[as.Date(data_set$ETL_LOAD_DATE) >= as.Date(Sys.Date()) - 95,]
          }
          
          if (commodity == "HOOKUP") {
            week_sub = unique(data_set$GRP_YEAR_WEEK)
            
            week_sub = sort(week_sub, decreasing = T)[1:26]
            
            data_set <- data_set[data_set$GRP_YEAR_WEEK %in% week_sub,]
          } 
          
          data_set <- data_set[as.Date(data_set$ETL_LOAD_DATE) >= as.Date(Sys.Date()) - 95,]
          
          data_set <-
            data_set[,colSums(is.na(data_set)) < nrow(data_set)]
          
          saveRDS(data_set, file = file.path(qpm_mod_folder, com))
          
        } else {
          print("No new data is needed to be sample")
          
        }
        
      } else {
        com_par_dic = par_dic[par_dic$COMMODITY_TYPE %in% unique(data_set$PRODUCT_TYPE), ]
        
        col_match = names(data_set) %in% com_par_dic$`PARARMETER_NAME.IN.EDW`
        
        com_par_dic = com_par_dic[com_par_dic$`PARARMETER_NAME.IN.EDW` %in% names(data_set)[col_match], ]
        
        com_par_dic = com_par_dic[match(names(data_set)[col_match],
                                        com_par_dic$`PARARMETER_NAME.IN.EDW`), ]
        
        print("com_par_dic$`PARARMETER_NAME.IN.DFD`")
        print(com_par_dic$`PARARMETER_NAME.IN.DFD`)
        
        print("names(data_set)[col_match]")
        print(names(data_set)[col_match])
        
        names(data_set)[col_match] <-
          com_par_dic$`PARARMETER_NAME.IN.DFD`
        
        data_set <-
          data_set[, colSums(is.na(data_set)) < nrow(data_set)]
        
        saveRDS(data_set, file = file.path(qpm_mod_folder, com))
        
      }
      
      if (exists('com_par_dic')) {
        par_change_keeper <- bind_rows(par_change_keeper, com_par_dic)
      } else {
        par_change_keeper
      }
      
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
    
    gc()
    
  }
  
  par_change_keeper <-
    par_change_keeper[complete.cases(par_change_keeper), ]
  
}