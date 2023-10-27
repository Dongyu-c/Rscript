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
  
  strt <<- Sys.time()
  
  print("Running INP QPM EDW naming convertion to DFD")
  
  library(stringr)
  library(data.table)
  
  qpm_rda_folder <- file.path(ResultsPath, "E1116383", "RDA_DATA")
  qpm_mod_folder <- file.path(ResultsPath, "E1116383", "RDA_MOD")
  
  if(!file.exists(qpm_mod_folder)){
	dir.create(qpm_mod_folder, showWarnings = FALSE)
  }
  
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
  
  par_dic <- readRDS(file.path(ResultsPath, "E1116383", "QPM_PARAMETER_DIC.rda"))
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
      
	  print(nrow(data_set))
	  
      if (commodity %in% c('HOOKUP', 'MOTOR', 'HGA')) {
        print(paste0("Sampling of ", commodity))
        
        if (nrow(data_set) > 0) {
          
          if("PRODUCT_TYPE" %in% names(data_set)){
            com_par_dic = par_dic[par_dic$COMMODITY_TYPE %in% unique(data_set$PRODUCT_TYPE),]
          } else {
            com_par_dic = data.frame()
          }
          
          if(nrow(com_par_dic) > 0) {
            
            col_match = names(data_set) %in% com_par_dic$`PARARMETER_NAME.IN.EDW`
            
            com_par_dic = com_par_dic[com_par_dic$`PARARMETER_NAME.IN.EDW` %in% names(data_set)[col_match], ]
            
            com_par_dic = com_par_dic[match(names(data_set)[col_match],
                                            com_par_dic$`PARARMETER_NAME.IN.EDW`), ]
            
            print("Parameter name with in the DFD dic")
            print(com_par_dic$`PARARMETER_NAME.IN.DFD`)
            
            print("Column which  match with the DFD dic")
            print(names(data_set)[col_match])
            
            names(data_set)[col_match] = com_par_dic$`PARARMETER_NAME.IN.DFD`
            
            print(paste0("COMMODITY : ", com))
            print(names(data_set))
            
          }
          
          # if (file.exists(file.path(qpm_mod_folder, com))) {
            # old_data_set = readRDS(file.path(qpm_mod_folder, com))
            
            # if (commodity %in% 'MOTOR') {
              ###old_data_set$GROUP_NUM <- as.integer(old_data_set$GROUP_NUM)
              ###data_set$GROUP_NUM <-
              ###as.integer(data_set$GROUP_NUM)
              
              ###data_set$RAMP_CAVITY_NUM <-
              ###as.integer(data_set$RAMP_CAVITY_NUM) 
              
              ###data_set$ACOUSTIC_SOUND_POWER <-
              ###as.integer(data_set$ACOUSTIC_SOUND_POWER)
              
              # old_data_set <- matchColClasses(data_set, old_data_set)
              # data_set <- matchColClasses(old_data_set, data_set)
              
              # print("##### OLD DATA #####")
              # str(old_data_set)
              
              # print("##### SAMPLE DATA #####")
              # str(data_set)
              
            # }
            
            # data_set <- bind_rows(data_set, old_data_set)
            
          # } else {
            # data_set <- data_set
            
          # }
          
          # if (commodity == "HGA") {
            # data_set <- data_set[data_set$TEST_DATE >= (as.Date(Sys.Date()) - 92),]
          # } else {
            # data_set <- data_set[as.Date(data_set$ETL_LOAD_DATE) >= as.Date(Sys.Date()) - 95,]
          # }
          
          # if (commodity == "HOOKUP") {
            # week_sub = unique(data_set$GRP_YEAR_WEEK)
            
            # week_sub = sort(week_sub, decreasing = T)[1:26]
            
            # data_set <- data_set[data_set$GRP_YEAR_WEEK %in% week_sub,]
          # } 
          
          # data_set <- data_set[as.Date(data_set$ETL_LOAD_DATE) >= as.Date(Sys.Date()) - 95,]
          
          # data_set <-
            # data_set[,colSums(is.na(data_set)) < nrow(data_set)]
          
		  print(nrow(data_set))
		  
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