custom_data_prep = function(input_path, output_path){
  
  library(stringr)
  
  replace_test_type <- function(DT) {
    name_set = unique(DT$ORT_CEE_TEST_UID)
    
    sub_names = unique(DT$PRODUCT_NAME)
    
    if (!is.null(name_set) &
        length(name_set[substring(name_set, 1, 2) == "FW"]) > 0) {
      name_set = name_set[substring(name_set, 1, 2) == "FW"]
      
      name_set = name_set[grep(sub_names, name_set)]
      
      DT_temp = data.frame(
        "ORT_CEE_TEST_UID" = name_set[substring(name_set, 1, 2) == "FW"],
        "ORT_CEE_TEST_TYPE" = str_replace(name_set, paste0("FW*.*", sub_names, "_"), ""),
        "ORT_CEE_TEST_WORK_WEEK" = substring(name_set, 1, 6),
        stringsAsFactors = F
      )
      
      DT_temp = DT_temp[!is.na(DT_temp$ORT_CEE_TEST_UID),]
      
      # DT_tuid = merge(
      # DT[, c("ORT_CEE_TEST_UID",
      # "ORT_CEE_TEST_TYPE",
      # "ORT_CEE_TEST_WORK_WEEK")],
      # DT_temp,
      # by = "ORT_CEE_TEST_UID",
      # all.x = TRUE,
      # stringsAsFactors = F
      # )
      
      # DT_tuid = transform(
      # DT_tuid,
      # ORT_CEE_TEST_TYPE = ifelse(
      # is.na(ORT_CEE_TEST_TYPE.y),
      # ORT_CEE_TEST_TYPE.x,
      # ORT_CEE_TEST_TYPE.y
      # ),
      # ORT_CEE_TEST_WORK_WEEK = ifelse(
      # is.na(ORT_CEE_TEST_WORK_WEEK.y),
      # ORT_CEE_TEST_WORK_WEEK.x,
      # ORT_CEE_TEST_WORK_WEEK.y
      # ),
      # stringsAsFactors = F
      # )
      
      # DT[order(DT$`_obs_`),]$ORT_CEE_TEST_TYPE <-
      # DT_tuid[order(DT_tuid$X_obs_),]$ORT_CEE_TEST_TYPE
      
      # DT[order(DT$`_obs_`),]$ORT_CEE_TEST_WORK_WEEK <-
      # DT_tuid[order(DT_tuid$X_obs_),]$ORT_CEE_TEST_WORK_WEEK
      
      DT <- DT %>% 
        left_join(DT_temp,  by = "ORT_CEE_TEST_UID") %>% 
        mutate(ORT_CEE_TEST_TYPE = ifelse(.$ORT_CEE_TEST_TYPE.y %in% DT_temp$ORT_CEE_TEST_TYPE, .$ORT_CEE_TEST_TYPE.y, DT$ORT_CEE_TEST_TYPE),
               ORT_CEE_TEST_WORK_WEEK = CEE_TEST_WORK_WEEK)
      
      DT <- DT[,!grepl(".y|.x",names(DT))]
      
      # if (any(grepl("_", DT$ORT_CEE_TEST_WORK_WEEK))) {
      # DT <- DT[!grepl("_", DT$ORT_CEE_TEST_WORK_WEEK),]
      # }
      
      return(DT)
      
      gc()
      
    }
    
  }
  
  aging_cal_process <- function(DT){
    
    DT = DT %>% mutate(MOTOR_FISCAL_YEAR = paste0(substr(ETL_LOAD_DATE, 1, 3), substr(MOTOR_DATE_CODE, 1, 1)),
                       MOTOR_FISCAL_WEEK	= substr(MOTOR_DATE_CODE, 2, 3),	
                       MOTOR_DAY_NUM = substr(MOTOR_DATE_CODE, 4, 4),
                       TOP_COVER_DATE_CODE = substr(TOP_COVER_LOT, 8, 12),
                       TOP_COVER_FISCAL_YEAR = paste0(substr(ETL_LOAD_DATE, 1, 3), substr(TOP_COVER_DATE_CODE, 1, 1)),
                       TOP_COVER_FISCAL_WEEK	= substr(TOP_COVER_DATE_CODE, 2, 3),	
                       TOP_COVER_DAY_NUM = substr(TOP_COVER_DATE_CODE, 4, 4)
    )
    
    date_dic = read.csv("/seamnt/sasdata/sasdata/ecube/results/E1061857/FINANCIAL_MONTH_RAW.csv")
    
    date_dic = date_dic %>% mutate(MOTOR_FISCAL_YEAR = as.character(FISCAL_YEAR),
                                   MOTOR_FISCAL_WEEK = as.character(FISCAL_WEEK),
                                   MOTOR_DAY_NUM = as.character(DAY_FISCAL_WEEK_NUM),
                                   MOTOR_ASSEMBLY_DATE = DATE_KEY,
                                   TOP_COVER_FISCAL_YEAR = as.character(FISCAL_YEAR),
                                   TOP_COVER_FISCAL_WEEK = as.character(FISCAL_WEEK),
                                   TOP_COVER_DAY_NUM = as.character(DAY_FISCAL_WEEK_NUM),
                                   TGA_FIPG_DATE = DATE_KEY)
    
    date_dic$MOTOR_ASSEMBLY_DATE = as.Date(as.character(date_dic$MOTOR_ASSEMBLY_DATE), format = "%Y%m%d")
    date_dic$TGA_FIPG_DATE = as.Date(as.character(date_dic$TGA_FIPG_DATE), format = "%Y%m%d")
    
    motor_date_dic = date_dic[,c("MOTOR_FISCAL_YEAR", "MOTOR_FISCAL_WEEK", "MOTOR_DAY_NUM", "MOTOR_ASSEMBLY_DATE")]
    
    top_date_dic = date_dic[,c("TOP_COVER_FISCAL_YEAR", "TOP_COVER_FISCAL_WEEK", "TOP_COVER_DAY_NUM", "TGA_FIPG_DATE")]
    
    rm(date_dic)
    
    DT$MOTOR_FISCAL_WEEK <- as.character(DT$MOTOR_FISCAL_WEEK)
    DT$MOTOR_DAY_NUM <- as.character(DT$MOTOR_DAY_NUM)
    
    DT$TOP_COVER_FISCAL_WEEK <- as.character(DT$TOP_COVER_FISCAL_WEEK)
    DT$TOP_COVER_DAY_NUM <- as.character(DT$TOP_COVER_DAY_NUM)
    
    DT = left_join(DT, motor_date_dic)
    DT = left_join(DT, top_date_dic)
    
    DT$CLRM_EXIT_DATE = as.Date(as.character(DT$CLRM_EXIT_DATE), format = "%Y%m%d")
    
    DT = DT %>% mutate(MOTOR_DATE_DIFF = difftime(DT$CLRM_EXIT_DATE, DT$MOTOR_ASSEMBLY_DATE, units = "days"),
                       TOP_COVER_DATE_DIFF = difftime(DT$CLRM_EXIT_DATE, DT$TGA_FIPG_DATE, units = "days"))
    # motor_date_diff_grp = ifelse(motor_date_diff >= 35, ">= 35 days", "< 35 days"),
    # top_cover_date_diff_grp = ifelse(top_cover_date_diff >= 15, ">= 15 days", "< 15 days"))
    
    gc()
    
    return(DT)
    
  }
  
  inputFilePath = input_path
  joinFilePath = "/seamnt/sasdata/sasdata/ecube/results/E1052603/Raw/ODS"
  NewPath = output_path
  
  library(stringr)
  
  data_file = list.files(inputFilePath)[grep("RELI_DEEP_DIVE_", list.files(inputFilePath))]
  
  join_file = list.files(joinFilePath)
  
  if (length(data_file[!grepl(".rda.gz", data_file)]) != 0) {
    data_file = data_file[!grepl(".rda.gz", data_file)]
  }
  
  # source(file.path(UploadPath, GID, file_names), local = TRUE)
  
  for (i in data_file) {
    
    if (!grepl(".rda.gz", i)) {
      
      print(paste0("file: ", i))
      
      tryCatch({
        temp_set <- readRDS(file.path(inputFilePath, i))
        
        temp_set <- temp_set[,!(names(temp_set) %in% "OPERATION")]
        
        temp_set <- aging_cal_process(temp_set)
        
        pro_name = unique(temp_set$PRODUCT_NAME)
        
        print(paste0("Running Prep: ", pro_name))
        
        if (length(pro_name) > 1) {
          for (j in pro_name) {
            decoded = replace_test_type(temp_set[temp_set$PRODUCT_NAME == j,])
            
            if (is.null(decoded)) {
              
              data_set = temp_set[temp_set$PRODUCT_NAME == j,]
              
              if(any(grepl(j, join_file))){
                join_data = readRDS(paste0(joinFilePath,"/", j,".rda"))
                join_data = join_data[,!(names(join_data) %in%  c("CAPACITY","ETL_LOAD_DATE"))]
                
                data_set = dplyr::left_join(data_set, join_data)
              }
              
              # data_set = custom_function(data_set)
              
              saveRDS(data_set, file = paste0(NewPath, "/", j, ".rda"))
            } else {
              
              if(any(grepl(j, join_file))){
                join_data = readRDS(paste0(joinFilePath,"/", j,".rda"))
                join_data = join_data[,!(names(join_data) %in%  c("CAPACITY","ETL_LOAD_DATE"))]
                
                decoded = dplyr::left_join(decoded, join_data)
              }
              
              # decoded = custom_function(decoded)
              
              saveRDS(decoded, file = paste0(NewPath, "/", j, ".rda"))
            }
            
          }
          
        } else {
          decoded = replace_test_type(temp_set)
          
          if (is.null(decoded)) {
            
            if(any(grepl(pro_name, join_file))){
              join_data = readRDS(paste0(joinFilePath,"/", pro_name,".rda"))
              join_data = join_data[,!(names(join_data) %in%  c("CAPACITY","ETL_LOAD_DATE"))]
              
              data_set = dplyr::left_join(temp_set, join_data)
            }
            
            # data_set = custom_function(data_set)
            
            saveRDS(temp_set, file = paste0(NewPath, "/", pro_name, ".rda"))
          } else {
            
            if(any(grepl(pro_name, join_file))){
              join_data = readRDS(paste0(joinFilePath,"/", pro_name,".rda"))
              join_data = join_data[,!(names(join_data) %in%  c("CAPACITY","ETL_LOAD_DATE"))]
              
              decoded = dplyr::left_join(decoded, join_data)
            }
            
            # decoded = custom_function(decoded)
            
            saveRDS(decoded, file = paste0(NewPath, "/", pro_name, ".rda"))
          }
          
        }
        
        gc()
        
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
      
    }
    
  }
  
}