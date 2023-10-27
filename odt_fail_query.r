custom_data_prep = function(input_path, output_path){

  library(dplyr)
  library(tidyr)
  
  failure_set <- readRDS(file.path(input_path, "odt_failure.rda"))
  
  names(failure_set) <- toupper(names(failure_set))
  
  names(failure_set)[names(failure_set) %in% "AR_FAILING_HEAD_NUM"] <- "HD_PHYS_PSN"
  
  failure_set <- failure_set[,c("FISCAL_YEAR_WEEK", "DRIVE_SERIAL_NUM", "HD_PHYS_PSN", "FAILURE_MODE", "FAIL_CODE", "TIME_TO_FAILURE")]
  
  if (any(failure_set$HD_PHYS_PSN %in% c('<NA>', 'n/a', 'NA'))) {
  
	failure_set[failure_set$HD_PHYS_PSN %in% c('<NA>', 'n/a', 'NA'),]$HD_PHYS_PSN <- NA
  
  }
  
  failure_set$HD_PHYS_PSN <- stringr::str_replace_all(failure_set$HD_PHYS_PSN, " ", "")
  
  failure_set <- failure_set %>% separate_rows(HD_PHYS_PSN)
  
  saveRDS(failure_set, file.path(output_path, "fail_detail.rda"))
  
  gc()

  ## FAILURE ( OLD FUNCTION ) #####

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