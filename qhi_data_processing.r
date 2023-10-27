custom_data_prep = function(input_path, output_path){
  
  print("##### QHI Reading + Preping ######")
  
  supplier_dic <- read.csv(file.path(input_path,"supplier DUNS or Code.csv"),stringsAsFactors = F)
  supplier_dic <- supplier_dic[,c(3,6)]
  names(supplier_dic) <- c("MFG_SITE_DUNS", "SUPPLIER_NAME")
  
  print(head(supplier_dic))
  
  # = data.frame(MFG_SITE_DUNS = "65-997-5924", SUPPLIER_NAME = "NMB MINEBEA THAI (MOTOR)", stringsAsFactors = F)
  
  QHI_set = read.csv(file.path(input_path, "QHI.csv"), stringsAsFactors = F)
  QHI_set = left_join(QHI_set, supplier_dic)
  # names(QHI_set) = stringr::str_replace_all(names(QHI_set), "M_", "")
  QHI_set$FISCAL_WEEK <- paste(QHI_set$FISCAL_YEAR, ifelse(stringi::stri_length(QHI_set$WORK_WEEK) < 2, stringi::stri_join("0",QHI_set$WORK_WEEK, sep = ""), QHI_set$WORK_WEEK), sep = "-")
  QHI_set$LOT_NUM <- substring(QHI_set$LOT_NUM, 1, 4)
  QHI_set = QHI_set[,!(names(QHI_set) %in% c("ETL_SOURCE_NAME","ETL_LOAD_DATE","ETL_SOURCE_NAME","ETL_LOAD_DATE" ))]
  QHI_set$PART_NUM <- as.character(QHI_set$PART_NUM)
  
  QHI_set$R
  
  if(any(QHI_set$REMARKS == 0)) {
    QHI_set[QHI_set$REMARKS == 0,]$REMARKS <- ""
  }
  
  QHI_set$PROCESS_FUNCTIONAL <- NA
  QHI_set$ODT_ORT <- NA
  QHI_set$CUSTOMER <- NA
  QHI_set$QPM <- NA
  QHI_set$PERFORMANCE_AT_SEAGATE <- NA
  
  print("##### QPM_DO Reading + Preping  ######")
  
  QPM_DO_set = read.csv(file.path(input_path, "QPM_DO.csv"), stringsAsFactors = F)
  QPM_DO_set = QPM_DO_set[,!(names(QPM_DO_set) %in% c("ETL_LOAD_DATE", "ETL_SOURCE_NAME"))]
  QPM_DO_set$PART_NUM <- as.character(QPM_DO_set$PART_NUM)
  
  A <- QHI_set[,c("PART_NUM", "SUPPLIER_NAME", "LOT_NUM")]
  B <- QPM_DO_set[,c("PART_NUM", "SUPPLIER_NAME", "LOT_NUM")]
  
  print("##### Joining QHI with QPM_DO ######")
  
  QHI_set = left_join(QHI_set, QPM_DO_set, by = c("PART_NUM", "SUPPLIER_NAME", "LOT_NUM"))
  
  names(QHI_set)[which(names(QHI_set) == "DO_NUM")] <- "SOI_DO_NUM"
  
  print("##### Joining QHI with UPS_data ######")
  
  # UPS_set = read.csv(file.path(input_path, "UPS_data.csv"), stringsAsFactors = F)
  UPS_set = readRDS(file.path(input_path, "UPS_data.rda"))
  
  QHI_set = left_join(QHI_set,UPS_set)
  
  print("##### Joining QHI with iMHi ######")
  
  # iMHi_set = read.csv(file.path(input_path, "IHMI_data.csv"), stringsAsFactors = F)
  iMHi_set = readRDS(file.path(input_path, "IHMI_data.rda"))
  
  QHI_set = left_join(QHI_set,iMHi_set)
  
  print("##### wrting QHI process data ######")
  
  saveRDS(QHI_set, file.path(output_path, "QHI_summary.rda"))
  
  gc()
  
  ##### UPSTREAM DATA #####
  
  QHI_RAW_DATA <- QHI_set
  
  Date_range <- as.numeric(stringr::str_replace_all(Sys.Date() - 3, '-', ''))
  
  QHI_RAW_DATA <- QHI_RAW_DATA[QHI_RAW_DATA$FISCAL_DATE_KEY > Date_range,]
  
  if (nrow(QHI_RAW_DATA)) {
	
	
  QHI_SHIP_INFO_DATA = QHI_RAW_DATA[,c("MFG_SITE_DUNS", "MODEL_NAME", "PART_NUM", "SUPPLIER_NAME", "FISCAL_WEEK", "GROUP_NUM",
                                         "DATA_SOURCE_TYPE", "LOT_NUM", "LOT_QTY", "SOI_DO_NUM", "REMARKS")]
  
  QHI_SHIP_INFO_DATA = QHI_SHIP_INFO_DATA %>%
    group_by(MFG_SITE_DUNS, MODEL_NAME, PART_NUM, SUPPLIER_NAME, FISCAL_WEEK, GROUP_NUM, DATA_SOURCE_TYPE, LOT_NUM, REMARKS) %>%
    dplyr::summarise(LOT_QTY = sum(LOT_QTY), SOI_DO_NUM = paste(unique(na.omit(SOI_DO_NUM)), collapse = ';'))
  
  QHI_SCORE_DATA = QHI_RAW_DATA[,c("MFG_SITE_DUNS", "MODEL_NAME", "PART_NUM", "SUPPLIER_NAME", "FISCAL_WEEK", "GROUP_NUM",
                                   "DATA_SOURCE_TYPE", "SUBTIER_SUPPLIER_1", "SUBTIER_SUPPLIER_2", "SUBTIER_SUPPLIER_3", "PQA_OBA", "QAN", "FIRST_PASS",
                                   "SECOND_PASS", "MAVERICK", "PEOPLE_ENVIRONMENT", "SQE_AUDIT", "CTQ_TREND",
                                   "LAT_LAR", "CLEANLINESS")]
  
  QHI_SCORE_DATA = unique(QHI_SCORE_DATA)
  
  QHI_SCORE_DATA = melt(QHI_SCORE_DATA, c("MFG_SITE_DUNS", "MODEL_NAME", "PART_NUM", "SUPPLIER_NAME", "FISCAL_WEEK", "GROUP_NUM", "DATA_SOURCE_TYPE"))
  
  QHI_SCORE_DATA = QHI_SCORE_DATA[QHI_SCORE_DATA$value > 0,]
  
  QHI_SCORE_SUM = QHI_SCORE_DATA %>%
    group_by(MFG_SITE_DUNS, MODEL_NAME, PART_NUM, SUPPLIER_NAME, FISCAL_WEEK, GROUP_NUM, DATA_SOURCE_TYPE) %>%
    dplyr::summarise("Performance" = sum(value)) %>% mutate(riskAssessment = case_when(Performance > 3 & Performance < 8 ~ "Level II",
                                                                                       Performance > 7 & Performance < 11 ~ "Level III",
                                                                                       Performance > 10 ~ "Reject",
                                                                                       TRUE ~ "Level I"))
  
  QHI_SCORE_SUM = QHI_SCORE_SUM[QHI_SCORE_SUM$riskAssessment %in% c("Level III", "Reject"),]
  
  QHI_SCORE_SUM = left_join(QHI_SCORE_SUM, QHI_SCORE_DATA)
  
  QHI_SCORE_SUM = left_join(QHI_SCORE_SUM, QHI_SHIP_INFO_DATA)
  
  ##### REST API #####
  
  if (nrow(QHI_SCORE_SUM) > 0) {
    
    library(httr)
    library(rjson)
    
    LOGINURL = "https://seagate.my.salesforce.com/services/oauth2/token"
    USERNAME = "qhi_ws@seagate.com"
    PASSWORD = "iS@fjsd_3s"
    CLIENTID = "3MVG9WQsPp5nH_EqJjHLmpM67Aeg1_C_.JQeQM_LFs1Auy1WIi4FeTC90nsXm2ke_Afg3MoEUWe0qOBLeH6rT"
    CLIENTSECRET = "7C0305E9D757B8F0361847E107762AFEB39A628AEFC113F4AB01D1A9379A8584"
    CALLBACK = 'https://seagate.my.salesforce.com/services/oauth2/callback'
    
    body = list(grant_type = 'password',
                client_id = CLIENTID,
                client_secret = CLIENTSECRET,
                redirect_uri = CALLBACK,
                username = USERNAME,
                password = PASSWORD,
                response_type = 'token')
    
    con_info <- POST(LOGINURL, body = body)
    
    cat(content(con_info, "text"), "\n")
    
    ACCESSTOKEN = content(con_info)$access_token
    
    ##### Upstream Testing
    
    #####  RestApi Key columns Key Parameter
    # Supplier
    # Commodity
    # Part Number
    # Affected Lots
    #####
    
    commodity="MBA"
    triggerCriteria="QHI"
    fieldSQE="427647"
    supplierCode="38279"
    detectionMethod="QHI Monitoring"
    causeIssue="To be advised"
    
    URL_LOADING = 'https://seagate.my.salesforce.com/services/apexrest/QPMTriggeringData?type=upstream&supplierCode='
    
    for (loop in 1:nrow(QHI_SCORE_SUM)) {
      
      data_set = QHI_SCORE_SUM[loop,]
      pn = data_set$PART_NUM
      lot = data_set$LOT_NUM
      product = data_set$MODEL_NAME
      FGIQty = data_set$LOT_QTY
      issueDesc = paste0(data_set$variable, " = ", data_set$value, ", Remark: ", data_set$REMARKS, ", DO_NUM: ", data_set$SOI_DO_NUM)
      riskAssessment = data_set$riskAssessment
      
      print("##### Key Columns #####")
      print(paste0("Supplier: " , data_set$SUPPLIER_NAME, " Part Number: ", data_set$PART_NUM, " Affected Lots: ", data_set$LOT_NUM))
      
      upstream_url = paste0(URL_LOADING, supplierCode,
                            "&commodity=",commodity,
                            "&triggerCriteria=",triggerCriteria,
                            "&fieldSQE=",fieldSQE,
                            "&pn=",pn,
                            "&lot=",lot,
                            "&product=",product,
                            "&issueDesc=",issueDesc,
                            "&FGIQty=",FGIQty,
                            "&detectionMethod=",detectionMethod,
                            "&causeIssue=",causeIssue,
                            "&riskAssessment=",riskAssessment,
                            collapse = "")
      
      upstream_url = stringr::str_replace_all(upstream_url, " ", "%20")
      
      upstream_upload_info <- POST(url = upstream_url, add_headers(Authorization = paste("Bearer", ACCESSTOKEN, sep = " ")))
      
      Status = content(upstream_upload_info)
      
      print(Status)
      
    }
  
  }
    
  }
  
  ##### DOWNSTREAM DATA #####
  
  ##### Prep data for Rest API #####
  
  library(dplyr)
  
  QHI_RAW_DATA <- QHI_set
  
  QHI_RAW_DATA <- QHI_RAW_DATA[QHI_RAW_DATA$FISCAL_DATE_KEY > 20211202,]
  
  Date_range <- as.numeric(stringr::str_replace_all(Sys.Date() - 3, '-', ''))
  
  QHI_RAW_DATA <- QHI_RAW_DATA[QHI_RAW_DATA$FISCAL_DATE_KEY > Date_range,]
  
  if (nrow(QHI_RAW_DATA)) {
    
    QHI_SHIP_INFO_DATA = QHI_RAW_DATA[,c("MFG_SITE_DUNS", "MODEL_NAME", "PART_NUM", "SUPPLIER_NAME", "FISCAL_WEEK", "GROUP_NUM", 
                                         "DATA_SOURCE_TYPE", "LOT_NUM", "LOT_QTY", "SOI_DO_NUM", "REMARKS")]
    
    QHI_SHIP_INFO_DATA = QHI_SHIP_INFO_DATA %>% 
      group_by(MFG_SITE_DUNS, MODEL_NAME, PART_NUM, SUPPLIER_NAME, FISCAL_WEEK, GROUP_NUM, DATA_SOURCE_TYPE, REMARKS) %>% 
      dplyr::summarise(LOT_QTY = sum(LOT_QTY), LOT_NUM = paste(unique(na.omit(LOT_NUM)), collapse = ';'), SOI_DO_NUM = paste(unique(na.omit(SOI_DO_NUM)), collapse = ';'))
    
    QHI_SCORE_DATA = QHI_RAW_DATA[,c("MFG_SITE_DUNS", "MODEL_NAME", "PART_NUM", "SUPPLIER_NAME", "FISCAL_WEEK", "GROUP_NUM", 
                                     "DATA_SOURCE_TYPE", "ALL_RESULT", "VMI_RESULT", "VPC_RESULT", "GHOST_RESULT", "RTPAD_RESULT", 
                                     "QPMTRIG_RESULT", "QPMCOMP_RESULT", "QPMTIME_RESULT")]
    
    result2score_dic <- data.frame(ALL_RESULT = c('R','D','C','B','A1','A2','A3','A4','P','N'),
                                   IMHI_SCORE = c(12,12,12,9,0,0,0,0,NA,NA), stringsAsFactors = F)
    
    QHI_SCORE_DATA <- left_join(QHI_SCORE_DATA, result2score_dic)
    
    group_com <- c("MFG_SITE_DUNS", "MODEL_NAME", "PART_NUM", "SUPPLIER_NAME", "FISCAL_WEEK", "GROUP_NUM", "DATA_SOURCE_TYPE", "IMHI_SCORE", "ALL_RESULT")
    
    QHI_SCORE_DATA <- melt(QHI_SCORE_DATA, id.vars = group_com)
    
    QHI_SCORE_DATA <- unique(QHI_SCORE_DATA[QHI_SCORE_DATA$IMHI_SCORE %in% 12 & QHI_SCORE_DATA$value %in% c("REJ", "FAIL"),])
    
    QHI_SCORE_DATA$variable <- stringr::str_remove_all(QHI_SCORE_DATA$variable, "_RESULT")
    
    QHI_SCORE_DATA$riskAssessment <- "Reject"
    QHI_SCORE_DATA$Severity <- "Critical"
    
    QHI_SCORE_SUM = left_join(QHI_SCORE_DATA, QHI_SHIP_INFO_DATA)
    
    QHI_SCORE_SUM = QHI_SCORE_SUM[order(QHI_SCORE_SUM$FISCAL_WEEK, decreasing = T),]
    
    ##### REST API #####
    
    library(httr)
    library(rjson)
    
    LOGINURL = "https://seagate.my.salesforce.com/services/oauth2/token"
    USERNAME = "qhi_ws@seagate.com"
    PASSWORD = "iS@fjsd_3s"
    CLIENTID = "3MVG9WQsPp5nH_EqJjHLmpM67Aeg1_C_.JQeQM_LFs1Auy1WIi4FeTC90nsXm2ke_Afg3MoEUWe0qOBLeH6rT"
    CLIENTSECRET = "7C0305E9D757B8F0361847E107762AFEB39A628AEFC113F4AB01D1A9379A8584"
    CALLBACK = 'https://seagate.my.salesforce.com/services/oauth2/callback'
    
    body = list(grant_type = 'password',
                client_id = CLIENTID,
                client_secret = CLIENTSECRET,
                redirect_uri = CALLBACK,
                username = USERNAME,
                password = PASSWORD,
                response_type = 'token')
    
    con_info <- POST(LOGINURL, body = body)
    
    cat(content(con_info, "text"), "\n")
    
    ACCESSTOKEN = content(con_info)$access_token
    
    #####  RestApi Key columns Key Parameter
    # Supplier
    # Commodity
    # Part Number
    # Affected Lots
    #####
    
    recordType="Downstream_triggering"
    commodity="MBA"
    triggerCriteria="QHI"
    fieldSQE="427647"
    supplierCode="38279"
    detectionMethod="QHI Monitoring"
    causeIssue="To be advised"
    
    # QHI_SCORE_SUM <- QHI_SCORE_SUM[1:5,]
    
    QHI_SCORE_SUM[QHI_SCORE_SUM$SOI_DO_NUM == '',]$SOI_DO_NUM <- 'NA'
    
    URL_LOADING = 'https://seagate.my.salesforce.com/services/apexrest/QPMTriggeringData?type=downstream&supplierCode='
    
    for (loop in 1:nrow(QHI_SCORE_SUM)) { 
      
      data_set = QHI_SCORE_SUM[loop,]
      pn = data_set$PART_NUM
      lot = data_set$LOT_NUM
      qty = data_set$LOT_QTY
      do = data_set$SOI_DO_NUM
      issuesDesc = paste0("Total Score ", data_set$IMHI_SCORE, " due to ", data_set$variable, " ", data_set$riskAssessment)
      link = 'www.osa.sing.seagate.com:8444/rsconnect/QHI_NMB'
      severity = data_set$Severity
      
      downstream_url = paste0(URL_LOADING, supplierCode,
                              "&commodity=",commodity,
                              "&triggerCriteria=",triggerCriteria,
                              "&qty=",qty,
                              "&fieldSQE=",fieldSQE,
                              "&do=",do,
                              "&pn=",pn,
                              "&link=",link,
                              "&lot=",lot,
                              "&issueDesc=",issuesDesc,
                              "&severity=",severity)
      
      # downstream_url = stringr::str_replace_all(downstream_url, " ", "%20")
      
      # downstream_upload_info <- POST(url = downstream_url, add_headers(Authorization = paste("Bearer", ACCESSTOKEN, sep = " ")))
      
      # print(downstream_url)
      
      # Status = content(downstream_upload_info)
      
      # print(Status)
      
    }
    
  }
  
}