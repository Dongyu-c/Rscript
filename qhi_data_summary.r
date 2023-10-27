custom_data_prep = function(input_path, output_path){
  
  # print("##### getting iMHi date set ######")
  
  # library(ROracle)
  
  # host = "ttdss2.tep.thai.seagate.com"
  # port = 1521
  # svc = "ODS"
  
  # connect.string = paste(
    # "(DESCRIPTION=",
    # "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
    # "(CONNECT_DATA=(SERVICE_NAME=", svc, ")))", sep = "")
  
  # print("Connecting to database")
  
  # con <- ROracle::dbConnect(dbDriver("Oracle"),
                            # user     = "PARTAGING1",
                            # password = "Partag1ng",
                            # dbname = connect.string)
  
  # print("Querying iMHi data")
  
  # res <- dbSendQuery(con, "SELECT RECEIPT_DO_NUM, QPM_SUPPLIER_NAME, ALL_RESULT, VMI_RESULT, VPC_RESULT, GHOST_RESULT, RTPAD_RESULT, QPMTRIG_RESULT, QPMCOMP_RESULT, QPMTIME_RESULT FROM IMHI_SUMMARY_ROW_VIEW WHERE ERP_SUPPLIER LIKE '%NIDEC%'")
  
  # RECEIPT_PART_NUMBER
  
  # IHMI_data <- as.data.frame(dbFetch(res), stringsAsFactors = FALSE)
  
  # print("Querying UPS data")
  
  # res <- dbSendQuery(con, "SELECT * FROM B2B_UPS_INVENTORY WHERE QPM_SUPPLIER_NAME LIKE '%NIDEC%'")
  
  # UPS_data <- as.data.frame(dbFetch(res), stringsAsFactors = FALSE)
  
  # print("Disconnect from DB")
  
  # DBI::dbDisconnect(con)
  
  # saveRDS(IHMI_data, file.path(input_path, "IHMI_data.rda"))
  # saveRDS(UPS_data, file.path(input_path, "UPS_data.rda"))
  
  print("##### QHI Reading + Preping ######")
  
  supplier_dic = data.frame(MFG_SITE_DUNS = "71-889-8596", SUPPLIER_NAME = "NIDEC PHILIPPINES CORP (NILF) (MBA)", stringsAsFactors = F)
  
  QHI_set = read.csv(file.path(input_path, "QHI.csv"), stringsAsFactors = F)
  QHI_set = left_join(QHI_set, supplier_dic)
  names(QHI_set) = stringr::str_replace_all(names(QHI_set), "M_", "")
  QHI_set$FISCAL_WEEK <- paste(QHI_set$FISCAL_YEAR, ifelse(stringi::stri_length(QHI_set$WORK_WEEK) < 2, stringi::stri_join("0",QHI_set$WORK_WEEK, sep = ""), QHI_set$WORK_WEEK), sep = "-")
  QHI_set$LOT_NUM <- substring(QHI_set$LOT_NUM, 1, 4)
  QHI_set = QHI_set[,!(names(QHI_set) %in% c("ETL_SOURCE_NAME","ETL_LOAD_DATE","ETL_SOURCE_NAME","ETL_LOAD_DATE" ))]
  QHI_set$PART_NUM <- as.character(QHI_set$PART_NUM)
  
  print("##### QPM_DO Reading + Preping  ######")
  
  QPM_DO_set = read.csv(file.path(input_path, "QPM_DO.csv"), stringsAsFactors = F)
  QPM_DO_set = QPM_DO_set[,!(names(QPM_DO_set) %in% c("ETL_LOAD_DATE", "ETL_SOURCE_NAME"))]
  QPM_DO_set$PART_NUM <- as.character(QPM_DO_set$PART_NUM)
  
  print("##### Joining QHI with QPM_DO ######")
  
  QHI_set = left_join(QHI_set, QPM_DO_set, by = c("PART_NUM", "SUPPLIER_NAME", "LOT_NUM"))
  
  names(QHI_set)[which(names(QHI_set) == "DO_NUM")] <- "SOI_DO_NUM"
  
  print("##### Joining QHI with UPS_data ######")
  
  UPS_set = read.csv(file.path(input_path, "UPS_data.csv"), stringsAsFactors = F)
  
  QHI_set = left_join(QHI_set,UPS_set)
  
  print("##### Joining QHI with iMHi ######")
  
  iMHi_set = read.csv(file.path(input_path, "IHMI_data.csv"), stringsAsFactors = F)
  
  names(QHI_set)[which(names(QHI_set) == "RECEIPT_PART_NUMBER ")] <- "PART_NUM"
  
  QHI_set = left_join(QHI_set,iMHi_set)
  
  print("##### wrting QHI process data ######")
  
  saveRDS(QHI_set, file.path(output_path, "QHI_summary.rda"))
  
}