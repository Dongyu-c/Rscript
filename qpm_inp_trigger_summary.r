custom_data_prep = function(input_path, output_path) {
  library(stringr)
  library(reshape2)
  library(plyr)
  library(dplyr)
  library(DT)
  library(reshape2)
  library(tidyr)
  library(data.table)
  library(ggplot2)
  
  options(digits=6) 
  
  QPM_folder <- file.path(ResultsPath, "E1072320", "RDA_MOD")
  QPM_sample_folder <- file.path(ResultsPath, "E1065238", "RDA_MOD")
  QPM_SPEC_folder <- file.path(ResultsPath, "E1082262", "QPM_transfer")
  QPM_DO_folder <- file.path(ResultsPath, "E1052810", "RDA_MOD")
  
  Current_week <- read.csv(file.path(ResultsPath, "E1061857", "FW_EVENT_DATE.csv"), stringsAsFactors = F) 
  Current_week <- Current_week[Current_week$DATE_KEY %in% stringr::str_replace_all(Sys.Date(), '-', ""),]$FISCAL_YEAR_WEEK
  Current_week <- unique(Current_week)
  print(paste0("Current_week: ", Current_week))
  
  # QPM_folder <- input_path
  # QPM_sample_folder <- input_path
  # QPM_SPEC_folder <- file.path(input_path)
  # QPM_DO_folder <- file.path(input_path)
  
  print(list.files(QPM_folder))
  
  CSV_PATH <- file.path(output_path, "CSV")
  PNG_PATH <- file.path(output_path, "PNG")
  CONFIG_PATH <- file.path(output_path, "CONFIG")
  
  if (!dir.exists(CSV_PATH)) {
    dir.create(file.path(CSV_PATH))
  }
  
  if (!dir.exists(PNG_PATH)) {
    dir.create(file.path(PNG_PATH))
  }
  
  if (!dir.exists(CONFIG_PATH)) {
    dir.create(file.path(CONFIG_PATH))
  }
  
  REMOVE_PARAM_LIST = c(
    "ACA_LINE_NUM",
    "ACTTR_CBORE_PSN",
    "ACTTR_HOLE_PSN",
    "ACTTR_POST_PSN",
    "ACTTR_THREAD_HOLE_PSN",
    "BASEPLATE_CAVITY_NUM",
    "BASEPLATE_LOT_NUM",
    "BASEPLATE_MOLD_NUM",
    "BASEPLATE_MOLD_SPECIAL_ID",
    "BREATHER_FILTER_HOLE_PSN",
    "BTM_CUST_THREAD_1_PSN",
    "BTM_CUST_THREAD_2_PSN",
    "BTM_CUST_THREAD_3_PSN",
    "BTM_CUST_THREAD_4_PSN",
    "CA_LOT_NUM",
    "a",
    "CNCTR_THREAD_1_PSN",
    "CNCTR_THREAD_2_PSN",
    "DAMPER_LOT_NUM",
    "DAMPER_REEL_NUM",
    "DATECODE",
    "DSP_HOLE_1_PSN",
    "DSP_HOLE_1_PSN_2ND",
    "DSP_HOLE_2_PSN",
    "DSP_HOLE_2_PSN_2ND",
    "DSP_PIN_1_PSN",
    "DSP_SLOT_PSN",
    "DSP_THREAD_1_COMPOSITE_PSN",
    "DSP_THREAD_2_COMPOSITE_PSN",
    "DSP_THREAD_3_COMPOSITE_PSN",
    "DSP_THREAD_HOLE_1_PSN",
    "DSP_THREAD_HOLE_2_PSN",
    "DSP_THREAD_HOLE_3_PSN",
    "DSP_THREAD_HOLE_4_PSN",
    "FINAL_TEST_FLAG",
    "FLEXURE_LOT_NUM",
    "GROUP_NUM",
    "IDCS_PIN_PSN",
    "LOADBEAM_LOT_NUM",
    "LOT_QTY",
    "LOT_SHIP_QTY",
    "MACHINE_NUM_OP2",
    "MFG_DATE_CODE",
    "MOLD_NUM",
    "MOLD_SPECIAL_ID",
    "MOTOR_CNCTN_SLOT_PSN_1",
    "MOTOR_CNCTN_SLOT_PSN_2",
    "MOTOR_HOLE_ID",
    "MOTOR_ID_ROUNDNESS",
    "MOTOR_ID_TRUE_PSN",
    "MOTOR_OD_TRUE_PSN",
    "MOTOR_STATOR_LEAD_IN_OD_PSN",
    "MOTOR_THRUST_YOKE_ID",
    "MOTOR_THRUST_YOKE_ID_PSN",
    "NCA_LOT_NUM",
    "PCBA_THREAD_1_PSN",
    "PCBA_THREAD_2_PSN",
    "PCBA_THREAD_3_PSN",
    "PCBA_THREAD_4_PSN",
    "PCBA_THREAD_5_PSN",
    "PCBA_THREAD_6_PSN",
    "PCBA_THREAD_7_PSN",
    "PCC_CNCTR_LOCATING_PIN_1_PSN",
    "PCC_CNCTR_LOCATING_PIN_2_PSN",
    "PCC_MOUNT_THREAD_1_PSN",
    "PCC_MOUNT_THREAD_2_PSN",
    "POST_MACHINE_NUM",
    "PRE_MACHINE_NUM",
    "PZT_NEGATIVE_LOT_NUM",
    "PZT_NEGATIVE_WAFER_NUM",
    "PZT_POSITIVE_LOT_NUM",
    "PZT_POSITIVE_WAFER_NUM",
    "RAMP_CBORE_HOLE_ID",
    "RAMP_CBORE_HOLE_PSN",
    "RAMP_CBORE_THREAD_PSN",
    "RAMP_STOPPER_PSN",
    "RAW_MATERIAL_LOT_NUMBER",
    "RECORD_SEQ",
    "SAMPLE_NUM",
    "SHAFT_HOLE_POSITION",
    "SHIP_DATE",
    "SIDE_CUST_THREAD_1_PSN",
    "SIDE_CUST_THREAD_2_PSN",
    "SIDE_CUST_THREAD_3_PSN",
    "SIDE_CUST_THREAD_4_PSN",
    "SIDE_CUST_THREAD_5_PSN",
    "SIDE_CUST_THREAD_6_PSN",
    "SUB_LOT_NUM",
    "TOP_COVER_LOCATING_POST_1_PSN",
    "TOP_COVER_LOCATING_POST_2_PSN",
    "TOP_COVER_THREAD_1_CMPST_PSN",
    "TOP_COVER_THREAD_1_PSN",
    "TOP_COVER_THREAD_2_CMPST_PSN",
    "TOP_COVER_THREAD_2_PSN",
    "TOP_COVER_THREAD_3_CMPST_PSN",
    "TOP_COVER_THREAD_3_PSN",
    "TOP_COVER_THREAD_4_CMPST_PSN",
    "TOP_COVER_THREAD_4_PSN",
    "TOP_COVER_THREAD_5_CMPST_PSN",
    "TOP_COVER_THREAD_5_PSN",
    "TOP_COVER_THREAD_6_CMPST_PSN",
    "TOP_COVER_THREAD_6_PSN",
    "VCM_BTM_THREAD_1_COMPOSITE_PSN",
    "VCM_BTM_THREAD_1_PSN",
    "VCM_BTM_THREAD_2_COMPOSITE_PSN",
    "VCM_BTM_THREAD_2_PSN",
    "VCM_BTM_THREAD_3_PSN",
    "VCM_LOCATE_POST_1_PSN",
    "VCM_LOCATE_POST_2_PSN",
    "VCM_LOCATING_HOLE_1_PSN",
    "VCM_LOCATING_HOLE_2_PSN",
    "VCM_LOCATING_HOLE_3_PSN",
    "VCM_TOP_THREAD_1_CMPST_PSN",
    "VCM_TOP_THREAD_2_CMPST_PSN",
    "VCM_TOP_THREAD_3_CMPST_PSN",
    "VCM_TOP_THREAD_PSN",
    "LOT_NUM",
    "RECORD_SEQ",
    "PART_NUM.1",
    "TRANS_SEQ", "HEAD_PSN", "READER_INDEX", "HEAD_SERIAL_NUM" 
  )
  
  remove_list <- c(
    "QPM_OOC_RANKING_NEW@E1033851.csv",
    "QPM_DASHBOARD_DIC.rda",
    "QPM_DASHBOARD_DO.rda",
    "QPM_DASHBOARD_CHART_KEY.rda",
    "QPM_DASHBOARD_SQE_CTQ.rda",
    "QPM_SPEC.rda",
    "QPM_TRIGGER.rda",
    "INP_QPM_DASHBOARD_CHART_KEY.rda",
    "INP_QPM_DASHBOARD_CASTEDBASEPLATE.rda",
    "INP_QPM_DASHBOARD_DIC.rda",
    "INP_QPM_DASHBOARD_SQE_CTQ.rda",
    "INP_QPM_DASHBOARD_TRIGGER.rda",
    "INP_QPM_DASHBOARD_SPEC.rda",
    "INP_QPM_DASHBOARD_DO.rda",
    "INP_QPM_DASHBOARD_EXT_HSA_RESONANCE_RAW.rda",
    "INP_QPM_DASHBOARD_EXT_HSA_RESONANCE_SUM.rda",
    "INP_QPM_DASHBOARD_TGA.rda"
  )
  
  QPM_INP_DATA = list.files(QPM_folder, pattern = "INP_QPM_DASHBOARD_")
  QPM_INP_SAMPLE_DATA = list.files(QPM_sample_folder, pattern = "INP_QPM_DASHBOARD_")
  QPM_INP_DATA = QPM_INP_DATA[!(QPM_INP_DATA %in% remove_list)]
  QPM_INP_SAMPLE_DATA = QPM_INP_SAMPLE_DATA[!(QPM_INP_SAMPLE_DATA %in% remove_list)]
  
  QPM_INP_DATA = c(QPM_INP_DATA, QPM_INP_SAMPLE_DATA)
  QPM_INP_DATA <- QPM_INP_DATA[!grepl(".rda.gz", QPM_INP_DATA)]
  
  DIC = readRDS(file.path(QPM_SPEC_folder, "QPM_DIC.rda"))
  DIC = unique(DIC)
  DIC[] = lapply(DIC, as.character)
  DIC$PART_NUM = stringr::str_remove_all(DIC$PART_NUM, c("\\bXXX|\\b "))
  DIC$ODT_PRODUCT_NAME = toupper(DIC$ODT_PRODUCT_NAME)
  DIC[DIC == ""] = "N/A"
  names(DIC)[names(DIC) %in% 'ODT_PRODUCT_NAME'] <- "PRODUCT_NAME"
  DIC <- DIC[, c("PRODUCT_NAME", "PART_NUM", "PRODUCT_TYPE")]
  
  trigger_con = readRDS(file.path(QPM_SPEC_folder, "QPM_TRIGGER.rda"))
  trigger_con = as.data.table(trigger_con)
  # trigger_con = trigger_con[,-5]
  trigger_con$TRIGGER_LEVEL = trigger_con$TRIGGER_LEVEL
  
  ##### SPEC
  
  SPEC = readRDS(file.path(QPM_SPEC_folder, "QPM_SPEC.rda"))
  
  SPEC = SPEC %>% mutate(QPM_PARAMETER  = ifelse(QPM_PARAMETER == "HD_VALUE", "GRAMLOAD_VALUE", QPM_PARAMETER))
  
  SPEC = SPEC %>% mutate(COMMODITY = ifelse(COMMODITY == "EXT_HSA_MARPOSS", "HSA", COMMODITY))
  SPEC = SPEC %>% mutate(COMMODITY = ifelse(COMMODITY == "MO", "MOTOR", COMMODITY))
  
  colnames(SPEC)[grep('QPM.trigger.level',colnames(SPEC))] <- "TRIGGER_LEVEL"
  
  SPEC$PRODUCT_NAME = as.character(toupper(SPEC$PRODUCT_NAME))
  SPEC$PRODUCT_NAME = stringr::str_remove_all(SPEC$PRODUCT_NAME, "-| ")
  
  SPEC$SUPPLIER_NAME = as.character(toupper(SPEC$SUPPLIER_NAME))
  SPEC$COMMODITY = as.character(toupper(SPEC$COMMODITY))
  SPEC$QPM_PARAMETER = as.character(toupper(SPEC$QPM_PARAMETER))
  
  treat_specs = function(x) {
    x = as.character(x)
    x[x %in% c("", "N/A", "-")] = ""
    as.numeric(x)
  }
  SPEC$LSL = treat_specs(SPEC$LSL)
  SPEC$USL = treat_specs(SPEC$USL)
  
  spec = SPEC
  spec <- as.data.table(spec)
  
  trigger_index = SPEC[, c(
    "COMMODITY",
    "PART_NUM",
    "SUPPLIER_NAME",
    "QPM_PARAMETER",
    "Key_CTQ",
    "TRIGGER_LEVEL"
  )]
  
  trigger_index <- as.data.table(trigger_index)
  
  ########################
  
  SPEC_ref = SPEC[, c(
    "PRODUCT_NAME",
    "COMMODITY",
    "PART_NUM",
    "SUPPLIER_NAME",
    "QPM_PARAMETER",
    "Target",
    "USL",
    "LSL"
  )]
  
  SPEC = SPEC[, c("COMMODITY",
                  "PART_NUM",
                  "SUPPLIER_NAME",
                  "QPM_PARAMETER",
                  "Target",
                  "USL",
                  "LSL")]
  
  SPEC$COMMODITY = toupper(SPEC$COMMODITY)
  
  SPEC_2 = melt(
    SPEC,
    id = c("COMMODITY", "PART_NUM", "SUPPLIER_NAME", "QPM_PARAMETER"),
    measure.vars = c("Target", "USL", "LSL"),
    variable.name = "LINE_TYPE",
    value.name = "VALUE"
  )
  
  SPEC_2$SUPPLIER_NAME[SPEC_2$SUPPLIER_NAME == "NIDEC PHILIPPINES FACTORY (NCFL)"] = "NIDEC PHILIPPINES CORP (NILF) (MBA)"
  
  #####
  
  # CHART_KEY <-
  # readRDS(file.path(QPM_SPEC_folder, 'QPM_CHART_KEY.rda'))
  
  # CHART_KEY <-
  # CHART_KEY[, c(
  # "PRODUCT_TYPE",
  # "SUPPLIER_NAME",
  # "PRODUCT_PART_NUM",
  # "PARM_NAME",
  # "CONTROL_CHART_KEY"
  # )]
  
  # names(CHART_KEY) <-
  # c("COMMODITY",
  # "SUPPLIER_NAME",
  # "PART_NUM",
  # "Parameter",
  # "CHART_KEY")
  
  # CHART_KEY$COMMODITY[CHART_KEY$COMMODITY == "MO"] <- "MOTOR"
  # CHART_KEY$COMMODITY <- NULL
  # CHART_KEY[] <- lapply(CHART_KEY, as.character)
  
  #####
  
  do_temp <- readRDS(file.path(QPM_DO_folder, "QPM_DASHBOARD_DO.rda"))
  
  do_temp <-
    unique(do_temp[, c("PART_NUM",
                       "SUPPLIER_NAME",
                       "GROUP_NUM",
                       "DO_NUM",
                       "SHIP_ACTUAL_DATE")])
  
  do_qty <- readRDS(file.path(QPM_SPEC_folder, "SHP_INFO.rda"))
  
  do_qty <-
    unique(do_qty[, c("RECEIPT_DO_NUM",
                      "RECEIPT_PART_NUMBER",
                      "RECEIPT_QUANTITY")])
  
  names(do_qty)[1:3] <- c("DO_NUM", "PART_NUM", "DO_QTY")
  
  #####
  
  cpk_table <- function(raw, com) {
    dic_product = as.data.table(DIC)
    
    X = raw
    X = as.data.table(X)
    X = X[, which(unlist(lapply(X, function(x)
      ! all(is.na(
        x
      ))))), with = F]
    
    if (com %in% c("ACA", "ACT", "COIL", "HOOKUP", "EXT_HSA_GRAMLOAD", "EXT_HSA_MARPOSS", "EXT_HSA_RESONANCE_SUM", "HGA")) {
      xname = "GRP_YEAR_WEEK"
      
      X = X[X$GRP_YEAR_WEEK < Current_week,]
      
    } else {
      xname = "GROUP_NUM"
    }
    
    # sup_link_temp = unique(X[,c("SUPPLIER_NAME","SUPPLIER_KEY")])
    
    X$TRIGGER_LEVEL <- as.character(X$TRIGGER_LEVEL)
    col = sort(names(X)[sapply(X, is.numeric)])
    col = col[!(col %in% REMOVE_PARAM_LIST)]
    
    X[,(xname):= lapply(.SD, as.character), .SDcols = xname]
    
    # SPEC_unique = SPEC_2 %>% group_by(PART_NUM, SUPPLIER_NAME,QPM_PARAMETER, LINE_TYPE) %>% slice(1)
    
    SPEC_unique = SPEC_2 %>% filter(COMMODITY %in% com) %>% group_by(PART_NUM, SUPPLIER_NAME,QPM_PARAMETER, LINE_TYPE) %>% slice(1)
    SPEC = tryCatch({ dcast(SPEC_unique,SUPPLIER_NAME+PART_NUM+QPM_PARAMETER ~ LINE_TYPE,value.var = "VALUE") }, error = function(e){NULL})
    
    COMMODITY <- com
    PART_NUM <- sort(unique(data_set$PART_NUM))
    SUPPLIER_NAME <- sort(unique(data_set$SUPPLIER_NAME))
    
    print("CPK Prep done")
    
    if (!(commodity %in% c("EXT_HSA_GRAMLOAD", "EXT_HSA_MARPOSS", "HGA"))) {
      
      if (commodity %in% c("DSP")) {
        grouping_con = c("DATA_SOURCE_TYPE", "CAVITY_NUM", "MAT_TYPE")
      } else if (commodity %in% c("RAMP")) {
        grouping_con = c("DATA_SOURCE_TYPE", "MOLD_NUM", "CAVITY_NUM")
      } else if (commodity %in% c("MOTOR")) {
        grouping_con = c("DATA_SOURCE_TYPE")
      } else if (commodity %in% c("SUBSTRATE")) {
        grouping_con = c("DATA_SOURCE_TYPE", "SITE_LOC")
      } else {
        grouping_con = c("DATA_SOURCE_TYPE")
      }
      
      X = X[, c("SUPPLIER_NAME", "PART_NUM", "GROUP_DATETIME", grouping_con, "ETL_LOAD_DATE", "GRP_YEAR_WEEK", xname, col), with = F]
      
      setkeyv(X, c( "SUPPLIER_NAME", "PART_NUM", "GROUP_DATETIME", grouping_con, "ETL_LOAD_DATE", "GRP_YEAR_WEEK", xname))
      
      X = suppressWarnings(reshape2::melt( X, measure.vars = col , value.name = "VALUE", variable.name = "QPM_PARAMETER"))
      X = X[!is.na(X$VALUE), ]
      
      print("Spec_checker: Start")
      
      Spec_checker = unique(X[,c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")])
      
      Spec_checker$COMMODITY = commodity
      
      print("before joining")
      
      Spec_checker = left_join(Spec_checker, sup_linker, by = "SUPPLIER_NAME")
      
      Spec_checker_temp = read.csv(file = file.path(ResultsPath, "E1092865", "SPEC_CHECKER_INP.csv"), stringsAsFactors = F)
      
      Spec_checker = rbind(Spec_checker, Spec_checker_temp)
      
      write.csv(Spec_checker, file = file.path(ResultsPath, "E1092865", "SPEC_CHECKER_INP.csv"), row.names = FALSE)
      
      gc(Spec_checker)
      
      print("Spec_checker: End")
      
      X = X[, NORMALIZE_VALUE := (VALUE - mean(VALUE, na.rm = T)) / sd(VALUE, na.rm = T) ,
            by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con)]
      
      dt = X[, list(
        N = .N,
        Mean = mean(VALUE, na.rm = T),
        Std = sd(VALUE , na.rm = T),
        Median = as.double(median(VALUE , na.rm = T)),
        # ETL_LOAD_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)],
        # YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)],
        ETL_LOAD_DATE = max(ETL_LOAD_DATE),
        YEAR_WEEK = max(GRP_YEAR_WEEK),
        GROUP_DATETIME = max(GROUP_DATETIME)
      ),
      by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, xname)]
      
      group_order = c( "SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, xname, "GROUP_DATETIME", "ETL_LOAD_DATE")
      dt = setorderv(dt, group_order, c(rep(1, length(group_order) - 3), -1, -1, -1))
      
      dt = dt[!(is.na(dt$Mean)), ]
      dt[, RANK := 1:.N , by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con)]
      
      grouping_col = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con)
      
      rank_tracker <- dt %>% group_by(.dots =  grouping_col) %>% dplyr::summarise(MAX_RANK = max(RANK))
      
      dt[, Last_7_Mean := mean(Mean[RANK[1:7]], na.rm = T), by =  c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con)]
      
      cpk_dt = dt[, c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", xname, grouping_con, "Mean", "Std", "RANK", "GROUP_DATETIME" ), with = F]
      rmlist = names(dt)
      
      raw_dt = dt[RANK == 1]
      gc(dt)
      spec = SPEC
      spec = as.data.table(spec)
      
      if (nrow(spec) > 0) {
        spec = spec[, .(SUPPLIER_NAME,
                        PART_NUM,
                        QPM_PARAMETER,
                        Target,
                        USL,
                        LSL)]
        y = SPEC_2
        raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
        spec$PART_NUM = as.character(spec$PART_NUM)
        
        collist = unique(names(raw_dt)[!names(raw_dt) %in% c("RANK")])
        raw_dt = raw_dt[, collist, with = F]
        raw_dt = merge(
          raw_dt,
          spec,
          by.x = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
          by.y = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
          all.x = T
        )
        ## cal  cpk
        raw_dt$Target = as.numeric(raw_dt$Target)
        raw_dt$USL = as.numeric(raw_dt$USL)
        raw_dt$LSL = as.numeric(raw_dt$LSL)
        
        cpks = numeric(nrow(raw_dt))
        cpks_Slope = numeric(nrow(raw_dt))
        for (i in 1:nrow(raw_dt)) {
          Usl = raw_dt$USL[i]
          Lsl = raw_dt$LSL[i]
          Mean = raw_dt$Mean[i]
          Mean_7_point = raw_dt$Last_7_Mean[i]
          Std = raw_dt$Std[i]
          cpks[i] =
            if (!is.na(Usl) & !is.na(Lsl)) {
              min((Usl - Mean) / (3 * Std), (Mean - Lsl) / (3 * Std))
            } else if (is.na(Usl) & !is.na(Lsl)) {
              (Mean - Lsl) / (3 * Std)
            } else if (!is.na(Usl) & is.na(Lsl)) {
              (Usl - Mean) / (3 * Std)
            } else{
              NA
            }
        }
        cpks = round(cpks, 3)
        raw_dt$Cpk = cpks
        
        ## create CPK moving DT
        cpk_dt = merge(
          cpk_dt,
          spec,
          by.x = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
          by.y = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
          all.x = T
        )
        cpk_dt$Target = as.numeric(cpk_dt$Target)
        cpk_dt$USL = as.numeric(cpk_dt$USL)
        cpk_dt$LSL = as.numeric(cpk_dt$LSL)
        cpk_moving = numeric(nrow(cpk_dt))
        
        for (i in 1:nrow(cpk_dt)) {
          Usl = cpk_dt$USL[i]
          Lsl = cpk_dt$LSL[i]
          Mean = cpk_dt$Mean[i]
          Std = cpk_dt$Std[i]
          
          cpk_moving[i] =
            if (!is.na(Usl) & !is.na(Lsl)) {
              min((Usl - Mean) / (3 * Std), (Mean - Lsl) / (3 * Std))
            } else if (is.na(Usl) & !is.na(Lsl)) {
              (Mean - Lsl) / (3 * Std)
            } else if (!is.na(Usl) & is.na(Lsl)) {
              (Usl - Mean) / (3 * Std)
            } else{
              NA
            }
        }
        cpk_moving = round(cpk_moving, 3)
        cpk_dt$Cpk = cpk_moving
        
      }
      
    } else if (commodity == "EXT_HSA_GRAMLOAD") {
      
      X = X[,c("SUPPLIER_NAME","PART_NUM","EVENT_DATE","HEAD_PSN","RUN_TYPE","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname,col), with = F]
      setkeyv(X, c("SUPPLIER_NAME","PART_NUM","EVENT_DATE","HEAD_PSN","RUN_TYPE","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname))
      X = suppressWarnings(melt(X , measure.vars = col, value.name="VALUE", variable.name="QPM_PARAMETER" ))
      X = X[!is.na(X$VALUE),]
      X = X[QPM_PARAMETER == "GRAMLOAD_VALUE",]
      
      columns = c('SUPPLIER_NAME','PART_NUM','QPM_PARAMETER','HEAD_PSN','RUN_TYPE',xname)
      
      sample_checking = X %>% group_by(.dots = columns) %>% dplyr::summarise(N = n())
      
      if (sum(sample_checking$N < 15) > 0) {
        
        low_sample = sample_checking %>% 
          group_by(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, HEAD_PSN, RUN_TYPE) %>% 
          dplyr::summarise(Record = n(), Less_than_15 = sum(N < 15), 
                           pop_percent = ((Record - Less_than_15) / Record * 100),
                           remove = 100 > pop_percent & pop_percent >= 90)
        
        low_sample = low_sample %>% 
          group_by(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, HEAD_PSN, RUN_TYPE) %>% 
          dplyr::summarise(remove = 100 > pop_percent & pop_percent >= 90)
        
        print(low_sample[low_sample$remove == T,])
        
        sample_checking <- left_join(sample_checking,low_sample)
        
        # sample_checking <- sample_checking %>% slice(which(N < 30 & remove == T))
        sample_checking <- sample_checking %>% slice(which(N < 15))
        
        X = anti_join(X, sample_checking)
        
        X = as.data.table(X)
        
      }
      
      X = X[,NORMALIZE_VALUE := (VALUE- mean(VALUE, na.rm = T) )/sd(VALUE, na.rm = T) ,
            by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","HEAD_PSN","RUN_TYPE" )]
      
      dt = X[,list(N = .N, Mean = mean(VALUE, na.rm = T), Std = sd(VALUE ,na.rm = T), Median = median(VALUE ,na.rm = T)
                   ,ETL_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)] , YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)]
                   ,EVENT_DATE =max(EVENT_DATE)), 
             by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","HEAD_PSN","RUN_TYPE",xname)]
      
      dt = setorderv(dt,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","HEAD_PSN","RUN_TYPE", "EVENT_DATE"),c(1,1,1,1,1,-1) )
      dt = dt[!(is.na(dt$Mean)),]
      dt[,RANK := 1:.N ,by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,HEAD_PSN)]
      
      rank_tracker <-  dt %>% group_by(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,HEAD_PSN) %>% dplyr::summarise(MAX_RANK = max(RANK))
      
      dt[,Last_7_Mean := mean(Mean[RANK[1:7]],na.rm = T), 
         by =  .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,HEAD_PSN)]
      
      cpk_dt = dt[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",xname,"HEAD_PSN","RUN_TYPE","Mean","Std","RANK","EVENT_DATE"), with = F]
      rmlist = names(dt)
      
      raw_dt = dt[RANK ==1]
      gc(dt)
      spec = SPEC
      spec = as.data.table(spec)
      
      if(nrow(spec) > 0) {
        
        spec = spec[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,Target,USL,LSL)]
        y = SPEC_2
        raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
        spec$PART_NUM = as.character(spec$PART_NUM)
        
        collist = unique(names(raw_dt)[!names(raw_dt) %in% c("RANK")])
        raw_dt = raw_dt[,collist, with =F]
        raw_dt = merge(raw_dt,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                       by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
        ## cal cpk
        raw_dt$Target = as.numeric(raw_dt$Target)
        raw_dt$USL = as.numeric(raw_dt$USL)
        raw_dt$LSL = as.numeric(raw_dt$LSL)
        
        cpks = numeric(nrow(raw_dt))
        cpks_slope = numeric(nrow(raw_dt))
        
        for(i in 1:nrow(raw_dt)){
          Usl = raw_dt$USL[i]
          Lsl = raw_dt$LSL[i]
          Mean = raw_dt$Mean[i]
          Mean_7_point = raw_dt$Last_7_Mean[i]
          Std = raw_dt$Std[i]
          cpks[i] = 
            if(!is.na(Usl) & !is.na(Lsl)){
              min((Usl - Mean)/(3*Std), (Mean - Lsl)/(3*Std))
            } else if(is.na(Usl) & !is.na(Lsl)){
              (Mean - Lsl)/(3*Std)
            } else if(!is.na(Usl) & is.na(Lsl)){
              (Usl - Mean)/(3*Std)
            } else{
              NA
            }
        }
        cpks = round(cpks,3)
        raw_dt$Cpk = cpks
        
        ## create CPK moving DT
        cpk_dt = merge(cpk_dt,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                       by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
        cpk_dt$Target = as.numeric(cpk_dt$Target)
        cpk_dt$USL = as.numeric(cpk_dt$USL)
        cpk_dt$LSL = as.numeric(cpk_dt$LSL)
        cpk_moving = numeric(nrow(cpk_dt))
        
        for(i in 1:nrow(cpk_dt)){
          Usl = cpk_dt$USL[i]
          Lsl = cpk_dt$LSL[i]
          Mean = cpk_dt$Mean[i]
          Std = cpk_dt$Std[i]
          
          cpk_moving[i] = 
            if(!is.na(Usl) & !is.na(Lsl)){
              min(  (Usl - Mean)/(3*Std), (Mean - Lsl)/(3*Std)    )
            } else if(is.na(Usl) & !is.na(Lsl)){
              (Mean - Lsl)/(3*Std)
            } else if(!is.na(Usl) & is.na(Lsl)){
              (Usl - Mean)/(3*Std)
            } else{
              NA
            }
        }
        cpk_moving = round(cpk_moving,3)
        cpk_dt$Cpk = cpk_moving
        
      } else {
        cpk_dt$Cpk <- NA
      } 
      
    } else if (commodity == "EXT_HSA_MARPOSS") {
      
      X = X[,c("SUPPLIER_NAME","PART_NUM","EVENT_DATE","HEAD_PSN","RUN_TYPE","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname,col), with = F]
      setkeyv(X, c("SUPPLIER_NAME","PART_NUM","EVENT_DATE","HEAD_PSN","RUN_TYPE","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname))
      X = suppressWarnings(melt(X , measure.vars = col , value.name="VALUE", variable.name="QPM_PARAMETER"))
      X = X[!is.na(X$VALUE),]
      
      columns = c('SUPPLIER_NAME','PART_NUM','QPM_PARAMETER','HEAD_PSN','RUN_TYPE',xname)
      
      sample_checking = X %>% group_by(.dots = columns) %>% dplyr::summarise(N = n())
      
      if (sum(sample_checking$N < 30) > 0) {
        low_sample = sample_checking %>% 
          group_by(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, HEAD_PSN, RUN_TYPE) %>% 
          dplyr::summarise(Record = n(), Less_than_30 = sum(N < 30), 
                           pop_percent = ((Record - Less_than_30) / Record * 100),
                           remove = 100 > pop_percent & pop_percent >= 90)
        
        low_sample = low_sample %>% 
          group_by(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, HEAD_PSN, RUN_TYPE) %>% 
          dplyr::summarise(remove = 100 > pop_percent & pop_percent >= 90)
        
        print(low_sample[low_sample$remove == T,])
        
        sample_checking <- left_join(sample_checking,low_sample)
        
        sample_checking <- sample_checking %>% slice(which(N < 30))
        
        X = anti_join(X, sample_checking)
        
        X = as.data.table(X)
        
      }
      
      X = X[,NORMALIZE_VALUE := (VALUE- mean(VALUE, na.rm = T) )/sd(VALUE, na.rm = T) ,
            by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","HEAD_PSN","RUN_TYPE")]
      
      dt = X[,list(N = .N, Mean = mean(VALUE, na.rm = T), Std = sd(VALUE ,na.rm = T), Median = median(VALUE ,na.rm = T)
                   ,ETL_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)] , YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)]
                   ,EVENT_DATE =max(EVENT_DATE)), 
             by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","HEAD_PSN","RUN_TYPE",xname)]
      
      dt = setorderv(dt,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","HEAD_PSN","RUN_TYPE","EVENT_DATE"),c(1,1,1,1,1,-1))
      dt = dt[!(is.na(dt$Mean)),]
      dt[,RANK := 1:.N ,by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,HEAD_PSN)]
      
      rank_tracker <-  dt %>% group_by(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,HEAD_PSN) %>% dplyr::summarise(MAX_RANK = max(RANK))
      
      dt[,Last_7_Mean := mean(Mean[RANK[1:7]],na.rm = T), 
         by =  .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,HEAD_PSN)]
      
      cpk_dt = dt[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",xname,"HEAD_PSN","RUN_TYPE","Mean","Std","RANK","EVENT_DATE"), with = F]
      rmlist = names(dt)
      
      raw_dt = dt[RANK ==1]
      gc(dt)
      spec = SPEC
      spec = as.data.table(spec)
      
      if(nrow(spec) > 0) {
        
        spec = spec[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,Target,USL,LSL)]
        y = SPEC_2
        raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
        spec$PART_NUM = as.character(spec$PART_NUM)
        
        collist = unique(names(raw_dt)[!names(raw_dt) %in% c("RANK")])
        raw_dt = raw_dt[,collist, with =F]
        raw_dt = merge(raw_dt,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                       by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
        ## cal  cpk
        raw_dt$Target = as.numeric(raw_dt$Target)
        raw_dt$USL = as.numeric(raw_dt$USL)
        raw_dt$LSL = as.numeric(raw_dt$LSL)
        
        cpks = numeric(nrow(raw_dt))
        cpks_slope = numeric(nrow(raw_dt))
        for(i in 1:nrow(raw_dt)){
          Usl = raw_dt$USL[i]
          Lsl = raw_dt$LSL[i]
          Mean = raw_dt$Mean[i]
          Mean_7_point = raw_dt$Last_7_Mean[i]
          Std = raw_dt$Std[i]
          cpks[i] = 
            if(!is.na(Usl) & !is.na(Lsl)){
              min(  (Usl - Mean)/(3*Std), (Mean - Lsl)/(3*Std)    )
            } else if(is.na(Usl) & !is.na(Lsl)){
              (Mean - Lsl)/(3*Std)
            } else if(!is.na(Usl) & is.na(Lsl)){
              (Usl - Mean)/(3*Std)
            } else{
              NA
            }
        }
        cpks = round(cpks,3)
        raw_dt$Cpk = cpks
        
        ## create CPK moving DT
        cpk_dt = merge(cpk_dt,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                       by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
        cpk_dt$Target = as.numeric(cpk_dt$Target)
        cpk_dt$USL = as.numeric(cpk_dt$USL)
        cpk_dt$LSL = as.numeric(cpk_dt$LSL)
        cpk_moving = numeric(nrow(cpk_dt))
        
        for(i in 1:nrow(cpk_dt)){
          Usl = cpk_dt$USL[i]
          Lsl = cpk_dt$LSL[i]
          Mean = cpk_dt$Mean[i]
          Std = cpk_dt$Std[i]
          
          cpk_moving[i] = 
            if(!is.na(Usl) & !is.na(Lsl)){
              min(  (Usl - Mean)/(3*Std), (Mean - Lsl)/(3*Std)    )
            } else if(is.na(Usl) & !is.na(Lsl)){
              (Mean - Lsl)/(3*Std)
            } else if(!is.na(Usl) & is.na(Lsl)){
              (Usl - Mean)/(3*Std)
            } else{
              NA
            }
        }
        cpk_moving = round(cpk_moving,3)
        cpk_dt$Cpk = cpk_moving
        
      } else {
        
        cpk_dt$Cpk <- NA
        
      } 
      
    } else if (commodity == "HGA") {
      X = X[,c("SUPPLIER_NAME","PART_NUM","GRP_ACTUAL_DATE","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname,col), with = F]
      setkeyv(X, c("SUPPLIER_NAME","PART_NUM","GRP_ACTUAL_DATE","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname))
      X = melt(X , measure.vars = col , value.name="VALUE", variable.name="QPM_PARAMETER" )
      X = X[!is.na(X$VALUE),]
      
      X = X[,NORMALIZE_VALUE := (VALUE- mean(VALUE, na.rm = T) )/sd(VALUE, na.rm = T) ,
            by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER")]
      
      dt = X[,list(N = .N, Mean = mean(VALUE, na.rm = T), Std = sd(VALUE ,na.rm = T), Median = as.double(median(VALUE ,na.rm = T)),
                   ETL_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)] , YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)],
                   GRP_ACTUAL_DATE =max(GRP_ACTUAL_DATE)), 
             by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",xname)]
      
      dt = setorderv(dt,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER", "GRP_ACTUAL_DATE"),c(1,1,1,-1) )
      dt = dt[!(is.na(dt$Mean)),]
      dt[,RANK := 1:.N ,by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER)]
      
      rank_tracker <- dt %>% group_by(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER) %>% dplyr::summarise(MAX_RANK = max(RANK))
      
      dt[,Last_7_Mean := mean(Mean[RANK[1:7]],na.rm = T), 
         by =  .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER)]
      
      cpk_dt = dt[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",xname,"Mean","Std","RANK","GRP_ACTUAL_DATE"), with = F]
      rmlist = names(dt)
      
      raw_dt = dt[RANK ==1]
      gc(dt)
      spec = SPEC
      spec = as.data.table(spec)
      
      if(nrow(spec) > 0) {
        spec = spec[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,Target,USL,LSL)]
        y = SPEC_2
        raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
        spec$PART_NUM = as.character(spec$PART_NUM)
        
        collist = unique(names(raw_dt)[!names(raw_dt) %in% c("RANK")])
        raw_dt = raw_dt[,collist, with =F]
        raw_dt = merge(raw_dt,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                       by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
        ## cal  cpk
        raw_dt$Target = as.numeric(raw_dt$Target)
        raw_dt$USL = as.numeric(raw_dt$USL)
        raw_dt$LSL = as.numeric(raw_dt$LSL)
        
        cpks = numeric(nrow(raw_dt))
        cpks_slope = numeric(nrow(raw_dt))
        for(i in 1:nrow(raw_dt)){
          Usl = raw_dt$USL[i]
          Lsl = raw_dt$LSL[i]
          Mean = raw_dt$Mean[i]
          Mean_7_point = raw_dt$Last_7_Mean[i]
          Std = raw_dt$Std[i]
          cpks[i] = 
            if(!is.na(Usl) & !is.na(Lsl)){
              min(  (Usl - Mean)/(3*Std), (Mean - Lsl)/(3*Std)    )
            } else if(is.na(Usl) & !is.na(Lsl)){
              (Mean - Lsl)/(3*Std)
            } else if(!is.na(Usl) & is.na(Lsl)){
              (Usl - Mean)/(3*Std)
            } else{
              NA
            }
        }
        cpks = round(cpks,3)
        raw_dt$Cpk = cpks
        
        ## create CPK moving DT
        cpk_dt = merge(cpk_dt,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                       by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
        cpk_dt$Target = as.numeric(cpk_dt$Target)
        cpk_dt$USL = as.numeric(cpk_dt$USL)
        cpk_dt$LSL = as.numeric(cpk_dt$LSL)
        cpk_moving = numeric(nrow(cpk_dt))
        
        for(i in 1:nrow(cpk_dt)){
          Usl = cpk_dt$USL[i]
          Lsl = cpk_dt$LSL[i]
          Mean = cpk_dt$Mean[i]
          Std = cpk_dt$Std[i]
          
          cpk_moving[i] = 
            if(!is.na(Usl) & !is.na(Lsl)){
              min(  (Usl - Mean)/(3*Std), (Mean - Lsl)/(3*Std)    )
            } else if(is.na(Usl) & !is.na(Lsl)){
              (Mean - Lsl)/(3*Std)
            } else if(!is.na(Usl) & is.na(Lsl)){
              (Usl - Mean)/(3*Std)
            } else{
              NA
            }
        }
        
        cpk_moving = round(cpk_moving,3)
        cpk_dt$Cpk = cpk_moving
        
      } else {
        
        cpk_dt$Cpk <- NA
        
      } 
      
    } 
    
    if (commodity == "HGA") {
      
      if ("GRP_ACTUAL_DATE" %in% names(raw_dt)) {
        
        raw_dt = raw_dt[,-11]
        names(raw_dt)[grep('QPM_PARAMETER',names(raw_dt))] <-"Parameter"
        
        H = raw_dt
        
      }
      
    } else {
      
      names(raw_dt)[grep('QPM_PARAMETER',names(raw_dt))] <-"Parameter"
      raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
      
      H = raw_dt
      
    }
    
    # names(raw_dt)[grep('QPM_PARAMETER', names(raw_dt))] <- "Parameter"
    # raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
    
    # if ("CHART_KEY" %in% names(raw_dt)) raw_dt[, "CHART_KEY"] = NULL
    # H = merge(raw_dt, CHART_KEY, by = c("SUPPLIER_NAME", "PART_NUM", "Parameter"), all.x = TRUE)
    
    gc()
    
    if (!(commodity %in% c("EXT_HSA_GRAMLOAD", "EXT_HSA_MARPOSS", "HGA"))) {
      
      if (commodity %in% c("RAMP")) {
        grouping_con = c("DATA_SOURCE_TYPE", "MOLD_NUM", "CAVITY_NUM")
      } else if (commodity %in% c("DSP")) {
        grouping_con = c("DATA_SOURCE_TYPE", "CAVITY_NUM", "MAT_TYPE")
      } else if (commodity %in% c("MOTOR")) {
        grouping_con = "DATA_SOURCE_TYPE"
      } else if (commodity %in% c("SUBSTRATE")) {
        grouping_con = c("DATA_SOURCE_TYPE", "SITE_LOC")
      } else { 
        grouping_con = "DATA_SOURCE_TYPE"
      }
      
      if(c("Cpk") %in% names(H)) {
        raw_cpk_Slope = cpk_dt[(Cpk!=Inf & Cpk!=-Inf)]
        
        if(nrow(raw_cpk_Slope)>0) {
          raw_cpk_Slope[, Cnt := .N , by =c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con)]
          raw_cpk_Slope = raw_cpk_Slope[!is.na(Cpk) & RANK %in% (1:7) & Cnt >=7]
          
          if (nrow(raw_cpk_Slope) > 0) {
            final_cpk_Slope = raw_cpk_Slope[,list(Cpk_Slope=round(coef(lm(Cpk~RANK))[2],3)),
                                            by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con)]
            final_cpk_Slope$Cpk_Slope = final_cpk_Slope$Cpk_Slope * (-1)
          }else {
            raw_cpk_Slope = cpk_dt[(Cpk!=Inf & Cpk!=-Inf)]
            final_cpk_Slope = unique(raw_cpk_Slope[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con), with= F])
            final_cpk_Slope$Cpk_Slope <- NA
          }
          
        } else {
          final_cpk_Slope = unique(cpk_dt[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con), with= F])
          final_cpk_Slope$Cpk_Slope <- NA
        }
        
      }
      
      if (exists("raw_cpk_Slope")) {
        gc(raw_cpk_Slope)
      }
      
      ## cal  Slope
      raw_Slope = X[, list(
        Normal_mean = mean(NORMALIZE_VALUE , na.rm = T),
        Mean = mean(VALUE , na.rm = T),
        Std = sd(VALUE, na.rm = T),
        Max = max(VALUE , na.rm = T),
        Min = min(VALUE, na.rm = T),
        GROUP_DATETIME = max(GROUP_DATETIME)
      ),
      by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, xname)]
      
      raw_Slope = raw_Slope[!(is.na(raw_Slope$Normal_mean)),]
      
      setkeyv(raw_Slope, c( "SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, xname))
      
      group_order = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, "GROUP_DATETIME")
      raw_Slope = setorderv(raw_Slope, c(group_order, xname), c(rep(1, length(group_order) - 1),-1,-1))
      raw_Slope = raw_Slope[, RANK := 1:.N,by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con)]
      
      raw_Slope = raw_Slope[, relabel := ifelse(RANK == 1 , "last", "prevoius"), by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con)]
      raw_Slope = raw_Slope[, N := max(RANK) , by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con)]
      raw_Slope = raw_Slope[N >= 7 & RANK %in% c(1:7)]
      
      if (nrow(raw_Slope) > 0) {
        final_Slope = raw_Slope[, list(Slope = coef(lm(Normal_mean ~ RANK))[2]),
                                by = c("SUPPLIER_NAME",
                                       "PART_NUM",
                                       "QPM_PARAMETER",
                                       grouping_con)]
        gc(raw_Slope)
        final_Slope$Slope = abs(round(final_Slope$Slope, 3))
      } else {
        final_Slope = unique(raw_Slope[,.(SUPPLIER_NAME, PART_NUM, DATA_SOURCE_TYPE, QPM_PARAMETER)])
        gc(raw_Slope)
        final_Slope$Slope <- NA
      }
      
      if (test_type == 1) {
        ############ T.test cal p.value :: Robust welth P
        setkeyv(X,c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, xname))
        X =  X[, c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, xname, "GROUP_DATETIME", "VALUE"), with = F]
        
        group_order = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, "GROUP_DATETIME" )
        X  = setorderv(X, c(group_order, xname), c(rep(1, length( group_order ) - 1), -1, -1))
        Index_A = unique(X[, c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, xname), with = F])
        Index_A = Index_A[, RANK := 1:.N, by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con)]
        
        Index_A = Index_A[, relabel := ifelse(RANK == 1, "last", "prevoius"),
                          by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con)]
        
        X = merge(X, Index_A, by = c( "SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, xname ), all.x = T )
        gc(Index_A)
        
        final_t_test = X[, list(p = tryCatch({
          round(t.test(VALUE ~ relabel)$p.value, 3)
        } , error  = function(e) {
          404
        })),
        by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con)]
        
        final_t_test =  final_t_test[!(p %in% c(404, NA)), ]
        
        final_Diff_between = X[, list(Mean = mean(VALUE , na.rm = T)) ,
                               by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, "relabel")]
        
        Std_between = X[, list(Std_all = sd(VALUE[relabel == "prevoius"], na.rm = T)),
                        by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con)]
        
        final_Diff_between = merge(final_Diff_between, Std_between, by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con))
        
        # final_Diff_between = final_Diff_between[complete.cases(final_Diff_between), ]
        
        # dcast_con = paste0("SUPPLIER_NAME+PART_NUM+QPM_PARAMETER+DATA_SOURCE_TYPE+",paste0(grouping_con, collapse = "+"),"+Std_all ~ relabel")
        # final_Diff_between = reshape2::dcast(final_Diff_between, dcast_con,  value.var = "Mean")
        # final_Diff_between$Diff <- round(abs(final_Diff_between$last - final_Diff_between$prevoius) / final_Diff_between$Std_all, 3)
        
        # final_Diff_between = final_Diff_between[, c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con, "Diff")]
        # final_Diff_between = final_Diff_between[!(is.na(final_Diff_between$Diff)), ]
        
        # if (nrow(final_Slope) > 0) {
        # final_t_test = merge(final_t_test, final_Diff_between, by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con), all = T)
        # final_t_test = merge(final_t_test, final_cpk_Slope, by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con), all = T)
        # final_result = merge(final_t_test, final_Slope, by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con), all = T)
        # } else {
        # final_result = merge(final_t_test, final_Diff_between, by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", grouping_con), all = T)
        # final_result$Slope <- NA
        # }
        
        if (nrow(final_Diff_between[complete.cases(final_Diff_between),]) > 0) {
          final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
          
          dcast_con = paste0("SUPPLIER_NAME+PART_NUM+QPM_PARAMETER+DATA_SOURCE_TYPE+",paste0(grouping_con, collapse = "+"),"+Std_all ~ relabel")
          final_Diff_between = reshape2::dcast(final_Diff_between, dcast_con,  value.var = "Mean")
          final_Diff_between$Diff <- round(abs(final_Diff_between$last - final_Diff_between$prevoius)/final_Diff_between$Std_all,3)
          final_Diff_between = final_Diff_between[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con,"Diff")]
          final_Diff_between = final_Diff_between[!(is.na(final_Diff_between$Diff)),]
          
        } else {
          
          
          final_Diff_between = final_Diff_between[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con), with = FALSE]
          
          final_Diff_between$Diff <- NA
          
        }
        
        if(nrow(final_Slope)>0) {
          final_t_test = merge(final_t_test,final_Diff_between,
                               by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con), all = T)
          
          if(!exists("final_cpk_Slope")) {
            final_t_test$Cpk_Slope <- NA
          } else {
            
            if (is.list(final_cpk_Slope)) {
              final_t_test$Cpk_Slope <- NA
            } else {
              final_t_test = merge(final_t_test, final_cpk_Slope,
                                   by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con), all = T)
            }
            
          }
          
          final_result = merge(final_t_test,final_Slope,
                               by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con), all = T)
          
        } else {
          final_result = merge(final_t_test,final_Diff_between, 
                               by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",grouping_con), all = T)
          final_result$Slope <- NA
        }
        
      } else {
        #P.value by anova
        final_anova = X[!(is.na(X$VALUE)), ]
        final_anova[, RANK := 1:.N, by = c("SUPPLIER_NAME",
                                           "PART_NUM",
                                           "QPM_PARAMETER",
                                           grouping_con)]
        final_anova = final_anova[, list(p = tryCatch({
          round(summary(aov(VALUE ~ RANK))[[1]][["Pr(>F)"]], 3)
        } ,
        error  = function(e) {
          404
        })) ,
        by = c("SUPPLIER_NAME",
               "PART_NUM",
               "QPM_PARAMETER",
               grouping_con)]
        final_anova = final_anova[!(p %in% c(404, NA)), ]
        
        #Diff by anova
        final_Diff_between = X[, list(Mean = mean(VALUE , na.rm = T) ,
                                      Std = sd(VALUE, na.rm = T)) ,
                               by = c("SUPPLIER_NAME",
                                      "PART_NUM",
                                      "QPM_PARAMETER",
                                      grouping_con,
                                      xname)]
        final_Diff_between = final_Diff_between[, list(
          Max = max(Mean, na.rm = T),
          Min = min(Mean, na.rm =  T),
          std_mean = mean(Std, na.rm = T)
        )
        , by = c("SUPPLIER_NAME",
                 "PART_NUM",
                 "QPM_PARAMETER",
                 grouping_con)]
        final_Diff_between = final_Diff_between[complete.cases(final_Diff_between), ]
        final_Diff_between$Diff <-
          round((final_Diff_between$Max - final_Diff_between$Min) / final_Diff_between$std_mean,
                3
          )
        final_Diff_between = final_Diff_between[, c("SUPPLIER_NAME",
                                                    "PART_NUM",
                                                    "QPM_PARAMETER",
                                                    grouping_con,
                                                    "Diff")]
        
        #merge result
        if (nrow(final_Slope) > 0) {
          final_anova = merge(
            final_anova,
            final_Diff_between,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              grouping_con
            ),
            all = T
          )
          
          final_anova = merge(
            final_anova,
            final_cpk_Slope,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              grouping_con
            ),
            all = T
          )
          
          final_result = merge(
            final_Slope,
            final_anova,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              grouping_con
            ),
            all = T
          )
        } else {
          final_result = merge(
            final_anova,
            final_Diff_between,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              grouping_con
            ),
            all = T
          )
          final_result$Slope <- NA
        }
      }
      
    } else if (commodity == "EXT_HSA_GRAMLOAD") {
      ### cal CPk_slope from CPK moving DT
      
      #### WORKING SET ####
      
      # cut Cannot calcualte from CPK
      raw_cpk_slope = cpk_dt[(Cpk != Inf & Cpk != -Inf)]
      raw_cpk_slope[, Cnt := .N , by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", "HEAD_PSN", "RUN_TYPE")]
      raw_cpk_slope = raw_cpk_slope[!is.na(Cpk) & RANK %in% (1:7) & Cnt >= 7]
      
      if (nrow(raw_cpk_slope) > 0) {
        final_cpk_slope = raw_cpk_slope[, list(Cpk_Slope = round(coef(lm(Cpk ~ RANK))[2], 3)),
                                        by = c("SUPPLIER_NAME",
                                               "PART_NUM",
                                               "QPM_PARAMETER",
                                               "HEAD_PSN",
                                               "RUN_TYPE")]
        final_cpk_slope$Cpk_Slope = final_cpk_slope$Cpk_Slope * (-1)
      } else {
        final_cpk_slope = unique(raw_cpk_slope[, c("SUPPLIER_NAME",
                                                   "PART_NUM",
                                                   "QPM_PARAMETER",
                                                   "HEAD_PSN",
                                                   "RUN_TYPE")])
        final_cpk_slope$Cpk_Slope <- NA
      }
      
      if (exists("raw_cpk_Slope")) {
        gc(raw_cpk_Slope)
      }
      
      ## cal  Slope
      raw_slope = X[, list(
        Normal_mean = mean(NORMALIZE_VALUE , na.rm = T),
        Mean = mean(VALUE , na.rm = T),
        Std = sd(VALUE, na.rm = T),
        Max = max(VALUE , na.rm = T),
        Min = min(VALUE, na.rm = T),
        EVENT_DATE = max(EVENT_DATE)
      )  ,
      by = c("SUPPLIER_NAME",
             "PART_NUM",
             "QPM_PARAMETER",
             "HEAD_PSN",
             "RUN_TYPE",
             xname)]
      
      raw_slope = raw_slope[!(is.na(raw_slope$Normal_mean)), ]
      setkeyv(
        raw_slope,
        c(
          "SUPPLIER_NAME",
          "PART_NUM",
          "QPM_PARAMETER",
          "HEAD_PSN",
          "RUN_TYPE",
          xname
        )
      )
      raw_slope  = setorderv(
        raw_slope,
        c(
          "SUPPLIER_NAME",
          "PART_NUM",
          "QPM_PARAMETER",
          "HEAD_PSN",
          "RUN_TYPE",
          "EVENT_DATE"
        ),
        c(1, 1, 1, 1, 1, -1)
      )
      raw_slope = raw_slope[, RANK := 1:.N, by = list(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, HEAD_PSN)]
      raw_slope = raw_slope[, relabel := ifelse(RANK == 1 , "last", "prevoius"), by = c("SUPPLIER_NAME",
                                                                                        "PART_NUM",
                                                                                        "QPM_PARAMETER",
                                                                                        "HEAD_PSN",
                                                                                        "RUN_TYPE")]
      raw_slope = raw_slope[, N := max(RANK) , by = c("SUPPLIER_NAME",
                                                      "PART_NUM",
                                                      "QPM_PARAMETER",
                                                      "HEAD_PSN",
                                                      "RUN_TYPE")]
      raw_slope = raw_slope[N >= 7 & RANK %in% c(1:7)]
      
      if (nrow(raw_slope) > 0) {
        final_slope = raw_slope[, list(Slope = coef(lm(Normal_mean ~ RANK))[2]),
                                by = c("SUPPLIER_NAME",
                                       "PART_NUM",
                                       "QPM_PARAMETER",
                                       "HEAD_PSN",
                                       "RUN_TYPE")]
        gc(raw_slope)
        final_slope$Slope = abs(round(final_slope$Slope, 3))
      }
      else {
        final_slope = unique(raw_slope[, .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER)])
        gc(raw_slope)
        final_slope$Slope <- NA
      }
      
      if (test_type == 1) {
        ############ T.test cal p.value :: Robust welth P
        setkeyv(
          X,
          c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "QPM_PARAMETER",
            "HEAD_PSN",
            "RUN_TYPE",
            xname
          )
        )
        X =  X[, c(
          "SUPPLIER_NAME",
          "PART_NUM",
          "QPM_PARAMETER",
          "HEAD_PSN",
          "RUN_TYPE",
          xname,
          "EVENT_DATE",
          "VALUE"
        ), with = F]
        
        X  = setorderv(
          X,
          c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "QPM_PARAMETER",
            "HEAD_PSN",
            "RUN_TYPE",
            "EVENT_DATE"
          ),
          c(1, 1, 1, 1, 1, -1)
        )
        Index_A = unique(X[, c("SUPPLIER_NAME",
                               "PART_NUM",
                               "QPM_PARAMETER",
                               "HEAD_PSN",
                               "RUN_TYPE",
                               xname),  with = F])
        Index_A = Index_A[, RANK := 1:.N, by = list(SUPPLIER_NAME,
                                                    PART_NUM,
                                                    QPM_PARAMETER,
                                                    HEAD_PSN,
                                                    RUN_TYPE)]
        
        Index_A = Index_A[, relabel := ifelse(RANK == 1 , "last", "prevoius"),
                          by = c("SUPPLIER_NAME",
                                 "PART_NUM",
                                 "QPM_PARAMETER",
                                 "HEAD_PSN",
                                 "RUN_TYPE")]
        
        X = merge(
          X,
          Index_A,
          by = c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "QPM_PARAMETER",
            "HEAD_PSN",
            "RUN_TYPE",
            xname
          ),
          all.x = T
        )
        gc(Index_A)
        final_t_test = X[, list(p =    tryCatch({
          round(t.test(VALUE ~ relabel)$p.value, 3)
        } , error  = function(e) {
          404
        })) ,
        by = .(SUPPLIER_NAME,
               PART_NUM,
               QPM_PARAMETER,
               HEAD_PSN,
               RUN_TYPE)]
        
        final_t_test =  final_t_test[!(p %in% c(404, NA)), ]
        
        final_diff_between = X[, list(Mean = mean(VALUE , na.rm = T)) ,
                               by = .(SUPPLIER_NAME,
                                      PART_NUM,
                                      QPM_PARAMETER,
                                      HEAD_PSN,
                                      RUN_TYPE,
                                      relabel)]
        
        Std_between = X[, list(Std_all = sd(VALUE[relabel == "prevoius"], na.rm = T)),
                        by = .(SUPPLIER_NAME,
                               PART_NUM,
                               QPM_PARAMETER,
                               HEAD_PSN,
                               RUN_TYPE)]
        final_diff_between = merge(
          final_diff_between,
          Std_between ,
          by = c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "QPM_PARAMETER",
            "HEAD_PSN",
            "RUN_TYPE"
          )
        )
        
        final_diff_between = final_diff_between[complete.cases(final_diff_between), ]
        final_diff_between =  dcast(
          final_diff_between,
          SUPPLIER_NAME + PART_NUM + QPM_PARAMETER + HEAD_PSN + RUN_TYPE + Std_all ~ relabel ,
          value.var = "Mean"
        )
        final_diff_between$Diff <-
          round(
            abs(
              final_diff_between$last - final_diff_between$prevoius
            ) / final_diff_between$Std_all,
            3
          )
        final_diff_between = final_diff_between[, c(
          "SUPPLIER_NAME",
          "PART_NUM",
          "QPM_PARAMETER",
          "HEAD_PSN",
          "RUN_TYPE",
          "Diff"
        )]
        final_diff_between = final_diff_between[!(is.na(final_diff_between$Diff)), ]
        
        if (nrow(final_cpk_slope) > 0) {
          final_t_test = merge(
            final_t_test,
            final_diff_between,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          final_t_test = merge(
            final_t_test,
            final_cpk_slope,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          final_result = merge(
            final_t_test,
            final_slope,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
        } else {
          final_result = merge(
            final_t_test,
            final_diff_between,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          final_result$Slope <- NA
        }
        # final_result <<- final_result
        
      } else {
        #P.value by anova
        final_anova = X[!(is.na(X$VALUE)), ]
        final_anova[, RANK := 1:.N, by = list(SUPPLIER_NAME,
                                              PART_NUM,
                                              QPM_PARAMETER,
                                              HEAD_PSN,
                                              RUN_TYPE)]
        final_anova = final_anova[, list(p = tryCatch({
          round(summary(aov(VALUE ~ RANK))[[1]][["Pr(>F)"]], 3)
        } ,
        error  = function(e) {
          404
        })) ,
        by = .(SUPPLIER_NAME,
               PART_NUM,
               QPM_PARAMETER,
               HEAD_PSN,
               RUN_TYPE)]
        final_anova = final_anova[!(p %in% c(404, NA)), ]
        
        #diff by anova
        final_diff_between = X[, list(Mean = mean(VALUE , na.rm = T) ,
                                      Std = sd(VALUE, na.rm = T)) ,
                               by = c("SUPPLIER_NAME",
                                      "PART_NUM",
                                      "QPM_PARAMETER",
                                      "HEAD_PSN",
                                      "RUN_TYPE",
                                      xname)]
        final_diff_between = final_diff_between[, list(
          Max = max(Mean, na.rm = T),
          Min = min(Mean, na.rm =  T),
          std_mean = mean(Std, na.rm = T)
        )
        , by = .(SUPPLIER_NAME,
                 PART_NUM,
                 QPM_PARAMETER,
                 HEAD_PSN,
                 RUN_TYPE)]
        final_diff_between = final_diff_between[complete.cases(final_diff_between), ]
        final_diff_between$Diff <-
          round((final_diff_between$Max - final_diff_between$Min) / final_diff_between$std_mean,
                3
          )
        final_diff_between = final_diff_between[, .(SUPPLIER_NAME,
                                                    PART_NUM,
                                                    QPM_PARAMETER,
                                                    HEAD_PSN,
                                                    RUN_TYPE,
                                                    Diff)]
        
        #merge result
        if (nrow(final_slope) > 0) {
          final_anova = merge(
            final_anova,
            final_diff_between,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          
          final_anova = merge(
            final_anova,
            final_cpk_slope,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          
          final_result = merge(
            final_slope,
            final_anova,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
        } else {
          final_result = merge(
            final_anova,
            final_diff_between,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          final_result$Slope <- NA
        }
        
      }
      
    } else if (commodity == "EXT_HSA_MARPOSS") {
      ### cal CPk_slope from CPK moving DT
      
      # cut Cannot calcualte from CPK
      raw_cpk_slope = cpk_dt[(Cpk != Inf & Cpk != -Inf)]
      raw_cpk_slope[, Cnt := .N , by = c("SUPPLIER_NAME",
                                         "PART_NUM",
                                         "QPM_PARAMETER",
                                         "HEAD_PSN",
                                         "RUN_TYPE")]
      raw_cpk_slope = raw_cpk_slope[!is.na(Cpk) &
                                      RANK %in% (1:7) & Cnt >= 7]
      
      if (nrow(raw_cpk_slope) > 0) {
        final_cpk_slope = raw_cpk_slope[, list(Cpk_Slope = round(coef(lm(Cpk ~ RANK))[2], 3)),
                                        by = c("SUPPLIER_NAME",
                                               "PART_NUM",
                                               "QPM_PARAMETER",
                                               "HEAD_PSN",
                                               "RUN_TYPE")]
        final_cpk_slope$Cpk_Slope = final_cpk_slope$Cpk_Slope * (-1)
      } else {
        final_cpk_slope = unique(raw_cpk_slope[, c("SUPPLIER_NAME",
                                                   "PART_NUM",
                                                   "QPM_PARAMETER",
                                                   "HEAD_PSN",
                                                   "RUN_TYPE")])
        final_cpk_slope$Cpk_Slope <- NA
      }
      
      if (exists("raw_cpk_Slope")) {
        gc(raw_cpk_Slope)
      }
      
      ## cal  Slope
      raw_slope = X[, list(
        Normal_mean = mean(NORMALIZE_VALUE , na.rm = T),
        Mean = mean(VALUE , na.rm = T),
        Std = sd(VALUE, na.rm = T),
        Max = max(VALUE , na.rm = T),
        Min = min(VALUE, na.rm = T),
        EVENT_DATE = max(EVENT_DATE)
      )  ,
      by = c("SUPPLIER_NAME",
             "PART_NUM",
             "QPM_PARAMETER",
             "HEAD_PSN",
             "RUN_TYPE",
             xname)]
      
      raw_slope = raw_slope[!(is.na(raw_slope$Normal_mean)), ]
      setkeyv(
        raw_slope,
        c(
          "SUPPLIER_NAME",
          "PART_NUM",
          "QPM_PARAMETER",
          "HEAD_PSN",
          "RUN_TYPE",
          xname
        )
      )
      raw_slope  = setorderv(
        raw_slope,
        c(
          "SUPPLIER_NAME",
          "PART_NUM",
          "QPM_PARAMETER",
          "HEAD_PSN",
          "RUN_TYPE",
          "EVENT_DATE"
        ),
        c(1, 1, 1, 1, 1, -1)
      )
      raw_slope = raw_slope[, RANK := 1:.N, by = list(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, HEAD_PSN)]
      raw_slope = raw_slope[, relabel := ifelse(RANK == 1 , "last", "prevoius"), by = c("SUPPLIER_NAME",
                                                                                        "PART_NUM",
                                                                                        "QPM_PARAMETER",
                                                                                        "HEAD_PSN",
                                                                                        "RUN_TYPE")]
      raw_slope = raw_slope[, N := max(RANK) , by = c("SUPPLIER_NAME",
                                                      "PART_NUM",
                                                      "QPM_PARAMETER",
                                                      "HEAD_PSN",
                                                      "RUN_TYPE")]
      raw_slope = raw_slope[N >= 7 & RANK %in% c(1:7)]
      
      if (nrow(raw_slope) > 0) {
        final_slope = raw_slope[, list(Slope = coef(lm(Normal_mean ~ RANK))[2]),
                                by = c("SUPPLIER_NAME",
                                       "PART_NUM",
                                       "QPM_PARAMETER",
                                       "HEAD_PSN",
                                       "RUN_TYPE")]
        gc(raw_slope)
        final_slope$Slope = abs(round(final_slope$Slope, 3))
      }
      else {
        final_slope = unique(raw_slope[, .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER)])
        gc(raw_slope)
        final_slope$Slope <- NA
      }
      
      if (test_type == 1) {
        ############ T.test cal p.value :: Robust welth P
        setkeyv(
          X,
          c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "QPM_PARAMETER",
            "HEAD_PSN",
            "RUN_TYPE",
            xname
          )
        )
        X =  X[, c(
          "SUPPLIER_NAME",
          "PART_NUM",
          "QPM_PARAMETER",
          "HEAD_PSN",
          "RUN_TYPE",
          xname,
          "EVENT_DATE",
          "VALUE"
        ), with = F]
        
        X  = setorderv(
          X,
          c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "QPM_PARAMETER",
            "HEAD_PSN",
            "RUN_TYPE",
            "EVENT_DATE"
          ),
          c(1, 1, 1, 1, 1, -1)
        )
        Index_A = unique(X[, c("SUPPLIER_NAME",
                               "PART_NUM",
                               "QPM_PARAMETER",
                               "HEAD_PSN",
                               "RUN_TYPE",
                               xname),  with = F])
        Index_A = Index_A[, RANK := 1:.N, by = list(SUPPLIER_NAME,
                                                    PART_NUM,
                                                    QPM_PARAMETER,
                                                    HEAD_PSN,
                                                    RUN_TYPE)]
        
        Index_A = Index_A[, relabel := ifelse(RANK == 1 , "last", "prevoius"),
                          by = c("SUPPLIER_NAME",
                                 "PART_NUM",
                                 "QPM_PARAMETER",
                                 "HEAD_PSN",
                                 "RUN_TYPE")]
        
        X = merge(
          X,
          Index_A,
          by = c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "QPM_PARAMETER",
            "HEAD_PSN",
            "RUN_TYPE",
            xname
          ),
          all.x = T
        )
        gc(Index_A)
        final_t_test = X[, list(p =    tryCatch({
          round(t.test(VALUE ~ relabel)$p.value, 3)
        } , error  = function(e) {
          404
        })) ,
        by = .(SUPPLIER_NAME,
               PART_NUM,
               QPM_PARAMETER,
               HEAD_PSN,
               RUN_TYPE)]
        
        final_t_test =  final_t_test[!(p %in% c(404, NA)), ]
        
        final_diff_between = X[, list(Mean = mean(VALUE , na.rm = T)) ,
                               by = .(SUPPLIER_NAME,
                                      PART_NUM,
                                      QPM_PARAMETER,
                                      HEAD_PSN,
                                      RUN_TYPE,
                                      relabel)]
        
        Std_between = X[, list(Std_all = sd(VALUE[relabel == "prevoius"], na.rm = T)),
                        by = .(SUPPLIER_NAME,
                               PART_NUM,
                               QPM_PARAMETER,
                               HEAD_PSN,
                               RUN_TYPE)]
        final_diff_between = merge(
          final_diff_between,
          Std_between ,
          by = c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "QPM_PARAMETER",
            "HEAD_PSN",
            "RUN_TYPE"
          )
        )
        
        
        final_diff_between = final_diff_between[complete.cases(final_diff_between), ]
        final_diff_between =  dcast(
          final_diff_between,
          SUPPLIER_NAME + PART_NUM + QPM_PARAMETER + HEAD_PSN + RUN_TYPE + Std_all ~ relabel ,
          value.var = "Mean"
        )
        final_diff_between$Diff <-
          round(
            abs(
              final_diff_between$last - final_diff_between$prevoius
            ) / final_diff_between$Std_all,
            3
          )
        final_diff_between = final_diff_between[, c(
          "SUPPLIER_NAME",
          "PART_NUM",
          "QPM_PARAMETER",
          "HEAD_PSN",
          "RUN_TYPE",
          "Diff"
        )]
        final_diff_between = final_diff_between[!(is.na(final_diff_between$Diff)), ]
        
        if (nrow(final_cpk_slope) > 0) {
          final_t_test = merge(
            final_t_test,
            final_diff_between,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          final_t_test = merge(
            final_t_test,
            final_cpk_slope,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          final_result = merge(
            final_t_test,
            final_slope,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
        } else {
          final_result = merge(
            final_t_test,
            final_diff_between,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          final_result$Slope <- NA
        }
        # final_result <<- final_result
        
      } else {
        #P.value by anova
        final_anova = X[!(is.na(X$VALUE)), ]
        final_anova[, RANK := 1:.N, by = list(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, HEAD_PSN)]
        final_anova = final_anova[, list(p = tryCatch({
          round(summary(aov(VALUE ~ RANK))[[1]][["Pr(>F)"]], 3)
        } ,
        error  = function(e) {
          404
        })) ,
        by = .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, HEAD_PSN)]
        final_anova = final_anova[!(p %in% c(404, NA)), ]
        
        #diff by anova
        final_diff_between = X[, list(Mean = mean(VALUE , na.rm = T) ,
                                      Std = sd(VALUE, na.rm = T)) ,
                               by = c("SUPPLIER_NAME",
                                      "PART_NUM",
                                      "QPM_PARAMETER",
                                      "HEAD_PSN",
                                      "RUN_TYPE",
                                      xname)]
        final_diff_between = final_diff_between[, list(
          Max = max(Mean, na.rm = T),
          Min = min(Mean, na.rm =  T),
          std_mean = mean(Std, na.rm = T)
        )
        , by = .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, HEAD_PSN)]
        final_diff_between = final_diff_between[complete.cases(final_diff_between), ]
        final_diff_between$Diff <-
          round((final_diff_between$Max - final_diff_between$Min) / final_diff_between$std_mean,
                3
          )
        final_diff_between = final_diff_between[, .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, HEAD_PSN, Diff)]
        
        #merge result
        if (nrow(final_slope) > 0) {
          final_anova = merge(
            final_anova,
            final_diff_between,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          
          final_anova = merge(
            final_anova,
            final_cpk_slope,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          
          final_result = merge(
            final_slope,
            final_anova,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
        } else {
          final_result = merge(
            final_anova,
            final_diff_between,
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "HEAD_PSN",
              "RUN_TYPE"
            ),
            all = T
          )
          final_result$Slope <- NA
        }
        
      }
      
    } else if (commodity == "HGA") {
      ### cal CPk_slope from CPK moving DT
      
      # cut Cannot calcualte from CPK
      raw_cpk_slope = cpk_dt[(Cpk != Inf & Cpk != -Inf)]
      raw_cpk_slope[, Cnt := .N , by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")]
      raw_cpk_slope = raw_cpk_slope[!is.na(Cpk) &
                                      RANK %in% (1:7) & Cnt >= 7]
      
      if (nrow(raw_cpk_slope) > 0) {
        final_cpk_slope = raw_cpk_slope[, list(Cpk_Slope = round(coef(lm(Cpk ~ RANK))[2], 3)),
                                        by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")]
        final_cpk_slope$Cpk_Slope = final_cpk_slope$Cpk_Slope * (-1)
      } else {
        final_cpk_slope = unique(raw_cpk_slope[, c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")])
        final_cpk_slope$Cpk_Slope <- NA
      }
      
      if (exists("raw_cpk_Slope")) {
        gc(raw_cpk_Slope)
      }
      
      ## cal  Slope
      raw_slope = X[, list(
        Normal_mean = mean(NORMALIZE_VALUE , na.rm = T),
        Mean = mean(VALUE , na.rm = T),
        Std = sd(VALUE, na.rm = T),
        Max = max(VALUE , na.rm = T),
        Min = min(VALUE, na.rm = T),
        GRP_ACTUAL_DATE = max(GRP_ACTUAL_DATE)
      ),
      by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", xname)]
      
      raw_slope = raw_slope[!(is.na(raw_slope$Normal_mean)), ]
      setkeyv(raw_slope,
              c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", xname))
      raw_slope  = setorderv(
        raw_slope,
        c(
          "SUPPLIER_NAME",
          "PART_NUM",
          "QPM_PARAMETER",
          "GRP_ACTUAL_DATE"
        ),
        c(1, 1, 1, -1)
      )
      raw_slope = raw_slope[, RANK := 1:.N, by = list(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER)]
      raw_slope = raw_slope[, relabel := ifelse(RANK == 1 , "last", "prevoius"), by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")]
      raw_slope = raw_slope[, N := max(RANK) , by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")]
      raw_slope = raw_slope[N >= 7 & RANK %in% c(1:7)]
      
      if (nrow(raw_slope) > 0) {
        final_slope = raw_slope[, list(Slope = coef(lm(Normal_mean ~ RANK))[2]),
                                by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")]
        gc(raw_slope)
        final_slope$Slope = abs(round(final_slope$Slope, 3))
      }
      else {
        final_slope = unique(raw_slope[, .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER)])
        gc(raw_slope)
        final_slope$Slope <- NA
      }
      
      if (test_type == 1) {
        ############ T.test cal p.value :: Robust welth P
        setkeyv(X,
                c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", xname))
        X =  X[, c(
          "SUPPLIER_NAME",
          "PART_NUM",
          "QPM_PARAMETER",
          xname,
          "GRP_ACTUAL_DATE",
          "VALUE"
        ), with = F]
        
        X  = setorderv(
          X,
          c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "QPM_PARAMETER",
            "GRP_ACTUAL_DATE"
          ),
          c(1, 1, 1, -1)
        )
        Index_A = unique(X[, c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", xname), with = F])
        Index_A = Index_A[, RANK := 1:.N, by = list(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER)]
        
        Index_A = Index_A[, relabel := ifelse(RANK == 1 , "last", "prevoius"),
                          by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")]
        
        X = merge(
          X,
          Index_A,
          by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", xname),
          all.x = T
        )
        gc(Index_A)
        final_t_test = X[, list(p =    tryCatch({
          round(t.test(VALUE ~ relabel)$p.value, 3)
        } , error  = function(e) {
          404
        })) ,
        by = .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER)]
        
        final_t_test =  final_t_test[!(p %in% c(404, NA)), ]
        
        final_diff_between = X[, list(Mean = mean(VALUE , na.rm = T)) ,
                               by = .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, relabel)]
        
        Std_between = X[, list(Std_all = sd(VALUE[relabel == "prevoius"], na.rm = T)),
                        by = .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER)]
        final_diff_between = merge(
          final_diff_between,
          Std_between ,
          by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")
        )
        
        
        final_diff_between = final_diff_between[complete.cases(final_diff_between), ]
        final_diff_between =  dcast(
          final_diff_between,
          SUPPLIER_NAME + PART_NUM + QPM_PARAMETER + Std_all ~ relabel ,
          value.var = "Mean"
        )
        final_diff_between$Diff <-
          round(
            abs(
              final_diff_between$last - final_diff_between$prevoius
            ) / final_diff_between$Std_all,
            3
          )
        final_diff_between = final_diff_between[, c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", "Diff")]
        final_diff_between = final_diff_between[!(is.na(final_diff_between$Diff)), ]
        
        if (nrow(final_cpk_slope) > 0) {
          final_t_test = merge(
            final_t_test,
            final_diff_between,
            by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
            all = T
          )
          final_t_test = merge(
            final_t_test,
            final_cpk_slope,
            by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
            all = T
          )
          final_result = merge(
            final_t_test,
            final_slope,
            by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
            all = T
          )
        } else {
          final_result = merge(
            final_t_test,
            final_diff_between,
            by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
            all = T
          )
          final_result$Slope <- NA
        }
        # final_result <<- final_result
        
      } else {
        #P.value by anova
        final_anova = X[!(is.na(X$VALUE)), ]
        final_anova[, RANK := 1:.N, by = list(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER)]
        final_anova = final_anova[, list(p = tryCatch({
          round(summary(aov(VALUE ~ RANK))[[1]][["Pr(>F)"]], 3)
        } ,
        error  = function(e) {
          404
        })) ,
        by = .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER)]
        final_anova = final_anova[!(p %in% c(404, NA)), ]
        
        #diff by anova
        final_diff_between = X[, list(Mean = mean(VALUE , na.rm = T) ,
                                      Std = sd(VALUE, na.rm = T)) ,
                               by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", xname)]
        final_diff_between = final_diff_between[, list(
          Max = max(Mean, na.rm = T),
          Min = min(Mean, na.rm =  T),
          std_mean = mean(Std, na.rm = T)
        )
        , by = .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER)]
        final_diff_between = final_diff_between[complete.cases(final_diff_between), ]
        final_diff_between$Diff <-
          round((final_diff_between$Max - final_diff_between$Min) / final_diff_between$std_mean,
                3
          )
        final_diff_between = final_diff_between[, .(SUPPLIER_NAME, PART_NUM, QPM_PARAMETER, Diff)]
        
        #merge result
        if (nrow(final_slope) > 0) {
          final_anova = merge(
            final_anova,
            final_diff_between,
            by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
            all = T
          )
          
          final_anova = merge(
            final_anova,
            final_cpk_slope,
            by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
            all = T
          )
          
          final_result = merge(
            final_slope,
            final_anova,
            by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
            all = T
          )
        } else {
          final_result = merge(
            final_anova,
            final_diff_between,
            by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER"),
            all = T
          )
          final_result$Slope <- NA
        }
        
      }
      
    }
    
    gc(X)
    
    names(final_result)[grep('QPM_PARAMETER', names(final_result))]  <- "Parameter"
    
    if(commodity == "RAMP") {
      grouping_con = c("Parameter","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM")
    } else if(commodity == "DSP") {
      grouping_con = c("Parameter","DATA_SOURCE_TYPE","CAVITY_NUM","MAT_TYPE")
    } else if(commodity %in% c("EXT_HSA_GRAMLOAD","EXT_HSA_MARPOSS")) {
      grouping_con = c("Parameter","HEAD_PSN","RUN_TYPE")
    } else if(commodity == "HGA") {
      grouping_con = "Parameter"
    } else if(commodity == "MOTOR") {
      grouping_con = c("Parameter","DATA_SOURCE_TYPE")
    } else if (commodity %in% c("SUBSTRATE")) {
      grouping_con = c("Parameter","DATA_SOURCE_TYPE", "SITE_LOC")
    } else {
      grouping_con = c("Parameter","DATA_SOURCE_TYPE")
    }
    
    H = merge(H, final_result, by = c("SUPPLIER_NAME","PART_NUM",grouping_con), all.x = T)
    
    if(commodity == "DSP") {
      names(H)[grep('MAT_TYPE',names(H))] <- "Material_type"
    }
    
    H$COMMODITY = commodity
    H$REPORT_DATE = Sys.Date()
    I = H
    
    CTQ_spec = readRDS(file.path(QPM_SPEC_folder, "QPM_SPEC.rda"))
    CTQ_spec = CTQ_spec[, c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER", "Key_CTQ")]
    CTQ_spec = CTQ_spec %>% mutate(QPM_PARAMETER  = ifelse(QPM_PARAMETER == "HD_VALUE", "GRAMLOAD_VALUE", QPM_PARAMETER))
    names(CTQ_spec)[3] <- "Parameter"
    names(CTQ_spec)[4] <- "CTQ"
    
    CTQ_spec$CTQ[CTQ_spec$CTQ %in% c("YES", "Y", "y", "Yes", "yes")] <- "CTQ"
    CTQ_spec$CTQ[CTQ_spec$CTQ %in% c("NO", "N", "n", "No", "no", NA)] <- NA
    I$PART_NUM = as.character(I$PART_NUM)
    CTQ_spec$PART_NUM = as.character(CTQ_spec$PART_NUM)
    LIST = I$PART_NUM
    df_null_pn  = subset(I, !(I$PART_NUM %in% LIST))
    
    I$Parameter <- as.character(I$Parameter)
    I$SUPPLIER_NAME <- as.character(I$SUPPLIER_NAME)
    
    CTQ_spec$SUPPLIER_NAME <- as.character(CTQ_spec$SUPPLIER_NAME)
    
    I = left_join(I, CTQ_spec, by = c("SUPPLIER_NAME", "PART_NUM", "Parameter"))
    gc(CTQ_spec)
    
    if (nrow(I) > 0) {
      I$Trigger <- NA
    }
    
    if (nrow(df_null_pn) > 0) {
      df_null_pn$Trigger <- NA
      df_null_pn$CTQ <- NA
      I = rbind(I, df_null_pn)
    }
    
    I = as.data.frame(I)
    I = merge(I,
              dic_product ,
              by = c("PART_NUM"),
              all.x = T)
    temp_null_product = I[is.na(I$PRODUCT_NAME), ]
    temp_null_product = unique(temp_null_product)
    temp_product = I[!is.na(I$PRODUCT_NAME), ]
    temp_product = unique(temp_product)
    
    if (nrow(temp_product) > 0) {
      pro_name = unique(temp_product[, c("PART_NUM", "SUPPLIER_NAME", "PRODUCT_NAME")])
      pro_name = pro_name[order(pro_name$PRODUCT_NAME), ]
      pro_name = pro_name %>% group_by(PART_NUM, SUPPLIER_NAME) %>% dplyr::summarise(PRODUCT_NAME = stringr::str_flatten(PRODUCT_NAME, collapse = ", "))
      temp_product = temp_product[, !(names(temp_product) %in% "PRODUCT_NAME")]
      temp_product = left_join(temp_product,
                               pro_name,
                               by = c("PART_NUM", "SUPPLIER_NAME"))
    }
    
    setDT(temp_product)
    
    I = rbind(temp_product, temp_null_product)
    gc(temp_product)
    gc(temp_null_product)
    
    I = as.data.frame(I)
    names(I)[grep('PRODUCT_NAME', names(I))] <- "Product"
    trigger_index = trigger_index[!is.na(TRIGGER_LEVEL), c("PART_NUM",
                                                           "SUPPLIER_NAME",
                                                           "QPM_PARAMETER",
                                                           "TRIGGER_LEVEL")]
    
    trigger_index$PART_NUM <- as.character(trigger_index$PART_NUM)
    trigger_index$TRIGGER_LEVEL <-
      as.character(trigger_index$TRIGGER_LEVEL)
    setDT(I)
    I = I[,!c("Trigger")]
    
    ###### Enable Trigger ##########
    
    trigger_index_col = c("Cpk", "Diff", "Slope")
    col = names(I)
    trigger_index_col = col[col %in% trigger_index_col]
    
    if(!(commodity %in% c("EXT_HSA_GRAMLOAD", "EXT_HSA_MARPOSS", "HGA"))) {
      
      if(commodity %in% c("RAMP")){
        grouping_con = c("DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM")
      } else if (commodity %in% c("DSP")){
        grouping_con = c("DATA_SOURCE_TYPE","CAVITY_NUM","Material_type")
      } else if (commodity %in% c("MOTOR")){
        grouping_con = "DATA_SOURCE_TYPE"
      } else if (commodity %in% c("SUBSTRATE")) {
        grouping_con = c("DATA_SOURCE_TYPE", "SITE_LOC")
      } else {
        grouping_con = "DATA_SOURCE_TYPE"
      }
      
      if (!"Cpk" %in% names(I)) {
        I$Cpk <- NA
      }
      
      if (commodity %in% c("SDRAM")){
        I_CTQ = I[CTQ == "CTQ" & !is.na(Slope), c("PART_NUM","SUPPLIER_NAME",grouping_con,"Parameter","Cpk","Diff","Slope"), with=F]
      } else {
        I_CTQ = I[CTQ == "CTQ" & !is.na(p) & !is.na(Slope), c("PART_NUM","SUPPLIER_NAME",grouping_con,"Parameter","Cpk","Diff","Slope"), with=F]
      }
      
      I_CTQ = merge(I_CTQ,trigger_index, by.x = c("PART_NUM", "SUPPLIER_NAME", "Parameter"), by.y = c("PART_NUM", "SUPPLIER_NAME", "QPM_PARAMETER"), all.x = T )
      I_CTQ = suppressWarnings(reshape2::melt(I_CTQ, measure.vars = c("Cpk","Diff","Slope"), value.name = "index_value", variable.name = "citeria"))
      
      I_CTQ$TRIGGER_LEVEL <- as.character(I_CTQ$TRIGGER_LEVEL)
      trigger_con$TRIGGER_LEVEL <- as.character(trigger_con$TRIGGER_LEVEL)
      
      I_CTQ = merge(I_CTQ,trigger_con,by.x = c("TRIGGER_LEVEL","citeria"),by.y = c("TRIGGER_LEVEL","Parameter"),all.x = T)
      I_CTQ[,Check_Con := ifelse(Equation == "Less",(index_value < VALUE),ifelse(Equation == "More",(index_value > VALUE),NA))]
      
      ### return column index for color highlight 
      
      I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM",grouping_con,"Parameter","citeria","TRIGGER_LEVEL","Check_Con"), with=F]
      # remove NA in Condition Cause Disable condtion to trigger
      I_CTQ[is.na(Check_Con),"Check_Con"]<-FALSE
      I_CTQ[,Check_all := any(Check_Con), by = c("SUPPLIER_NAME","PART_NUM",grouping_con,"Parameter") ]
      I_CTQ[,Trigger := ifelse(Check_all == TRUE,"YES","")]
      I_CTQ = unique(I_CTQ[, c("SUPPLIER_NAME","PART_NUM",grouping_con,"Parameter","TRIGGER_LEVEL","Trigger"), with=F])
      I = merge(I, I_CTQ, by = c("SUPPLIER_NAME","PART_NUM",grouping_con,"Parameter"), all.x = T)
      
      I[Trigger %in% c(NA,""),"Trigger"] <-NA
      setDT(I)
      
      if("Cpk" %in% names(I)) {
        print("CPK col")
        I = I[,.SDcols = c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Last_7_Mean","Std", "Cpk", "Cpk_Slope", "p", "Diff", "Slope",
                           grouping_con, "Trigger", xname, "ETL_DATE", "YEAR_WEEK", "CTQ", "Median", "Target", "USL", "LSL", "COMMODITY", 
                           "REPORT_DATE", "Product","TRIGGER_LEVEL"), with = F]
      } else {
        print("NO CPK col")
        I = I[,.SDcols = c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std" , "p", "Diff", "Slope",
                           grouping_con, "Trigger", xname, "ETL_DATE", "YEAR_WEEK", "CTQ", "Median",  "COMMODITY", 
                           "REPORT_DATE", "Product", "TRIGGER_LEVEL"), with = F]
      }
      
    }  else if (commodity %in% c("EXT_HSA_MARPOSS")) {
      if (!"Cpk" %in% names(I)) {
        I$Cpk <- NA
      }
      
      I_CTQ = I[CTQ == "CTQ" & !is.na(p) & !is.na(Slope),]
      
      if(nrow(I_CTQ)>0) {
        
        I_CTQ = I_CTQ[,c("PART_NUM","SUPPLIER_NAME","HEAD_PSN","RUN_TYPE","Parameter","Cpk","Diff","Slope")]
        I_CTQ = merge(I_CTQ,trigger_index, by.x = c("PART_NUM","SUPPLIER_NAME","Parameter"), by.y = c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                      all.x = T )
        I_CTQ = suppressWarnings(melt(I_CTQ , measure.vars = c("Cpk","Diff", "Slope") , value.name="index_value", variable.name="citeria"))
        
        I_CTQ$TRIGGER_LEVEL <- as.character(I_CTQ$TRIGGER_LEVEL)
        trigger_con$TRIGGER_LEVEL <- as.character(trigger_con$TRIGGER_LEVEL)
        
        I_CTQ = merge(I_CTQ,trigger_con,by.x = c("TRIGGER_LEVEL","citeria"),by.y = c("TRIGGER_LEVEL","Parameter"),all.x = T)
        I_CTQ[,Check_Con := ifelse(Equation == "Less",(index_value < VALUE),ifelse(Equation == "More",(index_value > VALUE),NA))]
        
        I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM","HEAD_PSN","RUN_TYPE","Parameter","citeria","TRIGGER_LEVEL","index_value", "Equation", "VALUE","Check_Con")]
        I_CTQ = I_CTQ[,c( "SUPPLIER_NAME","PART_NUM","HEAD_PSN","RUN_TYPE","Parameter","citeria","TRIGGER_LEVEL","Check_Con")]
        # remove NA in Condition Cause Disable condtion to trigger
        I_CTQ[is.na(Check_Con),"Check_Con"]<-FALSE
        I_CTQ[,Check_all := any(Check_Con),by = .(SUPPLIER_NAME,PART_NUM,HEAD_PSN,Parameter) ]
        I_CTQ[,Trigger := ifelse(Check_all == TRUE,"YES","")]
        I_CTQ = unique(I_CTQ[,c("SUPPLIER_NAME","PART_NUM","HEAD_PSN","RUN_TYPE","Parameter","TRIGGER_LEVEL","Trigger")])
        
        I = merge(I,I_CTQ,by = c("SUPPLIER_NAME","HEAD_PSN","RUN_TYPE","PART_NUM","Parameter"),all.x = T)
        
      } else {
        I$Trigger <- NA
        I <- merge(I,trigger_index, by.x = c("PART_NUM","SUPPLIER_NAME","Parameter"), by.y = c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                   all.x = T )
        
      }
      
      I[Trigger %in% c(NA,""),"Trigger"] <-NA
      setDT(I)
      
      if("Cpk" %in% names(I)) {
        print("EXT_HSA_MARPOSS cpk ")
        
        if (!exists("I$Cpk_Slope")) {
          I$Cpk_Slope <- NA
        }
        
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Last_7_Mean","Std", "Cpk","Cpk_Slope", "p", "Diff", "Slope",
                 "HEAD_PSN","RUN_TYPE","Trigger", xname, "ETL_DATE", "YEAR_WEEK",
                 "CTQ", "Median", "Target", "USL", "LSL", "COMMODITY", "REPORT_DATE", "Product","TRIGGER_LEVEL" ) , with = F]
      } else {
        print("EXT_HSA_MARPOSS no cpk ")
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std" , "p", "Diff", "Slope",
                 "HEAD_PSN","RUN_TYPE","Trigger", xname, "ETL_DATE", "YEAR_WEEK",
                 "CTQ", "Median",  "COMMODITY", "REPORT_DATE", "Product","TRIGGER_LEVEL" ), with = F]
      }
      
    } else if (commodity %in% c("EXT_HSA_GRAMLOAD")) {
      
      if (!"Cpk" %in% names(I)) {
        I$Cpk <- NA
      }
      
      I_CTQ = I[CTQ == "CTQ" & !is.na(p) & !is.na(Slope),c("PART_NUM","SUPPLIER_NAME","HEAD_PSN","RUN_TYPE","Parameter","Cpk","Diff","Slope")]
      I_CTQ = merge(I_CTQ,trigger_index, by.x = c("PART_NUM","SUPPLIER_NAME","Parameter"), by.y = c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"), all.x = T )
      
      var_list = c("Cpk","Diff", "Slope")[c("Cpk","Diff","Slope") %in% names(I)]
      
      I_CTQ = suppressWarnings(melt(I_CTQ , measure.vars = var_list, value.name="index_value", variable.name="citeria"))
      
      I_CTQ$TRIGGER_LEVEL <- as.character(I_CTQ$TRIGGER_LEVEL)
      trigger_con$TRIGGER_LEVEL <- as.character(trigger_con$TRIGGER_LEVEL)
      
      I_CTQ = merge(I_CTQ,trigger_con,by.x = c("TRIGGER_LEVEL","citeria"),by.y = c("TRIGGER_LEVEL","Parameter"),all.x = T)
      I_CTQ[,Check_Con := ifelse(Equation == "Less",(index_value < VALUE),ifelse(Equation == "More",(index_value > VALUE),NA))]
      
      I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM","HEAD_PSN","RUN_TYPE","Parameter","citeria","TRIGGER_LEVEL","index_value", "Equation", "VALUE","Check_Con")]
      I_CTQ = I_CTQ[,c( "SUPPLIER_NAME","PART_NUM","HEAD_PSN","RUN_TYPE","Parameter","citeria","TRIGGER_LEVEL","Check_Con")]
      # remove NA in Condition Cause Disable condtion to trigger
      I_CTQ[is.na(Check_Con),"Check_Con"]<-FALSE
      I_CTQ[,Check_all := any(Check_Con),by = .(SUPPLIER_NAME,PART_NUM,HEAD_PSN,Parameter) ]
      I_CTQ[,Trigger := ifelse(Check_all == TRUE,"YES","")]
      I_CTQ = unique(I_CTQ[,c("SUPPLIER_NAME","PART_NUM","HEAD_PSN","RUN_TYPE","Parameter","TRIGGER_LEVEL","Trigger")])
      I = merge(I,I_CTQ,by = c("SUPPLIER_NAME","HEAD_PSN","RUN_TYPE","PART_NUM","Parameter"),all.x = T)
      
      I[Trigger %in% c(NA,""),"Trigger"] <-NA
      setDT(I)
      
      if("Cpk" %in% names(I)) {
        print("EXT_HSA_GRAMLOAD cpk")
        
        if (!exists("I$Cpk_Slope")) {
          I$Cpk_Slope <- NA
        }
        
        # if (any(names(I) %in% "Target") == F){
        #   I$Target <- NA
        # }
        # 
        # if (any(names(I) %in% "USL") == F){
        #   I$USL <- NA
        # } 
        # 
        # if (any(names(I) %in% "LSL") == F){
        #   I$LSL <- NA
        # }
        
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Last_7_Mean","Std", "Cpk","Cpk_Slope", "p", "Diff", "Slope",
                 "HEAD_PSN","RUN_TYPE","Trigger", xname, "ETL_DATE", "YEAR_WEEK","CTQ", "Median", "Target", "USL", "LSL", 
                 "COMMODITY", "REPORT_DATE", "Product","TRIGGER_LEVEL" ) , with = F]
        
      } else {
        print("EXT_HSA_GRAMLOAD no cpk")
        
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std" , "p", "Diff", "Slope",
                 "HEAD_PSN","RUN_TYPE","Trigger", xname, "ETL_DATE", "YEAR_WEEK",
                 "CTQ", "Median",  "COMMODITY", "REPORT_DATE", "Product","TRIGGER_LEVEL"), with = F]
      }
      
    } else if (commodity %in% c("HGA")) {
      
      ## all commodity
      trigger_index_col = c("Cpk","Diff","Slope")
      col = names(I)
      trigger_index_col = col[col %in% trigger_index_col ]
      I_CTQ = I[CTQ == "CTQ"]
      
      if(nrow(I_CTQ)>0) {
        ## filter one group shoud not trigger
        # I_CTQ = I_CTQ[!is.na(p) & !is.na(Slope) ,c("PART_NUM","SUPPLIER_NAME","DATA_SOURCE_TYPE","Parameter",trigger_index_col), with = F]
        
        I_CTQ = I_CTQ[!is.na(Slope) ,c("PART_NUM","SUPPLIER_NAME","Parameter",trigger_index_col), with = F]
        
        I_CTQ = merge(I_CTQ,trigger_index, by.x = c("PART_NUM","SUPPLIER_NAME","Parameter"), by.y = c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                      all.x = T )
        
        I_CTQ = suppressWarnings(reshape2::melt(I_CTQ , measure.vars = trigger_index_col , value.name="index_value", variable.name="citeria"))
        
        I_CTQ$TRIGGER_LEVEL <- as.character(I_CTQ$TRIGGER_LEVEL)
        I_CTQ$citeria <- as.character(I_CTQ$citeria)
        trigger_con$TRIGGER_LEVEL <- as.character(trigger_con$TRIGGER_LEVEL)
        
        I_CTQ = merge(I_CTQ,trigger_con,by.x = c("TRIGGER_LEVEL","citeria"),by.y = c("TRIGGER_LEVEL","Parameter"),all.x = T)
        I_CTQ[,Check_Con := ifelse(Equation == "Less",(index_value < VALUE),ifelse(Equation == "More",(index_value > VALUE),NA))]
        
        I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM","Parameter","citeria","TRIGGER_LEVEL","index_value", "Equation", "VALUE","Check_Con")]
        I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM","Parameter","citeria","TRIGGER_LEVEL","Check_Con")]
        
        I_CTQ[is.na(Check_Con),"Check_Con"]<-FALSE
        I_CTQ[,Check_all := any(Check_Con),by = .(SUPPLIER_NAME,PART_NUM,Parameter) ]
        I_CTQ[,Trigger := ifelse(Check_all == TRUE,"YES",""),by = .(SUPPLIER_NAME,PART_NUM,Parameter)]
        I_CTQ = unique(I_CTQ[,c("SUPPLIER_NAME","PART_NUM","Parameter","TRIGGER_LEVEL","Trigger")])
        I = merge(I,I_CTQ,by = c("SUPPLIER_NAME","PART_NUM","Parameter"),all.x = T)
      } else {
        I$Trigger <- NA
        I <- merge(I,trigger_index, by.x = c("PART_NUM","SUPPLIER_NAME","Parameter"), by.y = c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                   all.x = T )
        
      }
      
      I[Trigger %in% c(NA,""),"Trigger"] <-NA
      setDT(I)
      
      trigger_checker = unique(I[is.na(I$TRIGGER_LEVEL), c("PART_NUM","SUPPLIER_NAME","Parameter")])
      names(trigger_checker) = c("PART_NUM", "SUPPLIER_NAME", "QPM_PARAMETER")
      trigger_checker = left_join(trigger_checker, trigger_index)
      names(trigger_checker) = c("PART_NUM", "SUPPLIER_NAME", "Parameter", "TRIGGER_LEVEL")
      trigger_checker = trigger_checker[!is.na(trigger_checker$TRIGGER_LEVEL),]
      
      if (sum(!is.na(trigger_checker$TRIGGER_LEVEL))) {
        
        for (var in unique(trigger_checker$TRIGGER_LEVEL)) {
          
          trigger_temp = trigger_checker[trigger_checker$TRIGGER_LEVEL == var, ]
          
          ind <- I$PART_NUM %in% trigger_temp$PART_NUM & I$SUPPLIER_NAME %in% trigger_temp$SUPPLIER_NAME & I$Parameter %in% trigger_temp$Parameter
          
          I[ind, "TRIGGER_LEVEL"] <- var
          
        }
        
      } else {
        
        if (sum(is.na(I$TRIGGER_LEVEL)) > 0) {
          I[is.na(I$TRIGGER_LEVEL),]$TRIGGER_LEVEL <- ""
        }
        
        if(!('TRIGGER_LEVEL' %in% names(I))) {
          I$TRIGGER_LEVEL <- ""
        }
        
      }
      
      print(names(I))
      
      if("Cpk" %in% names(I)) {
        print("All Com  cpk")
        
        if("Cpk_Slope" %in% names(I)){
          
          I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean","Last_7_Mean", "Std", "Cpk","Cpk_Slope", "p", "Diff", "Slope",
                   xname, "ETL_DATE","YEAR_WEEK", "CTQ", "Median", "Target", "USL", "LSL",
                   "COMMODITY", "REPORT_DATE", "Product","Trigger","TRIGGER_LEVEL"), with = F]
          
        } else {
          
          I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean","Last_7_Mean", "Std", "Cpk", "p", "Diff", "Slope",
                   xname, "ETL_DATE","YEAR_WEEK", "CTQ", "Median", "Target", "USL", "LSL",
                   "COMMODITY", "REPORT_DATE", "Product","Trigger","TRIGGER_LEVEL"), with = F]
          
        }
        
      } else {
        print("All Com No cpk")
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std", "p", "Diff", "Slope",
                 xname, "ETL_DATE","YEAR_WEEK", "CTQ", "Median", "COMMODITY", "REPORT_DATE", "Product","Trigger" ), with = F]
      }
      
    }
    
    setorder(I, -Trigger, -Slope, na.last = T)
    
    if (sum(rank_tracker$MAX_RANK < 7) > 0) {
      rank_tracker = rank_tracker[rank_tracker$MAX_RANK < 7, ]
      names(rank_tracker)[3] <- "Parameter"
      rank_tracker = rank_tracker[, -ncol(rank_tracker)]
      rank_tracker$Remove_trigger <- T
      
      I <- left_join(I, rank_tracker)
      
      # I_set[I_set$Remove_trigger %in% T,]$Cpk_Color <- 0
      # I_set[I_set$Remove_trigger %in% T,]$Diff_Color <- 0
      # I_set[I_set$Remove_trigger %in% T,]$Slope_Color <- 0
      I[I$Remove_trigger %in% T, ]$Trigger <- ""
      
      I <- I[, -ncol(I)]
      
    }
    
    gc(rank_tracker)
    
    com_checker = !any(c("EXT_HSA_GRAMLOAD", "EXT_HSA_MARPOSS", "HGA") %in% commodity)
    
    if ((sum(!is.na(I$USL) | !is.na(I$LSL)) > 0)) {
      if (com_checker == TRUE) {
        if (commodity == "RAMP") {
          OOC_checking = I[!is.na(I$USL) |
                             !is.na(I$LSL), c(
                               "PART_NUM",
                               "Parameter",
                               "CAVITY_NUM",
                               "MOLD_NUM",
                               "SUPPLIER_NAME",
                               "USL",
                               "LSL"
                             )]
          
          max_min = X[X$RANK == 1, list(Max = max(VALUE , na.rm = T),
                                        Min = min(VALUE, na.rm = T)), by = c(
                                          "SUPPLIER_NAME",
                                          "PART_NUM",
                                          "QPM_PARAMETER",
                                          "CAVITY_NUM",
                                          "MOLD_NUM",
                                          "DATA_SOURCE_TYPE"
                                        )]
          
        } else if (commodity == "DSP") {
          OOC_checking = I[!is.na(I$USL) |
                             !is.na(I$LSL), c("PART_NUM",
                                              "Parameter",
                                              "CAVITY_NUM",
                                              "SUPPLIER_NAME",
                                              "USL",
                                              "LSL")]
          
          max_min = X[X$RANK == 1, list(Max = max(VALUE , na.rm = T),
                                        Min = min(VALUE, na.rm = T)), by = c(
                                          "SUPPLIER_NAME",
                                          "PART_NUM",
                                          "QPM_PARAMETER",
                                          "CAVITY_NUM",
                                          "DATA_SOURCE_TYPE"
                                        )]
          
        } else if (commodity == "HGA") {
          OOC_checking = I[!is.na(I$USL) |
                             !is.na(I$LSL), c("PART_NUM",
                                              "Parameter",
                                              "SUPPLIER_NAME",
                                              "USL",
                                              "LSL")]
          
          max_min = X[X$RANK == 1, list(Max = max(VALUE , na.rm = T),
                                        Min = min(VALUE, na.rm = T)), by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")]
          
        } else {
          OOC_checking = I[!is.na(I$USL) |
                             !is.na(I$LSL), c(
                               "PART_NUM",
                               "Parameter",
                               "SUPPLIER_NAME",
                               "DATA_SOURCE_TYPE",
                               "USL",
                               "LSL"
                             )]
          
          max_min = X[X$RANK == 1, list(Max = max(VALUE , na.rm = T),
                                        Min = min(VALUE, na.rm = T)), by = c("SUPPLIER_NAME",
                                                                             "PART_NUM",
                                                                             "QPM_PARAMETER",
                                                                             "DATA_SOURCE_TYPE")]
          
        }
        
        names(max_min)[3] <- "Parameter"
        
        OOC_checking = left_join(OOC_checking, max_min)
        
        OOC_checking = OOC_checking %>% mutate(OOC = case_when(
          USL < Max & LSL > Min ~ 'USL&LSL',
          USL < Max ~ 'USL',
          LSL > Min ~ 'LSL',
          TRUE ~ ""
        ))
        
        if (commodity == "RAMP") {
          OOC_checking = OOC_checking[!is.na(OOC_checking$OOC), c(
            'PART_NUM',
            'Parameter',
            'SUPPLIER_NAME',
            "CAVITY_NUM",
            "MOLD_NUM",
            'DATA_SOURCE_TYPE',
            'USL',
            'LSL',
            'OOC'
          )]
          
        } else if (commodity == "DSP") {
          OOC_checking = OOC_checking[!is.na(OOC_checking$OOC), c(
            'PART_NUM',
            'Parameter',
            'SUPPLIER_NAME',
            'CAVITY_NUM',
            'DATA_SOURCE_TYPE',
            'USL',
            'LSL',
            'OOC'
          )]
          
        } else if (commodity == "HGA") {
          OOC_checking = OOC_checking[!is.na(OOC_checking$OOC), c('PART_NUM',
                                                                  'Parameter',
                                                                  'SUPPLIER_NAME',
                                                                  'USL',
                                                                  'LSL',
                                                                  'OOC')]
          
        } else {
          OOC_checking = OOC_checking[!is.na(OOC_checking$OOC), c(
            'PART_NUM',
            'Parameter',
            'SUPPLIER_NAME',
            'DATA_SOURCE_TYPE',
            'USL',
            'LSL',
            'OOC'
          )]
          
        }
        
        if (sum(OOC_checking$OOC == "") > 0) {
          OOC_checking[OOC_checking$OOC == "", ]$OOC <- NA
        }
        
        if (nrow(OOC_checking) > 0) {
          I <- left_join(I, OOC_checking)
          
          if ("CTQ" %in% names(I) & commodity %in% c("SOC", "SDRAM")) {
            con = !is.na(I$OOC) & I$CTQ %in% 'CTQ'
            
            if (sum(con) > 0) {
              I[con, ]$Trigger <- "YES"
            }
            
          }
          
          if (commodity %in% c("SOC", "SDRAM")) {
            
            no_ctq_con = !is.na(I$OOC) & commodity %in% c("SOC", "SDRAM")
            
            if (any(no_ctq_con)) {
              I[con,]$Trigger <- "YES"
              I[con,]$TRIGGER_LEVEL <- "OOC"
              
            }
            
          }
          
        }
        
      } else {
        if ("HGA" == commodity) {
          OOC_checking = I[!is.na(I$USL) |
                             !is.na(I$LSL), c("PART_NUM",
                                              "Parameter",
                                              "SUPPLIER_NAME",
                                              "USL",
                                              "LSL")]
          
          max_min = X[X$RANK == 1, list(Max = max(VALUE , na.rm = T),
                                        Min = min(VALUE, na.rm = T)),
                      by = c("SUPPLIER_NAME", "PART_NUM", "QPM_PARAMETER")]
          
        } else {
          OOC_checking = I[!is.na(I$USL) |
                             !is.na(I$LSL), c(
                               "PART_NUM",
                               "Parameter",
                               "SUPPLIER_NAME",
                               "HEAD_PSN",
                               "RUN_TYPE",
                               "USL",
                               "LSL"
                             )]
          
          max_min = X[X$RANK == 1, list(Max = max(VALUE , na.rm = T),
                                        Min = min(VALUE, na.rm = T)),
                      by = c("SUPPLIER_NAME",
                             "PART_NUM",
                             "QPM_PARAMETER",
                             "HEAD_PSN",
                             "RUN_TYPE")]
          
        }
        
        names(max_min)[3] <- "Parameter"
        
        OOC_checking = left_join(OOC_checking, max_min)
        
        OOC_checking = OOC_checking %>% mutate(OOC = case_when(
          USL < Max & LSL > Min ~ 'USL&LSL',
          USL < Max ~ 'USL',
          LSL > Min ~ 'LSL',
          TRUE ~ ""
        ))
        
        if ("HGA" == commodity) {
          OOC_checking = OOC_checking[!is.na(OOC_checking$OOC), c('PART_NUM',
                                                                  'Parameter',
                                                                  'SUPPLIER_NAME',
                                                                  'USL',
                                                                  'LSL',
                                                                  'OOC')]
          
        } else {
          OOC_checking = OOC_checking[!is.na(OOC_checking$OOC), c(
            'PART_NUM',
            'Parameter',
            'SUPPLIER_NAME',
            'HEAD_PSN',
            'RUN_TYPE',
            'USL',
            'LSL',
            'OOC'
          )]
          
        }
        
        if (sum(OOC_checking$OOC == "") > 0) {
          OOC_checking[OOC_checking$OOC == "", ]$OOC <- NA
        }
        
        if (nrow(OOC_checking) > 0) {
          I <- left_join(I, OOC_checking)
          
        }
        
      }
      
      I <- unique(data.table(I, stringsAsFactors = F))
      
    } else {
      I$OOC <- NA
      I <- unique(data.table(I, stringsAsFactors = F))
      
    }
    
    if (any(I$Trigger == "", na.rm = T)) {
      I[I$Trigger == "", ]$Trigger <- NA
    }
    
    # I <- I[I$N > 7,] 
    
    if (sum(!is.na(I$Trigger)) > 0) {
      I <- unique(I)
      I <- get_trigger_con(I[I$Trigger == "YES", ])
      return(I)
      
    } else {
      return(NA)
    }
    
  }
  
  cpk_plotting <- function(raw, trigger, com) {
    print("Plotting")
    
    if (com %in% c("ACA", "ACT", "COIL", "HOOKUP", "HSA", "HGA")) {
      xname = "GRP_YEAR_WEEK"
    } else {
      xname = "GROUP_NUM"
    }
    
    Y_Axis_Col = unique(trigger$Parameter)
    raw = as.data.table(raw)
    
    sel_columns = names(raw)[names(raw) %in% c(
      "SUPPLIER_NAME",
      "PART_NUM",
      "DATA_SOURCE_TYPE",
      "CAVITY_NUM",
      "MOLD_NUM",
      "HEAD_PSN",
      "RUN_TYPE",
      "GROUP_DATETIME",
      "GRP_YEAR_WEEK",
      "EVENT_DATE",
      xname
    )]
    
    index_columns = c(
      "SUPPLIER_NAME",
      "PART_NUM",
      "Parameter",
      "CAVITY_NUM",
      "MOLD_NUM",
      "HEAD_PSN",
      "RUN_TYPE",
      "DATA_SOURCE_TYPE"
    )
    
    Raw_Plot = raw[, c(sel_columns, Y_Axis_Col), with = F]
    Index_plot = trigger[, names(trigger)[names(trigger) %in% index_columns]]
    Index_plot = as.data.table(Index_plot)
    
    Temp = data.table()
    
    for (i in 1:nrow(Index_plot)) {
      if (commodity == "HSA") {
        
        X = Raw_Plot[SUPPLIER_NAME == Index_plot[i, SUPPLIER_NAME] &
                       PART_NUM == Index_plot[i, PART_NUM] & 
                       HEAD_PSN == Index_plot[i, HEAD_PSN] &
                       RUN_TYPE == Index_plot[i, RUN_TYPE]] 
        setorderv(Raw_Plot, c("EVENT_DATE"), c(1))
        
        X = X[, c("SUPPLIER_NAME", "PART_NUM", "HEAD_PSN", "RUN_TYPE", "GRP_YEAR_WEEK", "EVENT_DATE", Index_plot[i, Parameter] ), with = FALSE]
        
      } else if (commodity == "RAMP") {
        X = Raw_Plot[SUPPLIER_NAME ==  Index_plot[i, SUPPLIER_NAME] &
                       PART_NUM  ==  Index_plot[i, PART_NUM] &
                       CAVITY_NUM == Index_plot[i, CAVITY_NUM] &
                       MOLD_NUM == Index_plot[i, MOLD_NUM] &
                       DATA_SOURCE_TYPE == Index_plot[i, DATA_SOURCE_TYPE]]
        setorderv(X, c("GROUP_DATETIME"), c(1))
        print("FILTER RAMP")
        
        X = X[, c("SUPPLIER_NAME", "PART_NUM", "CAVITY_NUM", "MOLD_NUM", "GROUP_DATETIME", "GROUP_NUM", "DATA_SOURCE_TYPE", Index_plot[i, Parameter] ), with = FALSE]
        
      } else if (commodity == "DSP") {
        
        if (is.na(Index_plot[i, ]$CAVITY_NUM)) {
          X = Raw_Plot[SUPPLIER_NAME ==  Index_plot[i, SUPPLIER_NAME] &
                         PART_NUM  ==  Index_plot[i, PART_NUM] &
                         DATA_SOURCE_TYPE == Index_plot[i, DATA_SOURCE_TYPE]]
          setorderv(X, c("GROUP_DATETIME"), c(1))
        } else {
          X = Raw_Plot[SUPPLIER_NAME ==  Index_plot[i, SUPPLIER_NAME] &
                         PART_NUM  ==  Index_plot[i, PART_NUM] &
                         CAVITY_NUM == Index_plot[i, CAVITY_NUM] &
                         DATA_SOURCE_TYPE == Index_plot[i, DATA_SOURCE_TYPE]]
          setorderv(X, c("GROUP_DATETIME"), c(1))
        }
        
        X = X[, c("SUPPLIER_NAME", "PART_NUM", "CAVITY_NUM", "MOLD_NUM", "GROUP_DATETIME", "GROUP_NUM", "DATA_SOURCE_TYPE", Index_plot[i, Parameter] ), with = FALSE]
        
      } else if (com %in% c("ACA", "ACT", "COIL", "HOOKUP")) {
        X = Raw_Plot[(SUPPLIER_NAME ==  Index_plot[i, SUPPLIER_NAME] &
                        PART_NUM  ==  Index_plot[i, PART_NUM]) &
                       DATA_SOURCE_TYPE == Index_plot[i, DATA_SOURCE_TYPE]]
        ifelse(commodity == "MOTOR",
               setorderv(X, c("GRP_YEAR_WEEK", xname), c(1, 1)),
               setorderv(X, c(
                 "GRP_YEAR_WEEK", "GROUP_DATETIME"
               ), c(1, 1)))
        print("OTHER")
        
        X = X[, c("SUPPLIER_NAME", "PART_NUM", "GRP_YEAR_WEEK", "DATA_SOURCE_TYPE", Index_plot[i, Parameter] ), with = FALSE]
        
      } else {
        X = Raw_Plot[(SUPPLIER_NAME == Index_plot[i, SUPPLIER_NAME] &
                        PART_NUM  ==  Index_plot[i, PART_NUM]) &
                       DATA_SOURCE_TYPE == Index_plot[i, DATA_SOURCE_TYPE]]
        ifelse(commodity == "MOTOR",
               setorderv(X, c("GRP_YEAR_WEEK", xname), c(1, 1)),
               setorderv(X, c("GRP_YEAR_WEEK", "GROUP_DATETIME",xname), c(1, 1, 1)))
        print("OTHER")
        
        X = X[, c("SUPPLIER_NAME", "PART_NUM", "GRP_YEAR_WEEK", "GROUP_NUM", "DATA_SOURCE_TYPE", Index_plot[i, Parameter] ), with = FALSE]
        
      }
      
      X = as.data.frame(X)
      ## reorder axis
      
      if (com %in% c("ACA", "ACT", "COIL", "HOOKUP", "HSA", "HGA")) {
        xname = "GRP_YEAR_WEEK"
      } else {
        xname = "GROUP_NUM"
      }
      
      X[, xname] = as.character(X[, xname])
      X[, xname] = as.vector(X[, xname])
      X[, xname] = factor(X[, xname], levels = unique(X[, xname]))
      
      SPEC_all = spec[QPM_PARAMETER == Index_plot[i, Parameter] & 
                        SUPPLIER_NAME ==  Index_plot[i, SUPPLIER_NAME] & 
                        PART_NUM == Index_plot[i, PART_NUM]]
      
      SPEC_all = SPEC_all[, c("COMMODITY", "PART_NUM", "SUPPLIER_NAME", "QPM_PARAMETER", "Target", "USL", "LSL")]
      
      SPEC_all =  melt(SPEC_all, id.vars = c( "COMMODITY", "PART_NUM", "SUPPLIER_NAME", "QPM_PARAMETER"), variable.name = "LINE_TYPE")
      
      SPEC_all = as.data.frame(SPEC_all)
      SPEC_LINE = SPEC_all[SPEC_all$LINE_TYPE != "Target", ]
      TARGET_LINE = SPEC_all[SPEC_all$LINE_TYPE == "Target", ]
      
      SPEC_LINE = SPEC_LINE[, c("PART_NUM", "SUPPLIER_NAME", "value")]
      SPEC_LINE$value = as.numeric(SPEC_LINE$value)
      
      TARGET_LINE = TARGET_LINE[, c("PART_NUM", "SUPPLIER_NAME", "value")]
      TARGET_LINE$value = as.numeric(TARGET_LINE$value)
      
      if (com %in% c("ACA", "ACT", "COIL", "HOOKUP", "EXT_HSA_GRAMLOAD", "EXT_HSA_MARPOSS", "EXT_HSA_RESONANCE_SUM", "HGA")) {
        xname = "GRP_YEAR_WEEK"
      } else {
        xname = "GROUP_NUM"
      }
      
      yname  = Index_plot[i, Parameter]
      
      if (commodity == "DSP") {
        X = X[order(X$GROUP_DATETIME, X$GROUP_NUM, decreasing = T), ]
        GROUP_NUM_LVL = unique(X[order(X$GROUP_DATETIME, X$GROUP_NUM), ]$GROUP_NUM)
        X$GROUP_NUM <- factor(X$GROUP_NUM, levels = GROUP_NUM_LVL)
      }
      
      if (grepl(" ", yname)) {
        yname = paste0("`", yname, "`")
      }
      
      p =  ggplot(X, aes_string(x = xname, y = yname)) + geom_jitter(alpha = 0.5, height = 0, width = 0.2) +
        theme_bw() +
        geom_boxplot(outlier.shape = NA, alpha = 0.5) +
        stat_summary(fun.y = mean, geom = "line", size = 2, aes(group = 1, color = "blue") ) +
        stat_summary(fun.y = mean, geom = "point") +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text(colour = "blue", size = rel(1.4))
        )
      p = p + geom_hline(data = TARGET_LINE, aes(yintercept = value), color = "green", linetype = "dashed" )
      p = p + geom_hline(data = SPEC_LINE, aes(yintercept = value), color = "red")
      
      if (grepl("`", yname)) {
        yname = stringr::str_replace(yname, "`", "")
      }
      
      if (commodity %in% c("RAMP")) {
        p = p + ggtitle(paste( "Parameter:", yname, " -  Supplier:", Index_plot[i, SUPPLIER_NAME], "-  Part_Num:", Index_plot[i, PART_NUM], "-  Cavity_num:", Index_plot[i, CAVITY_NUM], "-  Mold_num:", Index_plot[i, MOLD_NUM]))
      } else if (commodity %in% c("DSP")) {
        if (is.na(Index_plot[i, 'CAVITY_NUM'])) {
          p = p + ggtitle(paste("Parameter:", yname, " -  Supplier:", Index_plot[i, SUPPLIER_NAME], "-  Part_Num:", Index_plot[i, PART_NUM]))
        } else {
          p = p + ggtitle(paste("Parameter:", yname, " -  Supplier:", Index_plot[i, SUPPLIER_NAME], "-  Part_Num:", Index_plot[i, PART_NUM], "-  Cavity_num:", Index_plot[i, CAVITY_NUM]))
        }
      } else if(commodity %in% c("HSA")) {
        p = p + ggtitle(paste("Parameter:", yname," -  Supplier:", Index_plot[i, SUPPLIER_NAME],"-  Head_Psn:", Index_plot[i, HEAD_PSN],"-  Run_type:", Index_plot[i, RUN_TYPE]))
      } else if(commodity %in% c("SUBSTRATE")) {
        p = p + ggtitle(paste("Parameter:", yname," -  Supplier:", Index_plot[i, SUPPLIER_NAME],"-  Part_Num:", Index_plot[i, PART_NUM],"-  Site:", Index_plot[i, SITE_LOC]))
      } else {
        p = p + ggtitle(paste("Parameter:", yname, " -  Supplier:", Index_plot[i, SUPPLIER_NAME], "-  Part_Num:", Index_plot[i, PART_NUM]))
      }
      
      SUP_NAME <- stringr::str_replace_all(Index_plot[i, SUPPLIER_NAME], "/", "")
      
      PNG_COMMODITY_PATH = paste(PNG_PATH, "/", com, sep = "")
      ifelse(!dir.exists(PNG_COMMODITY_PATH), dir.create(file.path(PNG_COMMODITY_PATH)), FALSE)
      PNG_SUP_PATH = paste(PNG_COMMODITY_PATH, "/", SUP_NAME, sep = "")
      ifelse(!dir.exists(PNG_SUP_PATH), dir.create(file.path(PNG_SUP_PATH)), FALSE)
      
      if (commodity == "DSP") {
        if (!is.na(Index_plot[i, 'CAVITY_NUM'])) {
          file_name = paste(PNG_SUP_PATH, "/", Index_plot[i, Parameter], "(", Index_plot[i, 'CAVITY_NUM'], ")", "(", Index_plot[i, PART_NUM], ")", ".png", sep = "" )
        } else {
          file_name = paste(PNG_SUP_PATH,"/", Index_plot[i, Parameter], "(", Index_plot[i, PART_NUM], ")", ".png", sep = "")
        }
        
      } else {
        file_name = paste(PNG_SUP_PATH,"/", Index_plot[i, Parameter], "(", Index_plot[i, PART_NUM], ")", ".png", sep = "")
      }
      
      gc()
      
      png(file_name , width = 1200 , height = 300)
      # print(plot_list[[i]])
      print(p)
      dev.off()
      print(paste("END :  ", i))
      
      gc()
      
    }
    
  }
  
  do_num_link <- function(raw, commodity) {
    do_temp = unique(do_temp[, c("PART_NUM", "SUPPLIER_NAME", "GROUP_NUM", "DO_NUM")])
    
    do_temp = do_temp %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_NUM) %>% dplyr::summarise(DO_NUM = paste0(DO_NUM, collapse = ","))
    
    do_temp = left_join(do_temp, do_qty, by = c("DO_NUM", "PART_NUM"))
    
    if (commodity == "MOTOR") {
      tmp = as.character(do_temp$GROUP_NUM)
      
      do_temp$GROUP_NUM = paste0(substr(tmp, 4, 4), "-", substr(tmp, 2, 3), "-", substr(tmp, 1, 1), substr(tmp, 5, 6))
      
    }
    
    data_set = left_join(raw, do_temp, by = c("PART_NUM", "SUPPLIER_NAME", "GROUP_NUM"))
    
    gc()
    
    return(data_set)
    
  }
  
  get_trigger_con <- function(working_data) {
    trigger_mod = trigger_con[complete.cases(trigger_con), ]
    
    trigger_mod = trigger_mod[, c("TRIGGER_LEVEL", "Parameter", "VALUE")]
    
    trigger_mod = melt(trigger_mod, id.vars = c("TRIGGER_LEVEL", "Parameter"))
    
    trigger_mod = trigger_mod[complete.cases(trigger_mod), ]
    
    trigger_mod = reshape2::dcast(trigger_mod, TRIGGER_LEVEL ~ Parameter, drop = TRUE)
    
    names(trigger_mod) = c("TRIGGER_LEVEL", "Cpk_con", "Diff_con", "Slope_con")
    
    if ("TRIGGER_LEVEL" %in% names(working_data)) {
      working_data <-
        left_join(working_data, trigger_mod, by = "TRIGGER_LEVEL")
      
      if ("Cpk" %in% names(working_data)) {
        working_data <-
          working_data %>% mutate(trigger_con = paste(
            ifelse(
              Cpk < Cpk_con &
                !is.na(Cpk_con) &
                N >= 15,
              paste0("CPK < ", Cpk_con),
              ""
            ),
            ifelse(
              Diff > Diff_con &
                !is.na(Diff_con),
              paste0("Diff > ", Diff_con),
              ""
            ),
            ifelse(
              Slope > Slope_con &
                !is.na(Slope_con),
              paste0("Slope > ", Slope_con),
              ""
            )
          ))
        
        working_data <-
          working_data[, !(names(working_data) %in% c("Cpk_con", "Diff_con", "Slope_con"))]
        
        working_data <-
          working_data[!grepl("NA", working_data$trigger_con), ]
        
      } else {
        working_data <-
          working_data %>% mutate(trigger_con = paste(
            ifelse(Diff > Diff_con, paste0("Diff > ", Diff_con), ""),
            ifelse(Slope > Slope_con, paste0("Slope > ", Slope_con), "")
          ))
        
        working_data <-
          working_data[, !(names(working_data) %in% c("Cpk_con", "Diff_con", "Slope_con"))]
        
      }
      
    }
    
    sample_checker = working_data$Trigger %in% 'YES' &
      working_data$trigger_con == "  "
    
    if (any(sample_checker) == TRUE) {
      working_data[sample_checker, ]$Trigger <- NA
      
      working_data <-
        working_data[complete.cases(working_data$Trigger), ]
      
    }
    
    gc(sample_checker)
    
    if (all(!is.na(working_data$OOC))) {
      sel_con = complete.cases(working_data$OOC)
      
      if (any(working_data$trigger_con == "  " | working_data$trigger_con == " ")) {
        working_data$trigger_con <-
          gsub(" ", "", working_data$trigger_con, fixed = TRUE)
        working_data[sel_con, ]$trigger_con <-
          paste0(working_data[sel_con, ]$trigger_con, working_data[sel_con, ]$OOC)
        
      } else {
        working_data[sel_con, ]$trigger_con <-
          paste0(working_data[sel_con, ]$trigger_con, working_data[sel_con, ]$OOC)
        
      }
      
    }
    
    return(working_data)
    
  }
  
  #####
  
  test_type <- 1
  
  data_temp <- data.frame()
  
  Gen_date = as.character(.POSIXct(Sys.time(), tz = "Asia/Shanghai"), format = "%Y%m%d")
  
  Data_collecter = data.frame()
  
  # ABC = QPM_INP_DATA
  # QPM_INP_DATA = ABC[c(1,2)]
  
  # QPM_INP_DATA = c("INP_QPM_DASHBOARD_SEALEDDRIVEBASEPLATE.rda")
  
  write.csv(data.frame("SUPPLIER_NAME" = NA, "PART_NUM" = NA, "QPM_PARAMETER" = NA, "COMMODITY" = NA, "SUPPLIER_KEY" = NA), 
            file = file.path(ResultsPath, "E1092865", "SPEC_CHECKER_INP.csv"),
            row.names = F)
  
  for (com in QPM_INP_DATA) {
    print(com)
    
    commodity <-
      stringr::str_remove_all(com, "INP_QPM_DASHBOARD_|.rda")
    
    commodity <- commodity[!(commodity %in% "TGA")]
    
    print(commodity)
    
    tryCatch({
    
    if (commodity %in% c("HOOKUP", "MOTOR", "HGA")) {
      data_set <- readRDS(file.path(QPM_sample_folder, com))
      
    } else {
      data_set <- readRDS(file.path(QPM_folder, com))
      
    }
    
    data_set$PART_NUM <- as.character(data_set$PART_NUM)
    
    if ("DATA_SOURCE_TYPE" %in% names(data_set) == FALSE) {
      if (any(grepl("DATA_SOURCE_TYPE", names(data_set)))) {
        names(data_set)[grepl("DATA_SOURCE_TYPE", names(data_set))] <-
          "DATA_SOURCE_TYPE"
      }
      
    }
    
    if ("GROUP_DATETIME" %in% names(data_set) == FALSE) {
      
      print(paste0('"GROUP_DATETIME" %in% names(data_set) == FALSE'))
      print(paste0("COM = ", commodity))
      
      if (any(grepl("GROUP_DATETIME", names(data_set)))) {
        names(data_set)[grepl("GROUP_DATETIME", names(data_set))] <- "GROUP_DATETIME"
      } else {
        data_set$GROUP_DATETIME <- "N/A"
      }
      
      print(paste0("GROUP_DATETIME = ", unique(head(data_set$GROUP_DATETIME))))
      
    }
    
    if (commodity == "MOTOR") {
      tmp = as.character(data_set$GROUP_NUM)
      
      data_set$GROUP_NUM = paste0(substr(tmp, 4, 4), "-", substr(tmp, 2, 3), "-", substr(tmp, 1, 1), substr(tmp, 5, 6))
      
      # data_set = data_set[,!(names(data_set) %in% "SUPPLIER_KEY")]
      
      data_set$SUPPLIER_KEY <- 10
      
      print("sup_code")
      
      sup_code = readRDS(file.path(QPM_SPEC_folder, "QPM_SUP_DIC.rda"))
      sup_code = sup_code[, c("SUPPLIER_ID", "SUPPLIER_CODE", "SUPPLIER_KEY")]
      
      print(names(sup_code)[!names(sup_code) %in% names(data_set)])
      
      data_set = left_join(data_set, sup_code, by = "SUPPLIER_KEY")
      
      rm(tmp)
      
    } else if (commodity == "DSP") {
      data_set$MAT_TYPE =  ifelse(data_set$SUPPLIER_NAME == 'HI-P INTERNATIONAL LIMITED', 'Plastic', 'Metal')
      
    } else if(commodity == "STMCV"){
      
      if (sum(data_set$GROUP_DATETIME == "") > 0) {
        data_set[data_set$GROUP_DATETIME == "",]$GROUP_DATETIME <- data_set[data_set$GROUP_DATETIME == "",]$GRP_ACTUAL_DATE
      }
      
    }	else if (commodity == "PCBA") {
      data_set$GROUP_NUM = data_set$LOT_NUM
      col_number = unique(data_set$PARAMETER_NAME)
      data_set$samples <- rownames(data_set)
      data_set = spread(data_set, key = 'PARAMETER_NAME', value = 'PARAMETER_VALUE', fill = NA)
      data_set = as.data.table(data_set)
      data_set[, (col_number) := lapply(.SD, as.numeric), .SDcols = col_number]
      data_set = data_set[, !("samples"), with = F]
      data_set = as.data.frame(data_set)
      
    } else if(commodity == "EXT_HSA_GRAMLOAD"){
      data_set$SUPPLIER_NAME = data_set$LOCATION_CODE
    } else if(commodity == "EXT_HSA_MARPOSS"){
      data_set$SUPPLIER_NAME = data_set$LOCATION_CODE
    } else if(commodity == "EXT_HSA_RESONANCE_SUM"){
      data_set$SUPPLIER_NAME = data_set$LOCATION_CODE
    } else if(commodity == "HGA"){
      data_set$SUPPLIER_NAME = data_set$LOCATION_CODE
    } else if (commodity == "RAMP") {
      data_set$CAVITY_NUM = as.character(data_set$CAVITY_NUM)
    }
    
    if(!(commodity %in% c("EXT_HSA_GRAMLOAD", "EXT_HSA_MARPOSS", "EXT_HSA_RESONANCE_SUM", "HGA"))) {
      
      print(names(data_set))
      
      sup_linker <- unique(data_set[, c("SUPPLIER_NAME", "SUPPLIER_KEY")])
      
    }
    
    print("CPK Table running")
    
    trigger <- cpk_table(data_set, commodity)
    
    print(unique(trigger))
    
    print("CPK Table completed")
    
    if (commodity %in% NA) {
      
      print("GRP_YEAR_WEEK Trigger dupication checking")
      
      record_file = file.path(output_path, paste0(commodity ,"_recorder_keeper.rda"))
      
      if(file.exists(record_file)){
        
        record_list = readRDS(record_file)
        
        trigger = anti_join(trigger, record_list, by =  c("COMMODITY","SUPPLIER_NAME","PART_NUM","Parameter","DATA_SOURCE_TYPE","GRP_YEAR_WEEK"))
        
        if(nrow(trigger )) {
          print("Removing existing triggers")
          
          record_list_t2d = unique(trigger[,c("COMMODITY","SUPPLIER_NAME","PART_NUM","Parameter","DATA_SOURCE_TYPE","GRP_YEAR_WEEK")])
          
          record_list_t2d$Recorded_date <- Sys.Date()
          
          record_list = bind_rows(record_list_t2d, record_list)
          
          saveRDS(record_list, file = record_file)
          
        } else {
          print("No new trigger")
        }
        
      } else {
        
        if(!is.na(trigger)) { 
          
          print("No Recorded Triggers")
          
          record_list = unique(trigger[,c("COMMODITY","SUPPLIER_NAME","PART_NUM","Parameter","DATA_SOURCE_TYPE","GRP_YEAR_WEEK")])
          
          record_list$Recorded_date <- Sys.Date()
          
          saveRDS(record_list, file = record_file)
          
        }
        
      }
      
    }
    
    if (any(!is.na(trigger))) {
      print("Trigger Found")
      
      if(commodity %in% c("EXT_HSA_MARPOSS", "EXT_HSA_GRAMLOAD")){
        commodity = "HSA"
      }
      
      File_Path =  paste0(CSV_PATH, "/", commodity, sep = "")
      ifelse(!dir.exists(file.path(File_Path)), dir.create(file.path(File_Path)), FALSE)
      
      File_Name = paste0(File_Path,"/", commodity, "_INP_", "Result", "_", Gen_date, ".csv", sep = "")
      
      selecter <-
        c("SUPPLIER_NAME",
          "PART_NUM",
          "Parameter",
          "CAVITY_NUM",
          "MOLD_NUM",
          "GROUP_NUM",
          "HEAD_PSN",
          "RUN_TYPE",
          "N",
          "Mean",
          "Std",
          "Median",
          "DATA_SOURCE_TYPE",
          "ETL_LOAD_DATE",
          "YEAR_WEEK",
          "Target",
          "USL",
          "LSL",
          "Cpk",
          "p",
          "Diff",
          "Slope",
          "OOC",
          "Trigger",
          "TRIGGER_LEVEL",
          "trigger_con",
          "Material_type",
          "Product"
        )
      
      trigger <-
        trigger[, names(trigger)[names(trigger) %in% selecter]]
      
      tryCatch({
        cpk_plotting(data_set, trigger, commodity)
        
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
      
      gc()
      
      if (!(commodity %in% c("ACA", "ACT", "COIL", "HOOKUP", "HSA"))) {
        trigger <- do_num_link(trigger, commodity)
      }
      
      trigger <- unique(trigger)
      
      col_sorter <- c("PART_NUM",
                      "Parameter",
                      "SUPPLIER_NAME",
                      "CAVITY_NUM",
                      "MOLD_NUM",
                      "Material_type",
                      "HEAD_PSN",
                      "RUN_TYPE",
                      "N",
                      "Mean",
                      "Std",
                      "Cpk",
                      "p",
                      "Diff",
                      "Slope",
                      "GROUP_NUM",
                      "ETL_LOAD_DATE",
                      "DATA_SOURCE_TYPE",
                      "YEAR_WEEK",
                      "Median",
                      "Target",
                      "USL",
                      "LSL",
                      "Product",
                      "OOC",
                      "Trigger",
                      "TRIGGER_LEVEL",
                      "trigger_con",
                      "DO_NUM",
                      "DO_QTY")
      
      trigger <-
        trigger[, order(match(names(trigger), col_sorter))]
      
      print(names(trigger))
      
      if (commodity == "DSP") {
        print("DSP")
        
        trigger <-
          trigger[, order(match(names(trigger), col_sorter))]
        
        names(trigger) <-
          c("PART_NUM",
            "Parameter",
            "SUPPLIER_NAME",
            "CAVITY_NUM",
            "Material type",
            "N",
            "Mean",
            "Std",
            "Cpk",
            "p",
            "Diff",
            "Slope",
            "GROUP_NUM",
            "ETL_LOAD_DATE",
            "DATA_SOURCE_TYPE",
            "YEAR_WEEK",
            "Median",
            "Target",
            "USL",
            "LSL",
            "Product",
            "OOC",
            "Trigger",
            "TRIGGER_LEVEL",
            "Trigger Criteria",
            "DO_NUM",
            "DO_QTY"
          )
        
      } else if (commodity  == "RAMP") {
        print("RAMP")
        
        trigger <-
          trigger[, order(match(names(trigger), col_sorter))]
        
        names(trigger) <- c(
          "PART_NUM",
          "Parameter",
          "SUPPLIER_NAME",
          "CAVITY_NUM",
          "MOLD_NUM",
          "N",
          "Mean",
          "Std",
          "Cpk",
          "p",
          "Diff",
          "Slope",
          "GROUP_NUM",
          "ETL_LOAD_DATE",
          "DATA_SOURCE_TYPE",
          "YEAR_WEEK",
          "Median",
          "Target",
          "USL",
          "LSL",
          "Product",
          "OOC",
          "Trigger",
          "TRIGGER_LEVEL",
          "Trigger Criteria",
          "DO_NUM",
          "DO_QTY"
        )
        
      } else if (commodity %in% c("ACA", "ACT", "COIL", "HOOKUP")) {
        if (!("trigger_con" %in% names(trigger))) {
          trigger$trigger_con <- NA
        }
        
        if (!("GROUP_NUM" %in% names(trigger))) {
          trigger$GROUP_NUM <- NA
        }
        
        if (!("DO_NUM" %in% names(trigger))) {
          trigger$DO_NUM <- NA
        }
        
        if (!("DO_QTY" %in% names(trigger))) {
          trigger$DO_QTY <- NA
        }
        
        trigger <-
          trigger[, order(match(names(trigger), col_sorter))]
        
        names(trigger) <-
          c("PART_NUM",
            "Parameter",
            "SUPPLIER_NAME",
            "N",
            "Mean",
            "Std",
            "Cpk",
            "p",
            "Diff",
            "Slope",
            "GROUP_NUM",
            "ETL_LOAD_DATE",
            "DATA_SOURCE_TYPE",
            "YEAR_WEEK",
            "Median",
            "Target",
            "USL",
            "LSL",
            "Product",
            "OOC",
            "Trigger",
            "TRIGGER_LEVEL",
            "Trigger Criteria",
            "DO_NUM",
            "DO_QTY")
        
      } else if (commodity %in% c("HSA")) {
        
        if (!("trigger_con" %in% names(trigger))) {
          trigger$trigger_con <- NA
        }
        
        if (!("GROUP_NUM" %in% names(trigger))) {
          trigger$GROUP_NUM <- NA
        }
        
        if (!("DO_NUM" %in% names(trigger))) {
          trigger$DO_NUM <- NA
        }
        
        if (!("DO_QTY" %in% names(trigger))) {
          trigger$DO_QTY <- NA
        }
        
        if (!("ETL_LOAD_DATE" %in% names(trigger))) {
          trigger$ETL_LOAD_DATE <- NA
        }
        
        if (!("DATA_SOURCE_TYPE" %in% names(trigger))) {
          trigger$DATA_SOURCE_TYPE <- NA
        }
        
        trigger <-
          trigger[, order(match(names(trigger), col_sorter))]
        
        names(trigger) <-	c("PART_NUM",
                            "Parameter",
                            "SUPPLIER_NAME",
                            "HEAD_PSN",
                            "RUN_TYPE",
                            "N",
                            "Mean",
                            "Std",
                            "Cpk",
                            "p",
                            "Diff",
                            "Slope",
                            "GROUP_NUM",
                            "ETL_LOAD_DATE",
                            "DATA_SOURCE_TYPE",
                            "YEAR_WEEK",
                            "Median",
                            "Target",
                            "USL",
                            "LSL",
                            "Product",
                            "OOC",
                            "Trigger",
                            "TRIGGER_LEVEL",
                            "Trigger Criteria",
                            "DO_NUM",
                            "DO_QTY"
        )
        
      } else {
        
        trigger <-
          trigger[, order(match(names(trigger), col_sorter))]
        
        names(trigger) <-
          c(
            "PART_NUM",
            "Parameter",
            "SUPPLIER_NAME",
            "N",
            "Mean",
            "Std",
            "Cpk",
            "p",
            "Diff",
            "Slope",
            "GROUP_NUM",
            "ETL_LOAD_DATE",
            "DATA_SOURCE_TYPE",
            "YEAR_WEEK",
            "Median",
            "Target",
            "USL",
            "LSL",
            "Product",
            "OOC",
            "Trigger",
            "TRIGGER_LEVEL",
            "Trigger Criteria",
            "DO_NUM",
            "DO_QTY"
          )
        
      }
      
      if (!(commodity %in% c("HSA"))) {
        sup_code = readRDS(file.path(QPM_SPEC_folder, "QPM_SUP_DIC.rda"))
        sup_code = sup_code[, c("SUPPLIER_ID", "SUPPLIER_CODE", "SUPPLIER_KEY")]
        
        sup_linker = join(sup_linker, sup_code, by = "SUPPLIER_KEY")
        
        sup_linker = sup_linker[, c("SUPPLIER_NAME", "SUPPLIER_ID", "SUPPLIER_CODE")]
        
        trigger <- left_join(trigger, sup_linker, by = "SUPPLIER_NAME")
        
        trigger <- trigger[complete.cases(trigger$SUPPLIER_CODE),]
        
      } else {
        trigger$SUPPLIER_ID <- NA
        trigger$SUPPLIER_CODE <- NA
        
      }
      
      if (commodity %in% c("HSA") & file.exists(File_Name)) {
        hsa_temp = read.csv(File_Name, stringsAsFactors = F)
        trigger = unique(dplyr::bind_rows(trigger, hsa_temp))
        
      }
      
      write.csv(trigger, file  = File_Name, row.names = F, na = "")
      
      trigger$COMMODITY <- commodity
      
      trigger$ETL_LOAD_DATE <- as.character(trigger$ETL_LOAD_DATE)
      
      Data_collecter = dplyr::bind_rows(trigger, Data_collecter)
      
      gc()
      
    }
    
    }, error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
    })
    
  }
  
  Config_name = paste0("Auto_config_", Gen_date, '.rda')
  
  saveRDS(data.frame(Data_collecter),
          file  = file.path(CONFIG_PATH, Config_name))
  
}