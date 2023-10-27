custom_data_prep = function(input_path, output_path){
  
  library(stringr)
  library(reshape2)
  library(plyr)
  library(dplyr)
  library(DT)
  library(reshape2)
  library(tidyr)
  library(data.table)
  library(ggplot2)
  
  QPM_folder <- file.path(ResultsPath, "E1052810", "RDA_MOD")
  QPM_SPEC_folder <- file.path(ResultsPath, "E1082262", "QPM_transfer")
  CSV_PATH <- file.path(output_path,"CSV")
  PNG_PATH <- file.path(output_path,"PNG")
    
  if (!dir.exists(CSV_PATH)) {
    dir.create(file.path(CSV_PATH))
  }
  
  if (!dir.exists(PNG_PATH)) {
    dir.create(file.path(PNG_PATH))
  }
  
  REMOVE_PARAM_LIST = c("ACA_LINE_NUM", "ACTTR_CBORE_PSN", "ACTTR_HOLE_PSN", "ACTTR_POST_PSN", 
                        "ACTTR_THREAD_HOLE_PSN", "BASEPLATE_CAVITY_NUM", "BASEPLATE_LOT_NUM", "BASEPLATE_MOLD_NUM", 
                        "BASEPLATE_MOLD_SPECIAL_ID", "BREATHER_FILTER_HOLE_PSN", "BTM_CUST_THREAD_1_PSN", "BTM_CUST_THREAD_2_PSN", 
                        "BTM_CUST_THREAD_3_PSN", "BTM_CUST_THREAD_4_PSN", "CA_LOT_NUM", "CAVITY_NUM", 
                        "CNCTR_THREAD_1_PSN", "CNCTR_THREAD_2_PSN", "DAMPER_LOT_NUM", "DAMPER_REEL_NUM", 
                        "DATECODE", "DSP_HOLE_1_PSN", "DSP_HOLE_1_PSN_2ND", "DSP_HOLE_2_PSN", 
                        "DSP_HOLE_2_PSN_2ND", "DSP_PIN_1_PSN", "DSP_SLOT_PSN", "DSP_THREAD_1_COMPOSITE_PSN", 
                        "DSP_THREAD_2_COMPOSITE_PSN", "DSP_THREAD_3_COMPOSITE_PSN", "DSP_THREAD_HOLE_1_PSN", "DSP_THREAD_HOLE_2_PSN", 
                        "DSP_THREAD_HOLE_3_PSN", "DSP_THREAD_HOLE_4_PSN", "FINAL_TEST_FLAG", "FLEXURE_LOT_NUM", 
                        "GROUP_NUM", "IDCS_PIN_PSN", "LOADBEAM_LOT_NUM", "LOT_QTY", 
                        "LOT_SHIP_QTY", "MACHINE_NUM_OP2", "MFG_DATE_CODE", "MOLD_NUM", 
                        "MOLD_SPECIAL_ID", "MOTOR_CNCTN_SLOT_PSN_1", "MOTOR_CNCTN_SLOT_PSN_2", "MOTOR_HOLE_ID", 
                        "MOTOR_HOLE_PSN", "MOTOR_ID_ROUNDNESS", "MOTOR_ID_TRUE_PSN", "MOTOR_OD_TRUE_PSN", 
                        "MOTOR_STATOR_LEAD_IN_OD_PSN", "MOTOR_THRUST_YOKE_ID", "MOTOR_THRUST_YOKE_ID_PSN", "NCA_LOT_NUM", 
                        "PCBA_THREAD_1_PSN", "PCBA_THREAD_2_PSN", "PCBA_THREAD_3_PSN", "PCBA_THREAD_4_PSN", 
                        "PCBA_THREAD_5_PSN", "PCBA_THREAD_6_PSN", "PCBA_THREAD_7_PSN", "PCC_CNCTR_LOCATING_PIN_1_PSN", 
                        "PCC_CNCTR_LOCATING_PIN_2_PSN", "PCC_MOUNT_THREAD_1_PSN", "PCC_MOUNT_THREAD_2_PSN", "POST_MACHINE_NUM", 
                        "PRE_MACHINE_NUM", "PZT_NEGATIVE_LOT_NUM", "PZT_NEGATIVE_WAFER_NUM", "PZT_POSITIVE_LOT_NUM", 
                        "PZT_POSITIVE_WAFER_NUM", "RAMP_CBORE_HOLE_ID", "RAMP_CBORE_HOLE_PSN", "RAMP_CBORE_THREAD_PSN", 
                        "RAMP_STOPPER_PSN", "RAW_MATERIAL_LOT_NUMBER", "RECORD_SEQ", "SAMPLE_NUM", 
                        "SHAFT_HOLE_POSITION", "SHIP_DATE", "SIDE_CUST_THREAD_1_PSN", "SIDE_CUST_THREAD_2_PSN", 
                        "SIDE_CUST_THREAD_3_PSN", "SIDE_CUST_THREAD_4_PSN", "SIDE_CUST_THREAD_5_PSN", "SIDE_CUST_THREAD_6_PSN", 
                        "SUB_LOT_NUM", "TOP_COVER_LOCATING_POST_1_PSN", "TOP_COVER_LOCATING_POST_2_PSN", "TOP_COVER_THREAD_1_CMPST_PSN", 
                        "TOP_COVER_THREAD_1_PSN", "TOP_COVER_THREAD_2_CMPST_PSN", "TOP_COVER_THREAD_2_PSN", "TOP_COVER_THREAD_3_CMPST_PSN", 
                        "TOP_COVER_THREAD_3_PSN", "TOP_COVER_THREAD_4_CMPST_PSN", "TOP_COVER_THREAD_4_PSN", "TOP_COVER_THREAD_5_CMPST_PSN", 
                        "TOP_COVER_THREAD_5_PSN", "TOP_COVER_THREAD_6_CMPST_PSN", "TOP_COVER_THREAD_6_PSN", "VCM_BTM_THREAD_1_COMPOSITE_PSN",
                        "VCM_BTM_THREAD_1_PSN", "VCM_BTM_THREAD_2_COMPOSITE_PSN", "VCM_BTM_THREAD_2_PSN", "VCM_BTM_THREAD_3_PSN", 
                        "VCM_LOCATE_POST_1_PSN", "VCM_LOCATE_POST_2_PSN", "VCM_LOCATING_HOLE_1_PSN", "VCM_LOCATING_HOLE_2_PSN", 
                        "VCM_LOCATING_HOLE_3_PSN", "VCM_TOP_THREAD_1_CMPST_PSN", "VCM_TOP_THREAD_2_CMPST_PSN", "VCM_TOP_THREAD_3_CMPST_PSN", 
                        "VCM_TOP_THREAD_PSN")
  
  remove_list <- c("QPM_OOC_RANKING_NEW@E1033851.csv",'QPM_DASHBOARD_DIC.rda','QPM_DASHBOARD_DO.rda','QPM_DASHBOARD_CHART_KEY.rda','QPM_DASHBOARD_SQE_CTQ.rda','QPM_SPEC.rda','QPM_TRIGGER.rda')
  
  QPM_LAT_DATA = list.files(QPM_folder, pattern = "QPM_DASHBOARD_")
  QPM_LAT_DATA <- QPM_LAT_DATA[!(QPM_LAT_DATA %in% remove_list)]
  QPM_LAT_DATA <- QPM_LAT_DATA[!grepl("INP_QPM_DASHBOARD_|.rda.gz", QPM_LAT_DATA)]
  
  DIC = readRDS(file.path(QPM_SPEC_folder,"QPM_DIC.rda"))
  DIC = unique(DIC)
  DIC[] = lapply(DIC, as.character)
  DIC$PART_NUM = stringr::str_remove_all(DIC$PART_NUM,c("\\bXXX|\\b "))
  DIC$ODT_PRODUCT_NAME = toupper(DIC$ODT_PRODUCT_NAME)
  DIC[DIC==""] = "N/A"
  names(DIC)[names(DIC) %in% 'ODT_PRODUCT_NAME'] <- "PRODUCT_NAME"
  DIC <- DIC[,c("PRODUCT_NAME", "PART_NUM", "PRODUCT_TYPE")]
  
  trigger_con = readRDS(file.path(QPM_SPEC_folder,"QPM_TRIGGER.rda"))
  trigger_con = as.data.table(trigger_con)
  # trigger_con = trigger_con[,-5]
  trigger_con$TRIGGER_LEVEL = trigger_con$TRIGGER_LEVEL
  
  ##### SPEC 
  
  SPEC = readRDS(file.path(QPM_SPEC_folder,"QPM_SPEC.rda"))
  
  colnames(SPEC)[grep('QPM.trigger.level',colnames(SPEC))] <-"TRIGGER_LEVEL"
  
  SPEC$PRODUCT_NAME = as.character(toupper(SPEC$PRODUCT_NAME))
  SPEC$PRODUCT_NAME = stringr::str_remove_all(SPEC$PRODUCT_NAME, "-| ")
  
  SPEC$SUPPLIER_NAME = as.character(toupper(SPEC$SUPPLIER_NAME))
  SPEC$COMMODITY = as.character(toupper(SPEC$COMMODITY))
  SPEC$QPM_PARAMETER = as.character(toupper(SPEC$QPM_PARAMETER))
  
  treat_specs = function(x){
    x = as.character(x)
    x[x %in% c("","N/A","-")] = ""
    as.numeric(x)
  }
  SPEC$LSL = treat_specs(SPEC$LSL)
  SPEC$USL = treat_specs(SPEC$USL)
  
  spec = SPEC
  spec <- as.data.table(spec)
  
  trigger_index = SPEC[,c("COMMODITY","PART_NUM","SUPPLIER_NAME","QPM_PARAMETER","Key_CTQ"
                          ,"TRIGGER_LEVEL","Remark")]
  trigger_index <- as.data.table(trigger_index)
  
  ########################
  
  SPEC_ref = SPEC[,c("PRODUCT_NAME","COMMODITY","PART_NUM","SUPPLIER_NAME","QPM_PARAMETER","Target","USL","LSL")]
  SPEC = SPEC[,c("COMMODITY","PART_NUM","SUPPLIER_NAME","QPM_PARAMETER","Target","USL","LSL")]
  SPEC$COMMODITY = toupper(SPEC$COMMODITY)
  SPEC_2 = melt(SPEC, id=c("COMMODITY","PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                measure.vars = c("Target","USL","LSL"),
                variable.name = "LINE_TYPE",value.name = "VALUE")
  SPEC_2$SUPPLIER_NAME[SPEC_2$SUPPLIER_NAME=="NIDEC PHILIPPINES FACTORY (NCFL)"] = "NIDEC PHILIPPINES CORP (NILF) (MBA)"
  
  #####
  
  CHART_KEY <- readRDS(file.path(QPM_SPEC_folder, 'QPM_CHART_KEY.rda'))
  
  CHART_KEY <- CHART_KEY[,c("PRODUCT_TYPE","SUPPLIER_NAME","PRODUCT_PART_NUM","PARM_NAME","CONTROL_CHART_KEY")]
  names(CHART_KEY) <- c("COMMODITY","SUPPLIER_NAME","PART_NUM","Parameter","CHART_KEY")
  CHART_KEY$COMMODITY[CHART_KEY$COMMODITY=="MO"] <- "MOTOR"
  CHART_KEY$COMMODITY <- NULL
  CHART_KEY[] <- lapply(CHART_KEY, as.character)
  
  #####
  
  do_temp <- readRDS(file.path(QPM_folder, 'QPM_DASHBOARD_DO.rda'))
  
  do_temp <- unique(do_temp[,c("PART_NUM", "SUPPLIER_NAME", "GROUP_NUM", "DO_NUM", "SHIP_ACTUAL_DATE")])
  
  do_qty <- readRDS(file.path(QPM_SPEC_folder, "SHP_INFO.rda"))
  
  do_qty <- unique(do_qty[,c("RECEIPT_DO_NUM","RECEIPT_PART_NUMBER","RECEIPT_QUANTITY")])
  
  names(do_qty)[1:3] <- c("DO_NUM","PART_NUM","DO_QTY")
  
  #####
  
  cpk_table <- function(raw, com){
    
    dic_product = as.data.table(DIC)
    
    X = raw
    X = as.data.table(X)
    X = X[,which(unlist(lapply(X, function(x)!all(is.na(x))))),with=F]
	
	if(com %in% c("ACA", "ACT", "COIL", "HOOKUP")){
		xname = "GRP_YEAR_WEEK"
	} else {
		xname = "GROUP_NUM"
	}
	
    X$TRIGGER_LEVEL <- as.character(X$TRIGGER_LEVEL)
    col = sort(names(X)[sapply(X, is.numeric)])
    col = col[!(col %in% REMOVE_PARAM_LIST)]
    
    # X = X[, sapply(X, function(col) length(unique(col))) > 5]
    
    X[,(xname):= lapply(.SD, as.character), .SDcols = xname]
    SPEC_unique = SPEC_2 %>% group_by(PART_NUM, SUPPLIER_NAME,QPM_PARAMETER, LINE_TYPE) %>% slice(1)
    SPEC = tryCatch({dcast(SPEC_unique,SUPPLIER_NAME+PART_NUM+QPM_PARAMETER ~ LINE_TYPE,value.var = "VALUE")}  
                    #, fun.aggregate = function(x) mean(x,na.rm = TRUE)
                    ,error = function(e){NULL}
                    #,warning = function(w){NULL}
    )
    
    COMMODITY <- com
    PART_NUM <- sort(unique(data_set$PART_NUM))
    SUPPLIER_NAME <- sort(unique(data_set$SUPPLIER_NAME))
    
    print("CPK Prep done")
    
    if(commodity == "RAMP") {
      X = X[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","GROUP_DATETIME","MOLD_NUM","CAVITY_NUM","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname,col), with = F]
      setkeyv(X, c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","GROUP_DATETIME","CAVITY_NUM","MOLD_NUM","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname))
      X = melt(X , measure.vars = col , value.name="VALUE", variable.name="QPM_PARAMETER" )
      X = X[!is.na(X$VALUE),]
      
      X = X[,NORMALIZE_VALUE := (VALUE- mean(VALUE, na.rm = T) )/sd(VALUE, na.rm = T) ,
            by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER","MOLD_NUM","CAVITY_NUM" )]
      
      dt = X[,list(N = .N, Mean = mean(VALUE, na.rm = T), Std = sd(VALUE ,na.rm = T), Median = median(VALUE ,na.rm = T)
                   ,ETL_LOAD_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)] , YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)]
                   ,GROUP_DATETIME =max(GROUP_DATETIME)), 
             by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER","MOLD_NUM","CAVITY_NUM",xname)]
      
      dt = setorderv(dt,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER","MOLD_NUM","CAVITY_NUM", "GROUP_DATETIME"),c(1,1,1,1,1,1,-1) )
      dt = dt[!(is.na(dt$Mean)),]
      dt[,RANK := 1:.N ,by = .(SUPPLIER_NAME,PART_NUM,DATA_SOURCE_TYPE,QPM_PARAMETER,MOLD_NUM,CAVITY_NUM)]
      dt[,Last_7_Mean := mean(Mean[RANK[1:7]],na.rm = T), 
         by =  .(SUPPLIER_NAME,PART_NUM,DATA_SOURCE_TYPE,QPM_PARAMETER,MOLD_NUM,CAVITY_NUM)]
      
      cpk_dt = dt[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER",xname,"MOLD_NUM","CAVITY_NUM","Mean","Std","RANK","GROUP_DATETIME"), with = F]
      rmlist = names(dt)
      
      raw_dt = dt[RANK ==1]
      gc(dt)
      spec = SPEC
      spec = as.data.table(spec)
      if(nrow(spec) > 0) {
        
        spec = spec[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,Target,USL,LSL)]
        y =SPEC_2
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
        cpks_Slope = numeric(nrow(raw_dt))
        for(i in 1:nrow(raw_dt)){
          Usl = raw_dt$USL[i]
          Lsl = raw_dt$LSL[i]
          Mean = raw_dt$Mean[i]
          Mean_7_point = raw_dt$Last_7_Mean[i]
          Std = raw_dt$Std[i]
          cpks[i] = 
            if(!is.na(Usl) & !is.na(Lsl)){
              min(  (Usl - Mean)/(3*Std), (Mean - Lsl)/(3*Std))
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
        
      }
    } else if (commodity == "DSP") {
      
      X = X[,c("SUPPLIER_NAME","PART_NUM","GROUP_DATETIME","CAVITY_NUM","DATA_SOURCE_TYPE","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname,col) ,  with = F]
      setkeyv(X, c("SUPPLIER_NAME","PART_NUM","GROUP_DATETIME","CAVITY_NUM","DATA_SOURCE_TYPE","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname))
      X = melt(X , measure.vars = col , value.name="VALUE", variable.name="QPM_PARAMETER" )
      X = X[!is.na(X$VALUE),]
      ## Group for plastic and metal DSP
      check_dsp_metal = unique(trigger_index[,c("PART_NUM","Remark")])
      check_dsp_metal$PART_NUM <- as.character(check_dsp_metal$PART_NUM)
      X = merge(X,check_dsp_metal, by = c("PART_NUM"), allow.cartesian=TRUE)
      
      ## calculate separate by DSP material 
      X_Metal  = X[Remark =="Metal",!c("Remark")]
      X_Plastic  = X[Remark =="Plastic",!c("Remark")]
      gc(X)
      
      if(nrow(X_Plastic) >0){
        X_Plastic = X_Plastic[,NORMALIZE_VALUE := (VALUE- mean(VALUE, na.rm = T) )/sd(VALUE, na.rm = T) ,    
                              by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER","CAVITY_NUM")]
        
        dt = X_Plastic[,list(N = .N, Mean = mean(VALUE, na.rm = T), Std = sd(VALUE ,na.rm = T), Median = median(VALUE ,na.rm = T)
                             ,ETL_LOAD_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)] , YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)]
                             , GROUP_DATETIME =max(GROUP_DATETIME)), 
                       by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER","CAVITY_NUM",xname)]
        
        dt = setorderv(dt,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER","CAVITY_NUM","GROUP_DATETIME"),c(1,1,1,1,1,-1))
        dt = dt[!(is.na(dt$Mean)),]
        dt[,RANK := 1:.N ,by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,CAVITY_NUM)]
        
        dt[,Last_7_Mean := mean(Mean[RANK[1:7]],na.rm = T), 
           by =  .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,CAVITY_NUM)]
        cpk_dt_plas = dt[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER",xname,"CAVITY_NUM","Mean","Std","RANK","GROUP_DATETIME"), with = F]
        
        rmlist = names(dt)
        raw_dt_Plastic = dt[RANK ==1]
        gc(dt)
        spec = SPEC
        spec = as.data.table(spec)
        
        if(nrow(spec) > 0) {
          spec = spec[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,Target,USL,LSL)]
          y =SPEC_2
          raw_dt_Plastic$PART_NUM = as.character(raw_dt_Plastic$PART_NUM)
          spec$PART_NUM = as.character(spec$PART_NUM)
          collist = unique(names(raw_dt_Plastic)[!names(raw_dt_Plastic) %in% c("RANK")])
          raw_dt_Plastic = raw_dt_Plastic[,collist, with =F]
          raw_dt_Plastic = merge(raw_dt_Plastic,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                                 by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
          
          ## cal  cpk
          
          raw_dt_Plastic$Target = as.numeric(raw_dt_Plastic$Target)
          raw_dt_Plastic$USL = as.numeric(raw_dt_Plastic$USL)
          raw_dt_Plastic$LSL = as.numeric(raw_dt_Plastic$LSL)
          
          cpks = numeric(nrow(raw_dt_Plastic))
          cpks_Slope = numeric(nrow(raw_dt_Plastic))
          for(i in 1:nrow(raw_dt_Plastic)){
            Usl = raw_dt_Plastic$USL[i]
            Lsl = raw_dt_Plastic$LSL[i]
            Mean = raw_dt_Plastic$Mean[i]
            Mean_7_point = raw_dt_Plastic$Last_7_Mean[i]
            Std = raw_dt_Plastic$Std[i]
            
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
          raw_dt_Plastic$Cpk = cpks
          
          ## create CPK moving DT
          cpk_dt_plas = merge(cpk_dt_plas,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                              by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
          cpk_dt_plas$Target = as.numeric(cpk_dt_plas$Target)
          cpk_dt_plas$USL = as.numeric(cpk_dt_plas$USL)
          cpk_dt_plas$LSL = as.numeric(cpk_dt_plas$LSL)
          cpk_moving = numeric(nrow(cpk_dt_plas))
          
          for(i in 1:nrow(cpk_dt_plas)){
            Usl = cpk_dt_plas$USL[i]
            Lsl = cpk_dt_plas$LSL[i]
            Mean = cpk_dt_plas$Mean[i]
            Std = cpk_dt_plas$Std[i]
            
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
          cpk_dt_plas$Cpk = cpk_moving
          
        }
      }
      if(nrow(X_Metal) >0) {
        
        X_Metal = X_Metal[,NORMALIZE_VALUE := (VALUE- mean(VALUE, na.rm = T) )/sd(VALUE, na.rm = T) , 
                          by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER","CAVITY_NUM" )]
        
        dt = X_Metal[,list(N = .N, Mean = mean(VALUE, na.rm = T), Std = sd(VALUE ,na.rm = T), Median = median(VALUE ,na.rm = T)
                           ,ETL_LOAD_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)] , YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)]
                           ,GROUP_DATETIME =max(GROUP_DATETIME)),
                     by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER",xname)]
        
        dt = setorderv(dt,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER","GROUP_DATETIME"),c(1,1,1,1,-1))
        dt = dt[!(is.na(dt$Mean)),]
        dt[,RANK := 1:.N ,by = .(SUPPLIER_NAME,PART_NUM,DATA_SOURCE_TYPE,QPM_PARAMETER)]
        dt[,Last_7_Mean := mean(Mean[RANK[1:7]],na.rm = T), by =  .(SUPPLIER_NAME,PART_NUM,DATA_SOURCE_TYPE,QPM_PARAMETER) ]
        cpk_dt_metal = dt[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER",xname,"Mean","Std","RANK","GROUP_DATETIME"), with = F]
        rmlist = names(dt)
        
        raw_dt_Metal = dt[RANK ==1]
        gc(dt)
        spec = SPEC
        spec = as.data.table(spec)
        
        if(nrow(spec) > 0) {
          spec = spec[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,Target,USL,LSL)]
          y =SPEC_2
          raw_dt_Metal$PART_NUM = as.character(raw_dt_Metal$PART_NUM)
          spec$PART_NUM = as.character(spec$PART_NUM)
          collist = unique(names(raw_dt_Metal)[!names(raw_dt_Metal) %in% c("RANK")])
          raw_dt_Metal = raw_dt_Metal[,collist, with =F]
          raw_dt_Metal = merge(raw_dt_Metal,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                               by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
          
          ## cal  cpk
          raw_dt_Metal$Target = as.numeric(raw_dt_Metal$Target)
          raw_dt_Metal$USL = as.numeric(raw_dt_Metal$USL)
          raw_dt_Metal$LSL = as.numeric(raw_dt_Metal$LSL)
          
          cpks = numeric(nrow(raw_dt_Metal))
          
          for(i in 1:nrow(raw_dt_Metal)){
            Usl = raw_dt_Metal$USL[i]
            Lsl = raw_dt_Metal$LSL[i]
            Mean = raw_dt_Metal$Mean[i]
            Std = raw_dt_Metal$Std[i]
            
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
          raw_dt_Metal$Cpk = cpks
          raw_dt_Metal$CAVITY_NUM <-NA
          
          ## create CPK moving DT
          cpk_dt_metal = merge(cpk_dt_metal,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                               by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
          cpk_dt_metal$Target = as.numeric(cpk_dt_metal$Target)
          cpk_dt_metal$USL = as.numeric(cpk_dt_metal$USL)
          cpk_dt_metal$LSL = as.numeric(cpk_dt_metal$LSL)
          cpk_moving = numeric(nrow(cpk_dt_metal))
          
          for(i in 1:nrow(cpk_dt_metal)){
            Usl = cpk_dt_metal$USL[i]
            Lsl = cpk_dt_metal$LSL[i]
            Mean = cpk_dt_metal$Mean[i]
            Std = cpk_dt_metal$Std[i]
            
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
          cpk_dt_metal$Cpk = cpk_moving
          cpk_dt_metal$CAVITY_NUM <-NA
          
        }
        raw_dt = rbind(raw_dt_Metal,raw_dt_Plastic)
        cpk_dt = rbind(cpk_dt_metal,cpk_dt_plas)
        
      }
    } else if(commodity =="MOTOR") {
      
      X = X[,c("SUPPLIER_NAME","PART_NUM","ETL_LOAD_DATE","GRP_YEAR_WEEK","DATA_SOURCE_TYPE",xname,col), with = F]
      setkeyv(X, c("SUPPLIER_NAME", "PART_NUM","ETL_LOAD_DATE","GRP_YEAR_WEEK","DATA_SOURCE_TYPE",xname))
      X = melt(X , measure.vars = col , value.name="VALUE", variable.name="QPM_PARAMETER")
      X = X[!is.na(X$VALUE),]
      
      X = X[,NORMALIZE_VALUE := (VALUE- mean(VALUE, na.rm = T) )/sd(VALUE, na.rm = T) ,
            by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
      
      dt = X[,list(N = .N, Mean = mean(VALUE, na.rm = T), Std = sd(VALUE ,na.rm = T), Median = median(VALUE ,na.rm = T)
                   , ETL_LOAD_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)] , YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)]),
             by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname)]
      
      dt = setorderv(dt,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","YEAR_WEEK",xname),c(1,1,1,1,-1,-1))
      dt = dt[!(is.na(dt$Mean)),]
      dt = dt[,RANK := 1:.N ,by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
      ##create Mean of last 7 point
      dt = dt[,Last_7_Mean := mean(Mean[RANK[1:7]],na.rm = T), by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
      cpk_dt = dt[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname,"Mean","Std","RANK","ETL_LOAD_DATE"), with = F]
      rmlist = names(dt)
      raw_dt = dt[RANK ==1]
      gc(dt)
      spec = SPEC
      spec = as.data.table(spec)
      
      if(nrow(spec) > 0) {
        
        spec = spec[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,Target,USL,LSL)]
        y =SPEC_2
        raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
        spec$PART_NUM = as.character(spec$PART_NUM)
        collist =rmlist[!(rmlist %in% c("RANK"))]
        raw_dt = raw_dt[,collist, with =F]
        rm_dup = unique(colnames(raw_dt))
        raw_dt = raw_dt[,rm_dup, with = F]
        raw_dt = merge(raw_dt,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                       by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
        
        ## cal  cpk
        raw_dt$Target = as.numeric(raw_dt$Target)
        raw_dt$USL = as.numeric(raw_dt$USL)
        raw_dt$LSL = as.numeric(raw_dt$LSL)
        
        cpks = numeric(nrow(raw_dt))
        
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
                       by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), all.x = T )
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
        
      }
      
    } else {
      
      print("1")
      
      setkeyv(X, c("SUPPLIER_NAME", "PART_NUM","DATA_SOURCE_TYPE","ETL_LOAD_DATE","GRP_YEAR_WEEK",xname))
      X = suppressWarnings(melt(X , measure.vars = col , value.name="VALUE", variable.name="QPM_PARAMETER"))
      X = X[!is.na(X$VALUE),]
      
      X = X[,NORMALIZE_VALUE := (VALUE- mean(VALUE, na.rm = T) )/sd(VALUE, na.rm = T) ,
            by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" )]
      
      print("2")
      
      dt = X[,list(N = .N, Mean = mean(VALUE, na.rm = T), Std = sd(VALUE ,na.rm = T), Median = median(VALUE ,na.rm = T)
                   , ETL_LOAD_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)] 
                   , YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)], GROUP_DATETIME =max(GROUP_DATETIME)),
             by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER",xname)]
      
      gc(X)
      
      dt = setorderv(dt, c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER","GROUP_DATETIME", xname),c(1,1,1,1,-1,-1))
      dt = dt[!(is.na(dt$Mean)),]
      dt[,RANK := 1:.N ,by = .(SUPPLIER_NAME,PART_NUM,DATA_SOURCE_TYPE,QPM_PARAMETER)]
      dt[,Last_7_Mean := mean(Mean[RANK[1:7]],na.rm = T), by = .(SUPPLIER_NAME,PART_NUM,DATA_SOURCE_TYPE,QPM_PARAMETER) ]
      cpk_dt = dt[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname,"Mean","Std","RANK","GROUP_DATETIME"), with = F]
      
      print("3")
      
      rmlist = names(dt)
      raw_dt = dt[RANK ==1]
      gc(dt)
      gc()
      spec = SPEC
      
      spec = as.data.table(spec)
      
      print("Check Spec")
      
      if(nrow(spec) > 0) {
        
        spec = spec[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,Target,USL,LSL)]
        y =SPEC_2
        raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
        spec$PART_NUM = as.character(spec$PART_NUM)
        collist =rmlist[!(rmlist %in% c("RANK"))]
        raw_dt = raw_dt[,collist, with =F]
        rm_dup = unique(colnames(raw_dt))
        raw_dt = raw_dt[,rm_dup, with = F]
        raw_dt = merge(raw_dt, spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                       by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER" ), all.x = T )
        
        ## cal  cpk
        raw_dt$Target = as.numeric(raw_dt$Target)
        raw_dt$USL = as.numeric(raw_dt$USL)
        raw_dt$LSL = as.numeric(raw_dt$LSL)
        
        cpks = numeric(nrow(raw_dt))
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
        
        cpk_dt = merge(cpk_dt,spec, by.x = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), 
                       by.y = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), all.x = T )
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
      }
      
    }
    
    names(raw_dt)[grep('QPM_PARAMETER',names(raw_dt))] <-"Parameter"
    raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
    # raw_dt = merge(raw_dt,OOC_2,by=c("SUPPLIER_NAME","PART_NUM","Parameter"),all.x=TRUE)#)
    raw_dt$COMMODITY = NULL
    if("CHART_KEY" %in% names(raw_dt)) raw_dt[,"CHART_KEY"] = NULL
    H = merge(raw_dt, CHART_KEY, by =c("SUPPLIER_NAME","PART_NUM","Parameter"), all.x=TRUE)
    
    gc()
    
    if(commodity == "RAMP") {
      
      ### cal CPk_Slope from CPK moving DT
      
      # cut Cannot calcualte from CPK
      raw_cpk_Slope = cpk_dt[(Cpk!=Inf & Cpk != -Inf)]
      raw_cpk_Slope[, Cnt := .N , by =c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM")]
      raw_cpk_Slope = raw_cpk_Slope[!is.na(Cpk) & RANK %in% (1:7) & Cnt >=7]
      final_cpk_Slope = raw_cpk_Slope[,list(Cpk_Slope=round(coef(lm(Cpk~RANK))[2],3)),
                                      by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM")]
      final_cpk_Slope$Cpk_Slope = final_cpk_Slope$Cpk_Slope * (-1)
      gc(raw_cpk_Slope)
      
      ## cal  Slope
      raw_Slope = X[,list( Normal_mean = mean(NORMALIZE_VALUE , na.rm = T),Mean = mean(VALUE , na.rm = T), Std = sd(VALUE,na.rm = T),
                           Max = max(VALUE ,na.rm = T ), Min = min(VALUE,na.rm = T),GROUP_DATETIME = max(GROUP_DATETIME))  ,
                    by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM",xname )]
      
      raw_Slope = raw_Slope[!(is.na(raw_Slope$Normal_mean)),]
      setkeyv(raw_Slope,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","MOLD_NUM","CAVITY_NUM",xname))
      raw_Slope  = setorderv(raw_Slope,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","MOLD_NUM","GROUP_DATETIME"),c(1,1,1,1,1,1,-1))
      raw_Slope = raw_Slope[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,CAVITY_NUM,MOLD_NUM)]
      raw_Slope = raw_Slope[,relabel := ifelse(RANK ==1 , "last", "prevoius"),by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","MOLD_NUM" ) ]
      raw_Slope = raw_Slope[,N :=max(RANK) ,by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","MOLD_NUM" ) ]
      raw_Slope = raw_Slope[N >=7 & RANK %in% c(1,2,3,4,5,6,7)]
      
      if(nrow(raw_Slope) >0) {
        final_Slope = raw_Slope[,list(Slope=coef(lm(Normal_mean~RANK))[2]),
                                by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM")]
        gc(raw_Slope)
        final_Slope$Slope = abs(round(final_Slope$Slope,3))
      } else {
        final_Slope = unique(raw_Slope[,.(SUPPLIER_NAME,PART_NUM,DATA_SOURCE_TYPE,QPM_PARAMETER)])
        gc(raw_Slope)
        final_Slope$Slope <- NA
      }
      
      if(test_type==1) {
        ############ T.test cal p.value :: Robust welth P
        setkeyv(X,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM",xname))
        X =  X[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM",xname,"GROUP_DATETIME", "VALUE"), with= F]
        
        X  = setorderv(X,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","MOLD_NUM","GROUP_DATETIME"),c(1,1,1,1,1,1,-1))
        Index_A = unique(X[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","MOLD_NUM",xname),  with = F])
        Index_A = Index_A[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,DATA_SOURCE_TYPE,QPM_PARAMETER,CAVITY_NUM,MOLD_NUM)]
        
        Index_A = Index_A[,relabel := ifelse(RANK ==1 , "last", "prevoius"),
                          by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","MOLD_NUM")]
        
        X = merge(X, Index_A, by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","MOLD_NUM",xname), all.x = T)
        gc(Index_A)
        final_t_test = X[,list(p =    tryCatch({round(t.test(VALUE~relabel)$p.value,3)} , error  = function(e){404})) , 
                         by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,MOLD_NUM,CAVITY_NUM)]
        
        final_t_test =  final_t_test[!(p %in% c(404,NA)),]
        
        final_Diff_between = X[ , list(Mean = mean(VALUE , na.rm = T)) ,  
                                by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,MOLD_NUM,CAVITY_NUM,relabel)]
        
        Std_between = X[ , list(Std_all = sd(VALUE , na.rm = T)),  
                         by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,MOLD_NUM,CAVITY_NUM)]
        final_Diff_between = merge(final_Diff_between,Std_between , by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM"))
        
        
        final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
        final_Diff_between =  dcast(final_Diff_between, SUPPLIER_NAME+PART_NUM+QPM_PARAMETER+DATA_SOURCE_TYPE+MOLD_NUM+CAVITY_NUM+Std_all ~ relabel ,  value.var = "Mean")
        final_Diff_between$Diff <- round(abs(final_Diff_between$last - final_Diff_between$prevoius)/final_Diff_between$Std_all,3)
        final_Diff_between = final_Diff_between[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM","Diff")]
        final_Diff_between = final_Diff_between[!(is.na(final_Diff_between$Diff)),]
        
        if(nrow(final_Slope)>0) {
          final_t_test = merge(final_t_test,final_Diff_between,
                               by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM"), all = T)
          final_t_test = merge(final_t_test,final_cpk_Slope,
                               by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM"), all = T)
          final_result = merge(final_t_test,final_Slope,
                               by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM"), all = T)
        } else {
          final_result = merge(final_t_test,final_Diff_between, 
                               by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM"), all = T)
          final_result$Slope <- NA
        }
        # final_result <<- final_result
        
      } else {
        #P.value by anova
        final_anova = X[!(is.na(X$VALUE)),]
        final_anova[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,CAVITY_NUM,MOLD_NUM)]
        final_anova = final_anova[,list( p = tryCatch({round( summary(aov(VALUE~RANK))[[1]][["Pr(>F)"]],3)} ,
                                                      error  = function(e){404})) ,  
                                  by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,MOLD_NUM,CAVITY_NUM)]
        final_anova = final_anova[!(p %in% c(404,NA)),]
        
        #Diff by anova
        final_Diff_between = X[ , list(Mean = mean(VALUE , na.rm = T) ,  Std = sd(VALUE, na.rm = T)) ,  
                                by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM",xname)]
        final_Diff_between = final_Diff_between[,list(Max = max(Mean, na.rm = T), Min = min(Mean,na.rm =  T), std_mean = mean(Std, na.rm = T))
                                                , by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,MOLD_NUM,CAVITY_NUM)]
        final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
        final_Diff_between$Diff <- round((final_Diff_between$Max - final_Diff_between$Min)/final_Diff_between$std_mean,3)
        final_Diff_between = final_Diff_between[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,MOLD_NUM,CAVITY_NUM,Diff)]
        
        #merge result  
        if(nrow(final_Slope)>0) {
          final_anova = merge(final_anova,final_Diff_between, 
                              by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM"), all = T)
          
          final_anova = merge(final_anova,final_cpk_Slope,
                              by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM"), all = T)
          
          final_result = merge(final_Slope,final_anova,  
                               by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM"), all = T)
        }
        else {
          final_result = merge(final_anova,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM"), all = T)
          final_result$Slope <- NA
        }
      }
    } else if(commodity == "DSP") {
      if(nrow(X_Plastic) >0) {
        
        raw_cpk_Slope_plas = cpk_dt_plas[(Cpk!=Inf & Cpk!=-Inf)]
        raw_cpk_Slope_plas[, Cnt := .N , by =c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM")]
        raw_cpk_Slope_plas = raw_cpk_Slope_plas[!is.na(Cpk) & RANK %in% (1:7) & Cnt >=7]
        final_cpk_Slope_plas = raw_cpk_Slope_plas[,list(Cpk_Slope=round(coef(lm(Cpk~RANK))[2],3)),
                                                  by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM")]
        #Assign Value to  Cal
        X = X_Plastic
        #calcalte Slope
        raw_Slope = X[,list( Normal_mean = mean(NORMALIZE_VALUE , na.rm = T),Mean = mean(VALUE , na.rm = T)
                             ,Std = sd(VALUE,na.rm = T),Max = max(VALUE ,na.rm = T )
                             ,Min = min(VALUE,na.rm = T),GROUP_DATETIME = max(GROUP_DATETIME))  ,
                      by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM", xname )]
        
        raw_Slope = raw_Slope[!(is.na(raw_Slope$Normal_mean)),]
        setkeyv(raw_Slope,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","CAVITY_NUM",xname))
        raw_Slope  = setorderv(raw_Slope,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","GROUP_DATETIME"),c(1,1,1,1,1,-1))
        raw_Slope = raw_Slope[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,CAVITY_NUM)]
        # raw_Slope <<- raw_Slope
        raw_Slope = raw_Slope[,relabel := ifelse(RANK ==1 , "last", "prevoius"),by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM" ) ]
        raw_Slope = raw_Slope[,N :=max(RANK) ,by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM" ) ]
        raw_Slope = raw_Slope[N >=7 & RANK %in% c(1,2,3,4,5,6,7)]
        
        if(nrow(raw_Slope) >0) {
          final_Slope = raw_Slope[,list(Slope=coef(lm(Normal_mean~RANK))[2]),
                                  by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM")]
          final_Slope$Slope = abs(round(final_Slope$Slope,3))
        } else {
          final_Slope = unique(raw_Slope[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,CAVITY_NUM)])
          final_Slope$Slope <- NA
        }
        
        if(test_type==1) {
          ############ T.test cal p.value :: Robust welth P
          setkeyv(X,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname))
          X  = setorderv(X,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","GROUP_DATETIME"),c(1,1,1,1,1,-1))
          Index_A = unique(X[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM",xname),  with = F])
          Index_A = Index_A[, RANK := 1:.N, by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,CAVITY_NUM)]
          Index_A = Index_A[,relabel := ifelse(RANK ==1 , "last", "prevoius"),by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM") ]
          X = merge(X, Index_A,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM",xname), all.x = T)
          
          final_t_test = X[,list(p =    tryCatch({round(t.test(VALUE~relabel)$p.value,3)} , error  = function(e){404})) , 
                           by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,CAVITY_NUM)]
          
          final_t_test =  final_t_test[!(p ==404),]
          final_Diff_between = X[ , list(Mean = mean(VALUE , na.rm = T)) ,  
                                  by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,CAVITY_NUM,relabel)]
          # final <<- final_Diff_between
          
          Std_between = X[ , list(Std_all = sd(VALUE , na.rm = T)),  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM")]
          final_Diff_between = merge(final_Diff_between,Std_between , by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM"))
          # Diff_between <<- final_Diff_between
          
          final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
          final_Diff_between =  dcast(final_Diff_between, SUPPLIER_NAME+PART_NUM+QPM_PARAMETER+DATA_SOURCE_TYPE+CAVITY_NUM+Std_all ~ relabel ,  value.var = "Mean")
          final_Diff_between$Diff <- round(abs(final_Diff_between$last - final_Diff_between$prevoius)/final_Diff_between$Std_all,3)
          final_Diff_between = final_Diff_between[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","Diff")]
          final_Diff_between = final_Diff_between[!(is.na(final_Diff_between$Diff)),]
          # Diff_between <<- final_Diff_between
          if(nrow(final_Slope)>0) {
            final_t_test = merge(final_t_test,final_Diff_between,  
                                 by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM"), all = T)
            
            final_t_test = merge(final_t_test,final_cpk_Slope_plas , 
                                 by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM"), all = T)  
            
            final_result = merge(final_t_test,final_Slope,  
                                 by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM"), all = T)
          } else {
            final_result = merge(final_t_test,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM"), all = T)
            final_result$Slope <- NA
          }
          
        } else {
          #P.value by anova
          final_anova = X[!(is.na(X$VALUE)),]
          final_anova[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,CAVITY_NUM)]
          final_anova = final_anova[,list( p = tryCatch({round( summary(aov(VALUE~RANK))[[1]][["Pr(>F)"]],3)} ,
                                                        error  = function(e){404})) ,  
                                    by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,CAVITY_NUM)]
          
          final_anova = final_anova[!(p %in% c(404,NA)),]
          #Diff by anova
          final_Diff_between = X[ , list(Mean = mean(VALUE , na.rm = T) ,  Std = sd(VALUE, na.rm = T)) ,  
                                  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM",xname)]
          
          final_Diff_between = final_Diff_between[,list(Max = max(Mean, na.rm = T), Min = min(Mean,na.rm =  T), std_mean = mean(Std, na.rm = T)),
                                                  by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER","CAVITY_NUM")]
          
          final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
          final_Diff_between$Diff <- round((final_Diff_between$Max - final_Diff_between$Min)/final_Diff_between$std_mean,3)
          final_Diff_between = final_Diff_between[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,CAVITY_NUM,Diff)]
          # final_Diff <<- final_Diff_between
          
          if(nrow(final_Slope)>0) {
            final_anova = merge(final_anova,final_Diff_between,
                                by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM"), all = T)
            final_anova = merge(final_anova,final_cpk_Slope_plas,
                                by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM"), all = T)
            
            final_result = merge(final_Slope,final_anova,
                                 by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM"), all = T)
          } else {
            final_result = merge(final_anova,final_Diff_between,
                                 by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM"), all = T)
            final_result$Slope <- NA
          }
          
        }
        ## Assign final result  
        P_final_result = final_result  
        
      }
      
      if(nrow(X_Metal) >0) {
        
        raw_cpk_Slope_metal = cpk_dt_metal[(Cpk!=Inf & Cpk!=-Inf)]
        raw_cpk_Slope_metal[, Cnt := .N , by =c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
        raw_cpk_Slope_metal = raw_cpk_Slope_metal[!is.na(Cpk) & RANK %in% (1:7) & Cnt >=7]
        final_cpk_Slope_metal = raw_cpk_Slope_metal[,list(Cpk_Slope=round(coef(lm(Cpk~RANK))[2],3)),
                                                    by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
        
        X = X_Metal
        #calcalte Slope
        raw_Slope = X[,list( Normal_mean = mean(NORMALIZE_VALUE , na.rm = T),Mean = mean(VALUE , na.rm = T), Std = sd(VALUE,na.rm = T),
                             Max = max(VALUE ,na.rm = T ), Min = min(VALUE,na.rm = T),GROUP_DATETIME = max(GROUP_DATETIME))  ,
                      by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname )]
        
        raw_Slope = raw_Slope[!(is.na(raw_Slope$Normal_mean)),]
        setkeyv(raw_Slope,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER",xname))
        raw_Slope  = setorderv(raw_Slope,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","GROUP_DATETIME"),c(1,1,1,1,-1))
        raw_Slope = raw_Slope[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        # raw_Slope <<- raw_Slope
        raw_Slope = raw_Slope[,relabel := ifelse(RANK ==1 , "last", "prevoius"),by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
        raw_Slope = raw_Slope[,N :=max(RANK) ,by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
        raw_Slope = raw_Slope[N >=7 & RANK %in% c(1,2,3,4,5,6,7)]
        
        if(nrow(raw_Slope) >0) {
          final_Slope = raw_Slope[,list(Slope=coef(lm(Normal_mean~RANK))[2]),by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
          final_Slope$Slope = abs(round(final_Slope$Slope,3))
        } else {
          final_Slope = unique(raw_Slope[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)])
          final_Slope$Slope <- NA
        }
        
        if(test_type==1) {
          ############ T.test cal p.value :: Robust welth P
          setkeyv(X,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname))
          X  = setorderv(X,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","CAVITY_NUM","GROUP_DATETIME"),c(1,1,1,1,1,-1))
          Index_A = unique(X[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname),  with = F])
          Index_A = Index_A[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
          Index_A = Index_A[,relabel := ifelse(RANK ==1 , "last", "prevoius"),by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE") ]
          X = merge(X, Index_A,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname), all.x = T)
          
          final_t_test = X[,list(p =    tryCatch({round(t.test(VALUE~relabel)$p.value,3)} , error  = function(e){404})) , 
                           by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
          
          final_t_test =  final_t_test[!(p ==404),]
          final_Diff_between = X[ , list(Mean = mean(VALUE , na.rm = T)) ,  
                                  by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,relabel)]
          # final <<- final_Diff_between
          
          Std_between = X[ , list(Std_all = sd(VALUE , na.rm = T)),  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
          final_Diff_between = merge(final_Diff_between,Std_between , by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"))
          # Diff_between <<- final_Diff_between
          
          final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
          final_Diff_between =  dcast(final_Diff_between, SUPPLIER_NAME+PART_NUM+QPM_PARAMETER+DATA_SOURCE_TYPE+Std_all ~ relabel ,  value.var = "Mean")
          final_Diff_between$Diff <- round(abs(final_Diff_between$last - final_Diff_between$prevoius)/final_Diff_between$Std_all,3)
          final_Diff_between = final_Diff_between[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","Diff")]
          final_Diff_between = final_Diff_between[!(is.na(final_Diff_between$Diff)),]
          
          # Diff_between <<- final_Diff_between
          if(nrow(final_Slope)>0) {
            final_t_test = merge(final_t_test,final_Diff_between, by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_t_test = merge(final_t_test,final_cpk_Slope_metal, by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result = merge(final_t_test,final_Slope, by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          } else {
            final_result = merge(final_t_test,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result$Slope <- NA
          }
          
        } else {
          #P.value by anova
          final_anova = X[!(is.na(X$VALUE)),]
          final_anova[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
          final_anova = final_anova[,list( p = tryCatch({round( summary(aov(VALUE~RANK))[[1]][["Pr(>F)"]],3)} ,
                                                        error  = function(e){404})) ,  
                                    by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
          final_anova = final_anova[!(p %in% c(404,NA)),]
          #Diff by anova
          final_Diff_between = X[ , list(Mean = mean(VALUE , na.rm = T) ,  Std = sd(VALUE, na.rm = T)) ,  
                                  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname)]
          final_Diff_between = final_Diff_between[,list(Max = max(Mean, na.rm = T), Min = min(Mean,na.rm =  T), std_mean = mean(Std, na.rm = T))
                                                  , by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
          final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
          final_Diff_between$Diff <- round((final_Diff_between$Max - final_Diff_between$Min)/final_Diff_between$std_mean,3)
          final_Diff_between = final_Diff_between[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,Diff)]
          # final_Diff <<- final_Diff_between
          
          if(nrow(final_Slope)>0) {
            final_anova = merge(final_anova,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_anova = merge(final_anova,final_cpk_Slope_metal,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result = merge(final_Slope,final_anova,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          } else {
            final_result = merge(final_anova,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result$Slope <- NA
            
          }
          
        }
        final_result$CAVITY_NUM <-NA
        M_final_result = final_result  
      }
      final_result = rbind(P_final_result,M_final_result)
      
    } else if(commodity =="MOTOR") { 
      ### cal CPk_Slope from CPK moving DT
      raw_cpk_Slope = cpk_dt[(Cpk!=Inf & Cpk!=-Inf)]
      raw_cpk_Slope[, Cnt := .N , by =c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
      raw_cpk_Slope = raw_cpk_Slope[!is.na(Cpk) & RANK %in% (1:7) & Cnt >=7]
      final_cpk_Slope = raw_cpk_Slope[,list(Cpk_Slope=round(coef(lm(Cpk~RANK))[2],3)),
                                      by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER", "DATA_SOURCE_TYPE")]
      
      final_cpk_Slope$Cpk_Slope = final_cpk_Slope$Cpk_Slope * (-1)
      gc(raw_cpk_Slope)
      
      ## cal Slope ##
      raw_Slope = X[,list( Normal_mean = mean(NORMALIZE_VALUE , na.rm = T),Mean = mean(VALUE , na.rm = T), Std = sd(VALUE,na.rm = T),
                           Max = max(VALUE ,na.rm = T ), Min = min(VALUE,na.rm = T))  ,
                    by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE", xname )]
      
      raw_Slope = raw_Slope[!(is.na(raw_Slope$Normal_mean)),]
      setkeyv(raw_Slope,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname))
      raw_Slope  = setorderv(raw_Slope,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname),c(1,1,1,1,-1))
      
      raw_Slope = raw_Slope[, RANK := 1:.N,
                            by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
      
      raw_Slope = raw_Slope[,relabel := ifelse(RANK ==1 , "last", "prevoius"),
                            by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE") ]
      
      raw_Slope = raw_Slope[,N :=max(RANK) ,by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
      raw_Slope = raw_Slope[N >=7 & RANK %in% c(1:7)]
      
      if(nrow(raw_Slope) >0) {
        final_Slope = raw_Slope[,list(Slope=coef(lm(Normal_mean~RANK))[2]),
                                by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
        
        final_Slope$Slope = abs(round(final_Slope$Slope,3))
      } else {
        final_Slope = unique(raw_Slope[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)])
        final_Slope$Slope <- NA
      }
      
      if(test_type==1) {
        ############ T.test cal p.value :: Robust welth P
        setkeyv(X,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname))
        X  = setorderv(X,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname),c(1,1,1,1,-1))
        Index_A = unique(X[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname),  with = F])
        Index_A = Index_A[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        
        Index_A = Index_A[,relabel := ifelse(RANK ==1 , "last", "prevoius"),
                          by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
        
        X = merge(X, Index_A,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname), all.x = T)
        gc(Index_A)
        final_t_test = X[,list(p = tryCatch({round(t.test(VALUE~relabel)$p.value,3)} , error  = function(e){404})) , 
                         by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        
        final_t_test =  final_t_test[!(p ==404),]
        final_Diff_between = X[ , list(Mean = mean(VALUE , na.rm = T)) ,  
                                by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,relabel)]
        
        Std_between = X[ , list(Std_all = sd(VALUE , na.rm = T)),  by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        final_Diff_between = merge(final_Diff_between,Std_between , by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"))
        
        final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
        final_Diff_between =  dcast(final_Diff_between, SUPPLIER_NAME+PART_NUM+QPM_PARAMETER+DATA_SOURCE_TYPE+Std_all ~ relabel ,  value.var = "Mean")
        final_Diff_between$Diff <- round(abs(final_Diff_between$last - final_Diff_between$prevoius)/final_Diff_between$Std_all,3)
        final_Diff_between = final_Diff_between[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","Diff")]
        final_Diff_between = final_Diff_between[!(is.na(final_Diff_between$Diff)),]
        # Diff_between <<- final_Diff_between
        if(nrow(final_Slope)>0) {
          final_t_test = merge(final_t_test,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          final_t_test = merge(final_t_test,final_cpk_Slope ,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T) 
          final_result = merge(final_t_test,final_Slope,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          DT_mt <<-  final_result
        } else {
          final_result = merge(final_t_test,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          final_result$Slope <- NA
        }
        
        # final_result <<- final_result
        
      } else {
        #P.value by anova
        final_anova = X[!(is.na(X$VALUE)),]
        final_anova[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        final_anova = final_anova[,list( p = tryCatch({round( summary(aov(VALUE~RANK))[[1]][["Pr(>F)"]],3)} ,
                                                      error  = function(e){404})) ,  
                                  by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        final_anova = final_anova[!(p %in% c(404,NA)),]
        #Diff by anova
        final_Diff_between = X[ , list(Mean = mean(VALUE , na.rm = T) ,  Std = sd(VALUE, na.rm = T)) ,  
                                by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname)]
        final_Diff_between = final_Diff_between[,list(Max = max(Mean, na.rm = T), Min = min(Mean,na.rm =  T), std_mean = mean(Std, na.rm = T))
                                                , by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
        final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
        final_Diff_between$Diff <- round((final_Diff_between$Max - final_Diff_between$Min)/final_Diff_between$std_mean,3)
        final_Diff_between = final_Diff_between[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,Diff)]
        # final_Diff <<- final_Diff_between
        
        #merge result 
        if(nrow(final_Slope)>0) {
          final_anova = merge(final_anova,final_Diff_between, by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          final_anova = merge(final_anova,final_cpk_Slope, by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          final_result = merge(final_Slope,final_anova, by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
        } else {
          final_result = merge(final_anova,final_Diff_between, by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          final_result$Slope <- NA
        }
        
      }
      
    } else {
      
      if(c("Cpk") %in% names(H)) {
        raw_cpk_Slope = cpk_dt[(Cpk!=Inf & Cpk!=-Inf)]
        if(nrow(raw_cpk_Slope)>0) {
          raw_cpk_Slope[, Cnt := .N , by =c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
          raw_cpk_Slope = raw_cpk_Slope[!is.na(Cpk) & RANK %in% (1:7) & Cnt >=7]
          if (nrow(raw_cpk_Slope) > 0) {
            final_cpk_Slope = raw_cpk_Slope[,list(Cpk_Slope=round(coef(lm(Cpk~RANK))[2],3)),
                                            by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
          }else {
            final_cpk_Slope = unique(raw_cpk_Slope[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")])
            final_cpk_Slope$Cpk_Slope <- NA
          }
        } else {
          final_cpk_Slope = unique(raw_cpk_Slope[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")])
          final_cpk_Slope$Cpk_Slope <- NA
        }
        
      }
      
      #Calcalte all Commodity
      raw_Slope = X[,list( Normal_mean = mean(NORMALIZE_VALUE , na.rm = T),Mean = mean(VALUE , na.rm = T), Std = sd(VALUE,na.rm = T),
                           Max = max(VALUE ,na.rm = T ), Min = min(VALUE,na.rm = T),GROUP_DATETIME = max(GROUP_DATETIME))  ,
                    by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname )]
      raw_Slope = raw_Slope[!(is.na(raw_Slope$Normal_mean)),]
      setkeyv(raw_Slope,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname))
      # XX_SLOPE <<-  raw_Slope
      raw_Slope  = setorderv(raw_Slope,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","GROUP_DATETIME"),c(1,1,1,1,-1))
      raw_Slope = raw_Slope[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER)]
      raw_Slope = raw_Slope[,relabel := ifelse(RANK ==1 , "last", "prevoius"),by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","QPM_PARAMETER")]
      raw_Slope = raw_Slope[,N :=max(RANK) ,by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
      raw_Slope = raw_Slope[N >=7 & RANK %in% c(1,2,3,4,5,6,7)]
      # bf_Slope <<- raw_Slope
      print("Slope Cal")
      
      if(nrow(raw_Slope) >0) {
        final_Slope = raw_Slope[,list(Slope=coef(lm(Normal_mean~RANK))[2]),by=c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
        final_Slope$Slope = abs(round(final_Slope$Slope,3))
      } else {
        final_Slope = unique(raw_Slope[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)])
        final_Slope$Slope <- NA
      }
      
      if(test_type==1) {
        ############ T.test cal p.value :: Robust welth P
        setkeyv(X,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname))
        X  = setorderv(X,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","GROUP_DATETIME"),c(1,1,1,1,-1))
        Index_A = unique(X[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname),  with = F])
        Index_A = Index_A[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        Index_A = Index_A[,relabel := ifelse(RANK ==1 , "last", "prevoius"),by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
        X = merge(X, Index_A,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname), all.x = T)
        gc(Index_A)
        
        final_t_test = X[,list(p =    tryCatch({round(t.test(VALUE~relabel)$p.value,3)} , error  = function(e){404})) , 
                         by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        final_t_test =  final_t_test[!(p %in% c(404,NA)),]
        
        ########### standard within group all Commodity ###############
        final_Diff_between = X[ , list(Mean = mean(VALUE , na.rm = T)) ,  
                                by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,relabel)]
        # final <<- final_Diff_between
        
        Std_between = X[ , list(Std_all = sd(VALUE , na.rm = T)),  by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        final_Diff_between = merge(final_Diff_between,Std_between , by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"))
        # Diff_between <<- final_Diff_between
        
        final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
        final_Diff_between =  dcast(final_Diff_between, SUPPLIER_NAME+PART_NUM+QPM_PARAMETER+DATA_SOURCE_TYPE+Std_all ~ relabel ,  value.var = "Mean")
        final_Diff_between$Diff <- round(abs(final_Diff_between$last - final_Diff_between$prevoius)/final_Diff_between$Std_all,3)
        final_Diff_between = final_Diff_between[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE","Diff")]
        final_Diff_between = final_Diff_between[!(is.na(final_Diff_between$Diff)),]
        
        if(c("Cpk") %in% names(H)) {
          if(nrow(final_Slope)>0) {
            final_t_test = merge(final_t_test,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_t_test = merge(final_t_test,final_cpk_Slope,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result = merge(final_t_test,final_Slope,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          } else {
            final_t_test = merge(final_t_test,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result = merge(final_t_test,final_Slope,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          }
          
        } else {
          if(nrow(final_Slope)>0) {
            final_t_test = merge(final_t_test,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result = merge(final_t_test,final_Slope,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          } else {
            final_result = merge(final_t_test,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result$Slope <- NA
          }
          
        }
        
      } else {
        #P.value by anova
        final_anova = X[!(is.na(X$VALUE)),]
        final_anova[, RANK := 1:.N,by = list(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        final_anova = final_anova[,list( p = tryCatch({round( summary(aov(VALUE~RANK))[[1]][["Pr(>F)"]],3)} ,
                                                      error  = function(e){404})) ,  
                                  by = .(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE)]
        final_anova = final_anova[!(p ==404),]
        #Diff by anova
        final_Diff_between = X[ , list(Mean = mean(VALUE , na.rm = T) ,  Std = sd(VALUE, na.rm = T)) ,  
                                by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE",xname)]
        final_Diff_between = final_Diff_between[,list(Max = max(Mean, na.rm = T), Min = min(Mean,na.rm =  T), std_mean = mean(Std, na.rm = T))
                                                , by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE")]
        final_Diff_between = final_Diff_between[complete.cases(final_Diff_between),]
        final_Diff_between$Diff <- round((final_Diff_between$Max - final_Diff_between$Min)/final_Diff_between$std_mean,3)
        final_Diff_between = final_Diff_between[,.(SUPPLIER_NAME,PART_NUM,QPM_PARAMETER,DATA_SOURCE_TYPE,Diff)]
        
        #merge result 
        
        if(c("Cpk") %in% names(H)) {
          if(nrow(final_Slope)>0) {
            final_anova = merge(final_anova,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_anova = merge(final_anova,final_cpk_Slope , by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result = merge(final_Slope,final_anova,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          } else {
            final_anova = merge(final_anova,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result = merge(final_Slope,final_anova,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
          }
          
        }
        
        else {
          if(nrow(final_Slope)>0) {
            final_anova = merge(final_anova,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result = merge(final_anova,final_Slope,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER"), all = T)
          } else {
            final_result = merge(final_anova,final_Diff_between,  by = c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","DATA_SOURCE_TYPE"), all = T)
            final_result$Slope <- NA
          }
          
        }
        
      }
    }  
    # })
    
    gc(X)
    # HDT <<- H
    # F_result <<-final_result
    
    names(final_result)[grep('QPM_PARAMETER',names(final_result))]  <- "Parameter"
    if(commodity == "RAMP") {
      H = merge(H,final_result, by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter","MOLD_NUM","CAVITY_NUM"), all.x = T)
    } else if(commodity == "DSP") {
      H = merge(H,final_result, by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter","CAVITY_NUM"), all.x = T)
      H = merge(H,check_dsp_metal, by = c("PART_NUM"))
      names(H)[grep('Remark',names(H))]<-"Material_type"
    } else if(commodity == "HSA_GRAMLOAD") {
      H = merge(H,final_result, by = c("SUPPLIER_NAME","PART_NUM","SBR_NUM","Parameter","GRMLD_CELL_ID","GRMLD_HEAD_PSN"), all.x = T) 
    } else if(commodity == "HSA_MARPOSS") {
      H = merge(H,final_result, by = c("SUPPLIER_NAME","PART_NUM","SBR_NUM","Parameter","HEAD_PSN","STKEVT_CELL_ID"), all.x = T) 
    } else if(commodity == "MOTOR") {
      H = merge(H,final_result, by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter"), all.x = T)
    } else {
      H = merge(H,final_result, by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter"), all.x = T)
    }
    
    # H$EDP[H$EDP=="Y"] = "Yes"
    H$COMMODITY = commodity
    H$REPORT_DATE = Sys.Date()
    I = H
    # I$EDP = rep("",nrow(I))
    
    CTQ_spec = readRDS(file.path(QPM_SPEC_folder,"QPM_SPEC.rda"))
    CTQ_spec = CTQ_spec[,c("SUPPLIER_NAME","PART_NUM","QPM_PARAMETER","Key_CTQ")]
    names(CTQ_spec)[3] <- "Parameter"
    names(CTQ_spec)[4] <- "CTQ"
    
    CTQ_spec$CTQ[CTQ_spec$CTQ %in% c("YES","Y","y","Yes","yes")] <-"CTQ"
    CTQ_spec$CTQ[CTQ_spec$CTQ %in% c("NO","N","n","No","no",NA)] <-NA
    I$PART_NUM = as.character(I$PART_NUM)
    CTQ_spec$PART_NUM = as.character(CTQ_spec$PART_NUM)
    LIST = I$PART_NUM
    df_null_pn  = subset(I,!(I$PART_NUM %in% LIST))
    
    I$Parameter <- as.character(I$Parameter)
    I$SUPPLIER_NAME <- as.character(I$SUPPLIER_NAME)
    
    CTQ_spec$SUPPLIER_NAME <- as.character(CTQ_spec$SUPPLIER_NAME)
    
    I = left_join(I,CTQ_spec, by = c("SUPPLIER_NAME","PART_NUM","Parameter"))
    gc(CTQ_spec)
    
    # Ipd <<-  I 
    if(nrow(I)>0) {
      I$Trigger <- NA
    }
    
    if(nrow(df_null_pn) >0) {
      df_null_pn$Trigger <- NA
      df_null_pn$CTQ <- NA
      I = rbind(I,df_null_pn)
    }
    
    I = as.data.frame(I)
    I = merge(I,dic_product , by = c("PART_NUM"), all.x = T)
    temp_null_product = I[is.na(I$PRODUCT_NAME),]
    temp_null_product = unique(temp_null_product)
    temp_product = I[!is.na(I$PRODUCT_NAME),]
    temp_product = unique(temp_product)
    
    if(nrow(temp_product) > 0) {
      pro_name = unique(temp_product[,c("PART_NUM", "SUPPLIER_NAME", "PRODUCT_NAME")])
      pro_name = pro_name[order(pro_name$PRODUCT_NAME),]
      pro_name = pro_name %>% group_by(PART_NUM, SUPPLIER_NAME) %>% dplyr::summarise(PRODUCT_NAME = stringr::str_flatten(PRODUCT_NAME,collapse = ", "))
      temp_product = temp_product[,!(names(temp_product) %in% "PRODUCT_NAME")]
      temp_product = left_join(temp_product, pro_name, by = c("PART_NUM", "SUPPLIER_NAME"))
    }
    
    setDT(temp_product)
    
    I = rbind(temp_product,temp_null_product)
    gc(temp_product)
    gc(temp_null_product)
    
    I = as.data.frame(I)
    names(I)[grep('PRODUCT_NAME',names(I))] <- "Product"
    trigger_index = trigger_index[!is.na(TRIGGER_LEVEL),c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER","TRIGGER_LEVEL")]
    trigger_index$PART_NUM <- as.character(trigger_index$PART_NUM)
    trigger_index$TRIGGER_LEVEL <- as.character(trigger_index$TRIGGER_LEVEL)
    setDT(I)
    I = I[,!c("Trigger")]
    
    ###### Enable Trigger ##########
    
    trigger_index_col = c("Cpk","Diff","Slope")
    col = names(I)
    trigger_index_col = col[col %in% trigger_index_col]
    
    if(commodity %in% c("HSA_MARPOSS")) {
      I$Trigger = NA
      I = I[,c("SUPPLIER_NAME","PART_NUM","SBR_NUM","Parameter","HEAD_PSN","STKEVT_CELL_ID","GRP_YEAR_WEEK","N",             
               "Mean","Std","Median","ETL_LOAD_DATE","YEAR_WEEK","GROUP_DATETIME",          
               "CHART_KEY","p","Diff","Slope","Trigger","COMMODITY","REPORT_DATE","CTQ")]
    } else if (commodity %in% c("HSA_GRAMLOAD")) {
      I$Trigger = NA
      I = I[,c("SUPPLIER_NAME","PART_NUM","SBR_NUM","Parameter","GRMLD_CELL_ID","GRMLD_HEAD_PSN","GRP_YEAR_WEEK","N",             
               "Mean","Std","Median","ETL_LOAD_DATE","YEAR_WEEK","GROUP_DATETIME",         
               "CHART_KEY","p","Diff","Slope","Trigger","COMMODITY","REPORT_DATE","CTQ")]
    } else if (commodity %in% c("RAMP")) {
      I_RAMP <<-  I
      I_CTQ = I[CTQ == "CTQ" & !is.na(p) & !is.na(Slope) ,c("PART_NUM","SUPPLIER_NAME","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM","Parameter","Cpk","Diff","Slope" )]
      I_CTQ = merge(I_CTQ,trigger_index, by.x = c("PART_NUM","SUPPLIER_NAME","Parameter"), by.y = c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                    all.x = T )
      I_CTQ = melt(I_CTQ , measure.vars = c("Cpk","Diff", "Slope") , value.name="index_value", variable.name="citeria" )
      
      I_CTQ$TRIGGER_LEVEL <- as.character(I_CTQ$TRIGGER_LEVEL)
      trigger_con$TRIGGER_LEVEL <- as.character(trigger_con$TRIGGER_LEVEL)
      
      I_CTQ = merge(I_CTQ,trigger_con,by.x = c("TRIGGER_LEVEL","citeria"),by.y = c("TRIGGER_LEVEL","Parameter"),all.x = T)
      I_CTQ[,Check_Con := ifelse(Equation == "Less",(index_value < VALUE),ifelse(Equation == "More",(index_value > VALUE),NA))]
      
      ### return column index for color highlight 
      # DT_Color = dcast(I_CTQ,SUPPLIER_NAME+PART_NUM+Parameter+MOLD_NUM+CAVITY_NUM ~ citeria ,  value.var = "Check_Con")
      # names(DT_Color)[names(DT_Color) %in% trigger_index_col] <- paste("color_",trigger_index_col,sep = "")
      
      I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM","Parameter","citeria","TRIGGER_LEVEL","index_value", "Equation", "VALUE","Check_Con")]
      I_CTQ = I_CTQ[,c( "SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM","Parameter","citeria","TRIGGER_LEVEL","Check_Con")]
      # remove NA in Condition Cause Disable condtion to trigger
      I_CTQ[is.na(Check_Con),"Check_Con"]<-FALSE
      I_CTQ[,Check_all := any(Check_Con),by = .(SUPPLIER_NAME,PART_NUM,DATA_SOURCE_TYPE,MOLD_NUM,CAVITY_NUM,Parameter) ]
      I_CTQ[,Trigger := ifelse(Check_all == TRUE,"YES","")]
      I_CTQ = unique(I_CTQ[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","MOLD_NUM","CAVITY_NUM","Parameter","TRIGGER_LEVEL","Trigger")])
      I = merge(I,I_CTQ,by = c("SUPPLIER_NAME","MOLD_NUM","DATA_SOURCE_TYPE","CAVITY_NUM","PART_NUM","Parameter"),all.x = T)
      DT_result <<- I
      I[Trigger %in% c(NA,""),"Trigger"] <-NA
      setDT(I)
      
      if("Cpk" %in% names(I)) {
        print("RAMP cpk ")
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Last_7_Mean","Std", "Cpk","Cpk_Slope", "p", "Diff", "Slope",
                 "CAVITY_NUM","MOLD_NUM","Trigger", xname, "ETL_LOAD_DATE","DATA_SOURCE_TYPE","YEAR_WEEK",
                 "CTQ", "Median", "Target", "USL", "LSL", "CHART_KEY", "COMMODITY", "REPORT_DATE", "Product","TRIGGER_LEVEL" ) , with = F]
      } else {
        print("RAMP no cpk ")
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std" , "p", "Diff", "Slope",
                 "CAVITY_NUM","MOLD_NUM","Trigger", xname, "ETL_LOAD_DATE","DATA_SOURCE_TYPE","YEAR_WEEK",
                 "CTQ", "Median",  "CHART_KEY", "COMMODITY", "REPORT_DATE", "Product","TRIGGER_LEVEL" ), with = F]
      }
      
    } else if(commodity == "DSP") {
      # PLASTIC = Include CAVITY for calculate
      # METAL = Skip CAVITY 
      I_Plastic = I[Material_type == "Plastic" & CTQ == "CTQ" & !is.na(p) & !is.na(Slope), c( "PART_NUM","SUPPLIER_NAME","DATA_SOURCE_TYPE","CAVITY_NUM","Parameter","Cpk","Diff","Slope" )]
      I_Metal = I[Material_type == "Metal" & CTQ == "CTQ" & !is.na(p) & !is.na(Slope), c( "PART_NUM","SUPPLIER_NAME","DATA_SOURCE_TYPE","Parameter","Cpk","Diff","Slope" )]
      
      if(nrow(I_Plastic)>0) {
        I_Plastic = merge(I_Plastic,trigger_index, by.x = c("PART_NUM","SUPPLIER_NAME","Parameter"), by.y = c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                          all.x = T )
        I_Plastic = melt(I_Plastic , measure.vars = c("Cpk","Diff", "Slope") , value.name="index_value", variable.name="citeria" )
        I_Plastic = merge(I_Plastic,trigger_con,by.x = c("TRIGGER_LEVEL","citeria"),by.y = c("TRIGGER_LEVEL","Parameter"),all.x = T)
        I_Plastic[,Check_Con := ifelse(Equation == "Less",(index_value < VALUE),ifelse(Equation == "More",(index_value > VALUE),NA))]
        # I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE,"MOLD_NUM","CAVITY_NUM","Parameter","citeria","TRIGGER_LEVEL","index_value", "Equation", "VALUE","Check_Con")]
        
        I_Plastic = I_Plastic[,c( "SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","CAVITY_NUM","Parameter","citeria","TRIGGER_LEVEL","Check_Con")]
        # remove NA in Condition Cause Disable condtion to trigger
        I_Plastic[is.na(Check_Con),"Check_Con"]<-FALSE
        I_Plastic[,Check_all := any(Check_Con),by = .(SUPPLIER_NAME,PART_NUM,CAVITY_NUM,Parameter) ]
        I_Plastic[,Trigger := ifelse(Check_all == TRUE,"YES","")]
        I_Plastic = unique(I_Plastic[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","CAVITY_NUM","Parameter","TRIGGER_LEVEL","Trigger")])
        filt_plastic =I[Material_type == "Plastic"]
        filt_plastic = merge(filt_plastic,I_Plastic,by = c("SUPPLIER_NAME","CAVITY_NUM","DATA_SOURCE_TYPE","PART_NUM","Parameter"),all.x = T)
        filt_plastic[Trigger %in% c(NA,""),"Trigger"] <-NA
      } 
      
      if(nrow(I_Metal)>0) {
        I_Metal = merge(I_Metal,trigger_index, by.x = c("PART_NUM","SUPPLIER_NAME","Parameter"), by.y = c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                        all.x = T )
        
        I_Metal = melt(I_Metal , measure.vars = c("Cpk","Diff", "Slope") , value.name="index_value", variable.name="citeria" )
        I_Metal = merge(I_Metal,trigger_con,by.x = c("TRIGGER_LEVEL","citeria"),by.y = c("TRIGGER_LEVEL","Parameter"),all.x = T)
        I_Metal[,Check_Con := ifelse(Equation == "Less",(index_value < VALUE),ifelse(Equation == "More",(index_value > VALUE),NA))]
        
        ### return column index for color highlight 
        # DT_Color = dcast(I_Metal,SUPPLIER_NAME+PART_NUM+Parameter ~ citeria ,  value.var = "Check_Con")
        # names(DT_Color)[names(DT_Color) %in% col] <- paste("color_",col,sep = "")
        
        # I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM","MOLD_NUM","CAVITY_NUM","Parameter","citeria","TRIGGER_LEVEL","index_value", "Equation", "VALUE","Check_Con")]
        
        I_Metal = I_Metal[,c( "SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter","citeria","TRIGGER_LEVEL","Check_Con")]
        # remove NA in Condition Cause Disable condtion to trigger
        I_Metal[is.na(Check_Con),"Check_Con"]<-FALSE
        I_Metal[,Check_all := any(Check_Con),by = .(SUPPLIER_NAME,PART_NUM,Parameter) ]
        I_Metal[,Trigger := ifelse(Check_all == TRUE,"YES","")]
        I_Metal = unique(I_Metal[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter","TRIGGER_LEVEL","Trigger")])
        filt_metal =I[Material_type == "Metal"]
        filt_metal = merge(filt_metal,I_Metal,by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter"),all.x = T)
        filt_metal[Trigger %in% c(NA,""),"Trigger"] <-NA
        
      }
      
      I = rbind(filt_metal,filt_plastic)
      ## check  trigger for plastics
      setDT(I)
      
      if(c("Cpk") %in% names(I)) {
        print("DSP cpk ")
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "CAVITY_NUM", "Material_type", "N", "Mean","Last_7_Mean", "Std", "Cpk","Cpk_Slope", "p", "Diff", "Slope",
                 xname, "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "YEAR_WEEK", "CTQ", "Median", "Target", "USL", "LSL",
                 "CHART_KEY", "COMMODITY", "REPORT_DATE", "Product","Trigger","TRIGGER_LEVEL"), with = F]
      } else { 
        print("dsp No cpk ")
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "CAVITY_NUM", "Material_type", "N", "Mean", "Std", "p", "Diff", "Slope",
                  xname, "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "YEAR_WEEK", "CTQ", "Median", "CHART_KEY",
                 "COMMODITY", "REPORT_DATE", "Product","Trigger"), with = F]
      }
      
    } else if (commodity == "MOTOR") {
      ## all commodity
      trigger_index_col = c("Cpk","Diff","Slope")
      col = names(I)
      trigger_index_col = col[col %in% trigger_index_col ]
      I_CTQ = I[CTQ == "CTQ"]
      if(nrow(I_CTQ)>0) {
        ## filter one group shoud not trigger
        I_CTQ = I_CTQ[!is.na(p) & !is.na(Slope) ,c( "PART_NUM","SUPPLIER_NAME","DATA_SOURCE_TYPE","Parameter",trigger_index_col), with = F]
        I_CTQ = merge(I_CTQ,trigger_index, by.x = c("PART_NUM","SUPPLIER_NAME","Parameter"), by.y = c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                      all.x = T )
        # I_CTQ = melt(I_CTQ , measure.vars = c("Cpk","Diff", "Slope") , value.name="index_value", variable.name="citeria" )
        I_CTQ = melt(I_CTQ , measure.vars = trigger_index_col , value.name="index_value", variable.name="citeria" )
        
        I_CTQ$TRIGGER_LEVEL <- as.character(I_CTQ$TRIGGER_LEVEL)
        trigger_con$TRIGGER_LEVEL <- as.character(trigger_con$TRIGGER_LEVEL)
        
        I_CTQ = merge(I_CTQ,trigger_con,by.x = c("TRIGGER_LEVEL","citeria"),by.y = c("TRIGGER_LEVEL","Parameter"),all.x = T)
        I_CTQ[,Check_Con := ifelse(Equation == "Less",(index_value < VALUE),ifelse(Equation == "More",(index_value > VALUE),NA))]
        
        I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter","citeria","TRIGGER_LEVEL","index_value", "Equation", "VALUE","Check_Con")]
        I_CTQ = I_CTQ[,c( "SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter","citeria","TRIGGER_LEVEL","Check_Con")]
        # remove NA in Condition Cause Disable condtion to trigger
        I_CTQ[is.na(Check_Con),"Check_Con"]<-FALSE
        I_CTQ[,Check_all := any(Check_Con),by = .(SUPPLIER_NAME,PART_NUM,Parameter) ]
        I_CTQ[,Trigger := ifelse(Check_all == TRUE,"YES",""),by = .(SUPPLIER_NAME,PART_NUM,Parameter)]
        I_CTQ = unique(I_CTQ[,c("SUPPLIER_NAME","PART_NUM","Parameter","DATA_SOURCE_TYPE","TRIGGER_LEVEL","Trigger")])
        I = merge(I,I_CTQ,by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter"),all.x = T)
        
        if (sum(I$TRIGGER_LEVEL == 2 & I$Cpk < 1.0 &I$ Diff < 1.0, na.rm =  TRUE) > 1) {
          
          I$Trigger <- as.character(I$Trigger)
          
          I <- I %>% mutate(Trigger = case_when(TRIGGER_LEVEL == 2 & Cpk < 1.0 & Diff > 1.0 ~ "YES",
                                                TRIGGER_LEVEL == 2 & Cpk < 1.0 & Diff < 1.0 ~ "NO",
                                                TRUE ~ Trigger))
          
        }
        
      } else {
        if (sum(is.na(I$TRIGGER_LEVEL)) > 0) {
          I[is.na(I$TRIGGER_LEVEL),]$TRIGGER_LEVEL <- ""
        }
      }
      
      I[I$Trigger %in% c(NA,"","NO"),"Trigger"] <-NA
      setDT(I)
      
      print(names(I))
      
      if("Cpk" %in% names(I)) {
        print("All Com  cpk")
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean","Last_7_Mean", "Std", "Cpk","Cpk_Slope", "p", "Diff", "Slope",
                 xname, "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "YEAR_WEEK", "CTQ", "Median", "Target", "USL", "LSL",
                 "CHART_KEY", "COMMODITY", "REPORT_DATE", "Product","Trigger","TRIGGER_LEVEL"), with = F]
      } else {
        print("All Com No cpk")
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std", "p", "Diff", "Slope",
                  xname, "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "YEAR_WEEK", "CTQ", "Median", "CHART_KEY",
                 "COMMODITY", "REPORT_DATE", "Product", "Trigger" ), with = F]
      }
      
    } else {
      ## all commodity
      trigger_index_col = c("Cpk","Diff","Slope")
      col = names(I)
      trigger_index_col = col[col %in% trigger_index_col ]
      I_CTQ = I[CTQ == "CTQ"]
      
      if(nrow(I_CTQ)>0) {
        ## filter one group shoud not trigger
        I_CTQ = I_CTQ[!is.na(p) & !is.na(Slope) ,c("PART_NUM","SUPPLIER_NAME","DATA_SOURCE_TYPE","Parameter",trigger_index_col), with = F]
        I_CTQ = merge(I_CTQ,trigger_index, by.x = c("PART_NUM","SUPPLIER_NAME","Parameter"), by.y = c("PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                      all.x = T )
        # I_CTQ = melt(I_CTQ , measure.vars = c("Cpk","Diff", "Slope") , value.name="index_value", variable.name="citeria" )
        I_CTQ = melt(I_CTQ , measure.vars = trigger_index_col , value.name="index_value", variable.name="citeria" )
        
        I_CTQ$TRIGGER_LEVEL <- as.character(I_CTQ$TRIGGER_LEVEL)
        trigger_con$TRIGGER_LEVEL <- as.character(trigger_con$TRIGGER_LEVEL)
        
        I_CTQ = merge(I_CTQ,trigger_con,by.x = c("TRIGGER_LEVEL","citeria"),by.y = c("TRIGGER_LEVEL","Parameter"),all.x = T)
        I_CTQ[,Check_Con := ifelse(Equation == "Less",(index_value < VALUE),ifelse(Equation == "More",(index_value > VALUE),NA))]
        
        ### return column index for color highlight 
        
        I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter","citeria","TRIGGER_LEVEL","index_value", "Equation", "VALUE","Check_Con")]
        I_CTQ = I_CTQ[,c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE","Parameter","citeria","TRIGGER_LEVEL","Check_Con")]
        # remove NA in Condition Cause Disable condtion to trigger
        I_CTQ[is.na(Check_Con),"Check_Con"]<-FALSE
        I_CTQ[,Check_all := any(Check_Con),by = .(SUPPLIER_NAME,PART_NUM,Parameter) ]
        I_CTQ[,Trigger := ifelse(Check_all == TRUE,"YES",""),by = .(SUPPLIER_NAME,PART_NUM,Parameter)]
        I_CTQ = unique(I_CTQ[,c("SUPPLIER_NAME","PART_NUM","Parameter","TRIGGER_LEVEL","Trigger")])
        I = merge(I,I_CTQ,by = c("SUPPLIER_NAME","PART_NUM","Parameter"),all.x = T)
      } else {
        I$Trigger <- NA
      }
      
      I[Trigger %in% c(NA,""),"Trigger"] <-NA
      setDT(I)
      
      trigger_checker = unique(I[is.na(I$TRIGGER_LEVEL), c("PART_NUM","SUPPLIER_NAME","Parameter")])
      
      names(trigger_checker) = c("PART_NUM", "SUPPLIER_NAME", "QPM_PARAMETER")
      
      trigger_checker = left_join(trigger_checker, trigger_index)
      names(trigger_checker) = c("PART_NUM", "SUPPLIER_NAME", "Parameter", "TRIGGER_LEVEL")
      trigger_checker = trigger_checker[!is.na(trigger_checker$TRIGGER_LEVEL),]
      
      if (sum(!is.na(trigger_checker$TRIGGER_LEVEL)) > 0) {
      
        for (var in unique(trigger_checker$TRIGGER_LEVEL)) {
          
          trigger_temp = trigger_checker[trigger_checker$TRIGGER_LEVEL == var, ]
          
          ind <- I$PART_NUM %in% trigger_temp$PART_NUM & I$SUPPLIER_NAME %in% trigger_temp$SUPPLIER_NAME & I$Parameter %in% trigger_temp$Parameter
          
          I[ind, "TRIGGER_LEVEL"] <- var
          
        }
        
      } else {
        
        I$TRIGGER_LEVEL <- ""
        
      }
      
      if("Cpk" %in% names(I)) {
        print("All Com  cpk")
        
        if("Cpk_Slope" %in% names(I)){
          
          I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean","Last_7_Mean", "Std", "Cpk","Cpk_Slope", "p", "Diff", "Slope",
                   xname, "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "YEAR_WEEK", "CTQ", "Median", "Target", "USL", "LSL",
                   "CHART_KEY", "COMMODITY", "REPORT_DATE", "Product", "Trigger", "TRIGGER_LEVEL"), with = F]
          
        } else {
          
          I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean","Last_7_Mean", "Std", "Cpk", "p", "Diff", "Slope",
                   xname, "ETL_LOAD_DATE", "DATA_SOURCE_TYPE","YEAR_WEEK", "CTQ", "Median", "Target", "USL", "LSL",
                   "CHART_KEY", "COMMODITY", "REPORT_DATE", "Product", "Trigger", "TRIGGER_LEVEL"), with = F]
          
        }
        
      } else {
        print("All Com No cpk")
        I = I[,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std", "p", "Diff", "Slope",
                  xname, "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "YEAR_WEEK", "CTQ", "Median", "CHART_KEY",
                 "COMMODITY", "REPORT_DATE", "Product", "Trigger"), with = F]
      }
      
    }
    
    if (sum(!is.na(I$Trigger)) > 0) {
      
      I <- get_trigger_con(I[I$Trigger == "YES",])
      
      return(I)
      
    } else {
      return(NA)
    }
    
  }
  
  cpk_plotting <- function(raw, trigger, com){
    
    print("Plotting")
    
	if(com %in% c("ACA", "ACT", "COIL", "HOOKUP")){
		xname = "GRP_YEAR_WEEK"
	} else {
		xname = "GROUP_NUM"
	}
	
    Y_Axis_Col = unique(trigger$Parameter)
    raw = as.data.table(raw)
    
    sel_columns = names(raw)[names(raw) %in% c("SUPPLIER_NAME","PART_NUM", "SBR_NUM", "STKEVT_CELL_ID", "HEAD_PSN", "GRMLD_CELL_ID", "GRMLD_HEAD_PSN", "CAVITY_NUM", "MOLD_NUM", "GROUP_DATETIME", "GRP_YEAR_WEEK", "EVENT_DATE", xname)]
    index_columns = c("SUPPLIER_NAME","PART_NUM","Parameter", "CAVITY_NUM", "MOLD_NUM")
    
    Raw_Plot = raw[,c(sel_columns,Y_Axis_Col), with = F]
    Index_plot = trigger[,names(trigger)[names(trigger) %in% index_columns]]
    Index_plot = as.data.table(Index_plot)
    
    Temp = data.table()
    # plot_list = list()
    
    for(i in 1:nrow(Index_plot)) {
      
      if(commodity == "HSA_MARPOSS") {
        
        X = Raw_Plot[SUPPLIER_NAME == Index_plot[i,SUPPLIER_NAME] & PART_NUM == Index_plot[i,PART_NUM] & SBR_NUM == Index_plot[i,SBR_NUM] & STKEVT_CELL_ID == Index_plot[i,STKEVT_CELL_ID] & HEAD_PSN == Index_plot[i,HEAD_PSN]]
        print(paste("value is", cell_id, "and ",h_psn ,  "sbr", sbr))
        setorderv(Raw_Plot,c("EVENT_DATE"),c(1))
        
        X = X[,c("SUPPLIER_NAME", "PART_NUM", "SBR_NUM", "STKEVT_CELL_ID", "HEAD_PSN", "EVENT_DATE", Index_plot[i,Parameter]), with=FALSE]
        
      } else if(commodity == "HSA_GRAMLOAD") {
        
        X = Raw_Plot[SUPPLIER_NAME ==  Index_plot[i,SUPPLIER_NAME] & PART_NUM  ==  Index_plot[i,PART_NUM]  & SBR_NUM ==  Index_plot[i,SBR_NUM] & GRMLD_CELL_ID ==  Index_plot[i,GRMLD_CELL_ID] & GRMLD_HEAD_PSN ==  Index_plot[i,GRMLD_HEAD_PSN]]
        setorderv(X,c("EVENT_DATE"),c(1))
        print(paste("value is", gram_cell_id, "and ",h_psn ,  "sbr", sbr))
        
        X = X[,c("SUPPLIER_NAME", "PART_NUM", "SBR_NUM", "GRMLD_CELL_ID", "GRMLD_HEAD_PSN", "EVENT_DATE", Index_plot[i,Parameter]), with=FALSE]
        
      } else if(commodity == "RAMP") { 
        
        X = Raw_Plot[SUPPLIER_NAME ==  Index_plot[i,SUPPLIER_NAME] & PART_NUM  ==  Index_plot[i,PART_NUM] & CAVITY_NUM == Index_plot[i,CAVITY_NUM] & MOLD_NUM == Index_plot[i,MOLD_NUM]]
        setorderv(X,c("GROUP_DATETIME"),c(1))
        print("FILTER RAMP")
        
        X = X[,c("SUPPLIER_NAME", "PART_NUM", "CAVITY_NUM", "MOLD_NUM", "GROUP_DATETIME", "GROUP_NUM", Index_plot[i,Parameter]), with=FALSE]
        
      } else if(commodity =="DSP") {
        
        check_dsp_metal = unique(trigger_index[COMMODITY =="DSP",c("PART_NUM","Remark"),]) 
        check_dsp_metal$PART_NUM <- as.character(check_dsp_metal$PART_NUM)
        Raw_Plot_temp = merge(Raw_Plot,check_dsp_metal, by = "PART_NUM", allow.cartesian=TRUE)
        if(is.na(Index_plot[i,]$CAVITY_NUM)) {
          X = Raw_Plot_temp[SUPPLIER_NAME ==  Index_plot[i,SUPPLIER_NAME] & PART_NUM  ==  Index_plot[i,PART_NUM]]
          setorderv(Raw_Plot_temp,c("GROUP_DATETIME"),c(1))
        } else {
          X = Raw_Plot_temp[SUPPLIER_NAME ==  Index_plot[i,SUPPLIER_NAME] & PART_NUM  ==  Index_plot[i,PART_NUM] & CAVITY_NUM == Index_plot[i,CAVITY_NUM]]
          setorderv(X,c("GROUP_DATETIME"),c(1))
        }
        
        X = X[,c("SUPPLIER_NAME", "PART_NUM", "CAVITY_NUM", "MOLD_NUM", "GROUP_DATETIME", "GROUP_NUM", Index_plot[i,Parameter]), with=FALSE]
        
      } else if (com %in% c("ACA", "ACT", "COIL", "HOOKUP")) {
        
        X = Raw_Plot[(SUPPLIER_NAME ==  Index_plot[i,SUPPLIER_NAME] & PART_NUM  ==  Index_plot[i,PART_NUM])]
        ifelse(commodity == "MOTOR", setorderv(X,c("GRP_YEAR_WEEK",xname),c(1,1)),setorderv(X,c("GRP_YEAR_WEEK","GROUP_DATETIME"),c(1,1)))
        print("OTHER")
        
        X = X[,c("SUPPLIER_NAME", "PART_NUM", "GRP_YEAR_WEEK" , Index_plot[i,Parameter]), with=FALSE]
        
      } else {
        
        X = Raw_Plot[(SUPPLIER_NAME == Index_plot[i,SUPPLIER_NAME] & PART_NUM  ==  Index_plot[i,PART_NUM])]
        ifelse(commodity == "MOTOR", setorderv(X,c("GRP_YEAR_WEEK",xname),c(1,1)),setorderv(X,c("GRP_YEAR_WEEK","GROUP_DATETIME"),c(1,1)))
        print("OTHER")
        
        X = X[,c("SUPPLIER_NAME", "PART_NUM", "GRP_YEAR_WEEK", "GROUP_NUM", Index_plot[i,Parameter]), with=FALSE]
        
      }
      
      X = as.data.frame(X)
      ## reorder axis
      
	  if(com %in% c("ACA", "ACT", "COIL", "HOOKUP")){
		xname = "GRP_YEAR_WEEK"
      } else {
		xname = "GROUP_NUM"
      }
      
      X[,xname] = as.character(X[,xname])
      X[,xname] = as.vector(X[,xname])
      X[,xname] = factor(X[,xname], levels = unique(X[,xname]))
      
      SPEC_all = spec[QPM_PARAMETER == Index_plot[i,Parameter ]  
                      & SUPPLIER_NAME ==  Index_plot[i,SUPPLIER_NAME]
                      & spec$PART_NUM == Index_plot[i,PART_NUM] ]
      
      SPEC_all = SPEC_all[,c("COMMODITY","PART_NUM","SUPPLIER_NAME", "QPM_PARAMETER","Target","USL","LSL")]
      
      SPEC_all =  melt(SPEC_all,  id.vars = c("COMMODITY","PART_NUM","SUPPLIER_NAME","QPM_PARAMETER"),
                       variable.name="LINE_TYPE")
      
      SPEC_all = as.data.frame(SPEC_all)
      SPEC_LINE = SPEC_all[SPEC_all$LINE_TYPE!="Target",]
      TARGET_LINE = SPEC_all[SPEC_all$LINE_TYPE=="Target",]
      
      SPEC_LINE = SPEC_LINE[,c("PART_NUM","SUPPLIER_NAME","value")]
      SPEC_LINE$value = as.numeric(SPEC_LINE$value)
      
      TARGET_LINE = TARGET_LINE[,c("PART_NUM","SUPPLIER_NAME","value")]
      TARGET_LINE$value = as.numeric(TARGET_LINE$value)
      
      if(com %in% c("ACA", "ACT", "COIL", "HOOKUP")){
		xname = "GRP_YEAR_WEEK"
      } else {
		xname = "GROUP_NUM"
      }
	
      yname  = Index_plot[i,Parameter]
      # xname = names(xname)
      # yname = names(yname)
      
      if(commodity == "DSP"){
        X = X[order(X$GROUP_DATETIME, X$GROUP_NUM, decreasing = T),]
        
        GROUP_NUM_LVL = unique(X[order(X$GROUP_DATETIME, X$GROUP_NUM),]$GROUP_NUM)
        
        X$GROUP_NUM <- factor(X$GROUP_NUM, levels = GROUP_NUM_LVL)
        
      }
      
      p =  ggplot(X, aes_string(x=xname,y=yname)) + geom_jitter(alpha=0.5, height=0, width=0.2)+
        theme_bw()+
        geom_boxplot(outlier.shape = NA,alpha=0.5) +
        stat_summary(fun.y=mean, geom="line", size=2,aes(group=1,color="blue")) +
        stat_summary(fun.y=mean, geom="point") +
        theme(legend.position="none",
              axis.text.x = element_text(angle = 90, hjust = 0),
              plot.title = element_text( colour = "blue",size=rel(1.4))
        ) 
      p = p + geom_hline(data=TARGET_LINE, aes(yintercept = value),color="green", linetype = "dashed")
      p = p + geom_hline(data=SPEC_LINE, aes(yintercept = value),color="red")
      
      if(commodity %in% c("RAMP")) {
        p = p + ggtitle(paste("Parameter:",yname," -  Supplier:",Index_plot[i,SUPPLIER_NAME],"-  Part_Num:",Index_plot[i,PART_NUM],"-  Cavity_num:",Index_plot[i,CAVITY_NUM], "-  Mold_num:", Index_plot[i,MOLD_NUM]))
      } else if(commodity %in% c("DSP")) {
        if(is.na(Index_plot[i,'CAVITY_NUM'])) {
          p = p + ggtitle(paste("Parameter:",yname," -  Supplier:",Index_plot[i,SUPPLIER_NAME],"-  Part_Num:",Index_plot[i,PART_NUM]))
        } else {
          p = p + ggtitle(paste("Parameter:",yname," -  Supplier:",Index_plot[i,SUPPLIER_NAME],"-  Part_Num:",Index_plot[i,PART_NUM],"-  Cavity_num:",Index_plot[i,CAVITY_NUM]))
        }
      } else {
        p = p + ggtitle(paste("Parameter:",yname," -  Supplier:",Index_plot[i,SUPPLIER_NAME],"-  Part_Num:",Index_plot[i,PART_NUM]))
      }
      
      # plot_list[[i]] = p
      
      SUP_NAME <- stringr::str_replace_all(Index_plot[i,SUPPLIER_NAME], "/", "")
      
      PNG_COMMODITY_PATH = paste(PNG_PATH,"/",com, sep = "")
      ifelse(!dir.exists(PNG_COMMODITY_PATH), dir.create(file.path(PNG_COMMODITY_PATH)), FALSE)
      PNG_SUP_PATH = paste(PNG_COMMODITY_PATH,"/",SUP_NAME, sep = "")
      ifelse(!dir.exists(PNG_SUP_PATH), dir.create(file.path(PNG_SUP_PATH)), FALSE)
      file_name = paste(PNG_SUP_PATH,"/",Index_plot[i,Parameter],"(",Index_plot[i,PART_NUM],")",".png", sep="")
      
      gc()
      
      png(file_name ,width = 1200 , height = 300)
      # print(plot_list[[i]])
      print(p)
      dev.off()
      print(paste("END :  ",i))
      
      gc()
      
    }
    
  }
  
  do_num_link <- function(raw, commodity){
    
    do_temp = unique(do_temp[,c("PART_NUM", "SUPPLIER_NAME", "GROUP_NUM", "DO_NUM")])
    
    do_temp = do_temp %>% group_by(PART_NUM, SUPPLIER_NAME, GROUP_NUM) %>% dplyr::summarise(DO_NUM = paste0(DO_NUM, collapse = ","))
    
    # do_qty <- read.csv(file.path(QPM_SPEC_folder,"data.csv"))
    
    # do_qty <- readRDS(file.path(QPM_SPEC_folder, "SHP_INFO.rda"))
    
    # do_qty <- unique(do_qty[,c("RECEIPT_DO_NUM","RECEIPT_PART_NUMBER","RECEIPT_QUANTITY")])
    
    # names(do_qty) <- c("DO_NUM","PART_NUM","DO_QTY")
    
    do_temp = left_join(do_temp, do_qty, by = c("DO_NUM","PART_NUM"))
    
    if (commodity == "MOTOR") {
      
      tmp = as.character(do_temp$GROUP_NUM)
      
      do_temp$GROUP_NUM = paste0(substr(tmp,4,4), "-",substr(tmp,2,3), "-", substr(tmp,1,1), substr(tmp,5,6))
      
    }
    
    data_set = left_join(raw, do_temp, by = c("PART_NUM", "SUPPLIER_NAME", "GROUP_NUM"))
    
    gc()
    
    return(data_set)
    
  }
  
  get_trigger_con <- function(working_data){
    
    trigger_mod = trigger_con[complete.cases(trigger_con),]
    
    trigger_mod = trigger_mod[,c("TRIGGER_LEVEL", "Parameter", "VALUE")]
    
    trigger_mod = melt(trigger_mod, id.vars = c("TRIGGER_LEVEL", "Parameter"))
    
    trigger_mod = trigger_mod[complete.cases(trigger_mod),]
    
    trigger_mod = reshape2::dcast(trigger_mod, TRIGGER_LEVEL ~ Parameter, drop = TRUE)
    
    names(trigger_mod) = c("TRIGGER_LEVEL", "Cpk_con", "Diff_con", "Slope_con")
    
    if ("TRIGGER_LEVEL" %in% names(working_data)) {
      
      working_data <- left_join(working_data, trigger_mod, by = "TRIGGER_LEVEL")
      
      if ("Cpk" %in% names(working_data)) {
        
        working_data <- working_data %>% mutate(trigger_con = paste(ifelse(Cpk < Cpk_con & !is.na(Cpk_con), paste0("CPK < ", Cpk_con), ""), ifelse(Diff > Diff_con & !is.na(Diff_con), paste0("Diff > ", Diff_con), ""), ifelse(Slope > Slope_con & !is.na(Slope_con), paste0("Slope > ", Slope_con), "")))
        
        working_data <- working_data[,!(names(working_data) %in% c("Cpk_con", "Diff_con", "Slope_con"))]
        
        working_data <- working_data[!grepl("NA", working_data$trigger_con),]
        
      } else {
        
        working_data <- working_data %>% mutate(trigger_con = paste(ifelse(Diff > Diff_con, paste0("Diff > ", Diff_con), ""), ifelse(Slope > Slope_con, paste0("Slope > ", Slope_con), "")))
        
        working_data <- working_data[,!(names(working_data) %in% c("Cpk_con", "Diff_con", "Slope_con"))]
        
      }
      
    }
    
    return(working_data)
    
  }
  
  #####
  
  test_type <- 1
  
  data_temp <- data.frame()
  
  # QPM_LAT_DATA <- QPM_LAT_DATA[!(QPM_LAT_DATA %in% "QPM_DASHBOARD_ACT.rda")]
  
  # QPM_LAT_DATA <- "QPM_DASHBOARD_SEALEDDRIVEBASEPLATE.rda"
  
  # QPM_LAT_DATA = "QPM_DASHBOARD_MOTOR.rda"
  
  Gen_date = as.character(.POSIXct(Sys.time(), tz="Asia/Shanghai"), format = "%Y%m%d")
  
  for (com in QPM_LAT_DATA) {
    
    commodity <- stringr::str_remove_all(com, "QPM_DASHBOARD_|.rda")
    
    print(commodity)
    
    tryCatch({
      
      data_set <- readRDS(file.path(QPM_folder, com))
      
      data_set$PART_NUM <- as.character(data_set$PART_NUM)
      
      if (commodity == "MOTOR") {
        
        tmp = as.character(data_set$GROUP_NUM)
        
        data_set$GROUP_NUM = paste0(substr(tmp,4,4), "-",substr(tmp,2,3), "-", substr(tmp,1,1), substr(tmp,5,6))
        
        rm(tmp)
        
      } else if (commodity == "PCBA") {
        
        data_set$PARAMETER_NAME = gsub(" ", "_", data_set$PARAMETER_NAME, fixed = TRUE)
        data_set$GROUP_NUM = data_set$LOT_NUM
        col_number = unique(data_set$PARAMETER_NAME)
        data_set$samples<-rownames(data_set)
        data_set =  spread(data_set, key='PARAMETER_NAME', value='PARAMETER_VALUE', fill=NA)
        data_set = as.data.table(data_set)
        data_set[,(col_number):= lapply(.SD, as.numeric), .SDcols = col_number]
        data_set = data_set[,!("samples"),with = F]
        data_set = as.data.frame(data_set)
        
      } else if(commodity == 'TGATRAY'){
		data_set$PART_NUM = as.character(data_set$PART_NUM)
		
	  } else if(commodity == 'ALUMINIUM_BLANK'){
		data_set$SUPPLIER_NAME = as.character(toupper(data_set$SUPPLIER_NAME))
		
	  } 
      
      print("CPK Table running")
      
      trigger <- cpk_table(data_set, commodity)
      
      print("CPK Table completed")
      
      print("head(trigger)")
      
      print(head(trigger))
      
      if (any(!is.na(trigger))) {
        
        print("Trigger Found")
        
        #trigger <- cpk_table(data_set, commodity)
        
        File_Path =  paste0(CSV_PATH,"/",commodity,sep= "")
        ifelse(!dir.exists(file.path(File_Path)), dir.create(file.path(File_Path)), FALSE)
        File_Name = paste0(File_Path,"/",commodity,"_","Result","_",Gen_date,".csv", sep = "")
        
        selecter <- c("SUPPLIER_NAME","PART_NUM","Parameter","CAVITY_NUM","MOLD_NUM","GROUP_NUM","N","Mean","Std","Median","DATA_SOURCE_TYPE","ETL_LOAD_DATE","YEAR_WEEK","Target","USL","LSL","Cpk","p","Diff","Slope","TRIGGER_LEVEL","Trigger","trigger_con","Material_type","Product")
        
        trigger <- trigger[,names(trigger)[names(trigger) %in% selecter]]
                
        tryCatch({
          
          cpk_plotting(data_set, trigger, commodity)
          
        }, error = function(e) {
          cat("ERROR :", conditionMessage(e), "\n")
        })
        
        gc()
        
		if (!(commodity %in% c("ACA", "ACT", "COIL", "HOOKUP"))) {
			trigger <- do_num_link(trigger, commodity)
		}
		
        trigger <- unique(trigger)
        
        if (commodity == "DSP") {
          names(trigger) <- c("PART_NUM", "Parameter", "SUPPLIER_NAME", "CAVITY_NUM", "Material type", "N", "Mean", "Std", "Cpk", "p", "Diff", "Slope", "GROUP_NUM", "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "YEAR_WEEK", "Median", "Target", "USL", "LSL", "Product", "Trigger", "TRIGGER_LEVEL", "Trigger Criteria", "DO_NUM", "DO_QTY")
          
        } else if (commodity %in% c("ACA", "ACT", "COIL", "HOOKUP")) {
          
			# names(trigger) <- c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std", "Cpk", "p", "Diff", "Slope", "GRP_YEAR_WEEK", "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "YEAR_WEEK", "Median", "Target", "USL", "LSL", "Product", "Trigger", "TRIGGER_LEVEL", "Trigger Criteria")
			print(names(trigger))
			c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std", "Cpk", "p", "Diff", "Slope", "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "GRP_YEAR_WEEK", "Median", "Target", "USL", "LSL", "Product", "Trigger", "TRIGGER_LEVEL", "Trigger Criteria")
			c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std", "Cpk", "p", "Diff", "Slope", "GRP_YEAR_WEEK", "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "Median", "Target", "USL", "LSL", "Product", "Trigger", "TRIGGER_LEVEL", "Trigger Criteria")
		
        } else {
          names(trigger) <- c("PART_NUM", "Parameter", "SUPPLIER_NAME", "N", "Mean", "Std", "Cpk", "p", "Diff", "Slope", "GROUP_NUM", "ETL_LOAD_DATE", "DATA_SOURCE_TYPE", "YEAR_WEEK", "Median", "Target", "USL", "LSL", "Product", "Trigger", "TRIGGER_LEVEL", "Trigger Criteria", "DO_NUM", "DO_QTY")
		  
        }
		
        write.csv(trigger, file  = File_Name, row.names= F, na="")
        
        gc()
        
      } else {
        print("No Trigger found")
      }
      
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
    
  }
  
}