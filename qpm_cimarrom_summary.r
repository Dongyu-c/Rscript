custom_data_prep = function(input_path, output_path) {
  
  library(data.table)
  library(reshape2)
  library(dplyr)
  library(plyr)
  
  QPM_folder <- file.path(ResultsPath, "E1052810", "RDA_MOD")
  cimmaron_pn_parm <- readRDS(file.path(ResultsPath, "E1082095","CIMMARONBP_DIC.rda"))
  QPM_SPEC_folder <- file.path(ResultsPath, "E1082262", "QPM_transfer")
  prep_ex <- file.path(UploadPath, "534180/Rscript/temp_files/Book1.xlsx")

  ##### SPEC
  
  SPEC = readRDS(file.path(QPM_SPEC_folder, "QPM_SPEC.rda"))
  
  colnames(SPEC)[grep('trigger level', colnames(SPEC))] <-
    "TRIGGER_LEVEL"
  
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
  
  DIC = readRDS(file.path(QPM_folder, "QPM_DIC.rda"))
  DIC = unique(DIC)
  DIC[] = lapply(DIC, as.character)
  DIC$PART_NUM = substr(DIC$PART_NUM, 4, nchar(DIC$PART_NUM))
  DIC[DIC == ""] = "N/A"
  names(DIC)[names(DIC) %in% 'ODT_PRODUCT_NAME'] <- "PRODUCT_NAME"
  DIC = DIC[!(DIC$PRODUCT_NAME %in% 'CIMARRON'), c("PART_NUM", "PRODUCT_NAME")]
  DIC$PRODUCT_NAME = toupper(DIC$PRODUCT_NAME)
  DIC = DIC[DIC$PRODUCT_NAME %in% c("CIMARRONBP3D", "CIMARRONBP4D", "CIMARRONBP5D"), ]
  
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
  
  file_list = list.files(QPM_folder)
  
  qpm_files = file_list[grepl("QPM_", file_list)]
  
  qpm_files = file_list[grepl(".rda", file_list)]
  
  qpm_files = file_list[!grepl("INP_QPM_|.rda.gz", file_list)]
  
  stack_df <- c()
  
  qpm_files = qpm_files[!(
    qpm_files %in% c(
      "data.csv",
      "all.rda",
      "dist_edp_pn.rda",
      "QPM_DASHBOARD_CHART_KEY.rda",
      "QPM_OOC_RANKING_NEW@E1033851.csv",
      "QPM_DIC.rda",
      "QPM_TRIGGER.rda",
      "QPM_CHART_KEY.rda",
      "QPM_SPEC.rda",
      "QPM_DASHBOARD_SQE_CTQ.rda",
      "SHP_INFO.rda",
      "qpm_filter_list.rda"
    )
  )]
  
  # qpm_files = "QPM_DASHBOARD_MOTOR.rda"
  
  check_list = toupper(cimmaron_pn_parm[!is.na(cimmaron_pn_parm$PART_NUM), ]$QPM_PARAMETER)
  part_num_list = toupper(cimmaron_pn_parm[!is.na(cimmaron_pn_parm$PART_NUM), ]$PART_NUM)
  
  for (file in qpm_files) {
    tryCatch({
      working_set <- readRDS(file.path(QPM_folder, file))
      
      if (!is.character(working_set$PART_NUM) &
          ("PART_NUM" %in% names(working_set))) {
        working_set$PART_NUM <- as.character(working_set$PART_NUM)
      }
      
      if ("SUPPLIER_NAME" %in% names(working_set)) {
        working_set$SUPPLIER_NAME <- toupper(working_set$SUPPLIER_NAME)
      }
      
      commodity <-
        stringr::str_remove_all(file, "QPM_DASHBOARD_|.rda")
      current_param_list <- names(working_set)
      xname = "GRP_YEAR_WEEK"
      
      if (commodity == 'WOVENTAPE') {
        names(working_set)[names(working_set) %in% "DATA_SORUCE_TYPE"] <-
          "DATA_SOURCE_TYPE"
      }
      
      # check_list = cimmaron_pn_parm[cimmaron_pn_parm$COMMODITY %in% commodity,]$QPM_PARAMETER
      # part_num_list = cimmaron_pn_parm[cimmaron_pn_parm$COMMODITY %in% commodity,]$PART_NUM
      
      # check_list = cimmaron_pn_parm[!is.na(cimmaron_pn_parm$PART_NUM),]$QPM_PARAMETER
      # part_num_list = cimmaron_pn_parm[!is.na(cimmaron_pn_parm$PART_NUM),]$PART_NUM
      
      if (any(check_list %in% current_param_list)) {
        print(commodity)
        
        col = check_list[check_list %in% current_param_list]
        
        if (commodity == "MOTOR") {
          # X = working_set[, c("SUPPLIER_NAME", "PART_NUM", "ETL_LOAD_DATE", "GRP_YEAR_WEEK", "DATA_SOURCE_TYPE", xname, col)]
          X = working_set[, c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "ETL_LOAD_DATE",
            "DATA_SOURCE_TYPE",
            xname,
            col
          )]
        } else {
          # X = working_set[, c("SUPPLIER_NAME", "PART_NUM", "ETL_LOAD_DATE", "GRP_YEAR_WEEK", "DATA_SOURCE_TYPE", "GROUP_DATETIME", xname, col)]
          X = working_set[, c(
            "SUPPLIER_NAME",
            "PART_NUM",
            "ETL_LOAD_DATE",
            "DATA_SOURCE_TYPE",
            "GROUP_DATETIME",
            xname,
            col
          )]
        }
        
        X = X[X$PART_NUM %in% part_num_list, ]
        
        if (nrow(X) > 0) {
          if (commodity == "MOTOR") {
            X = as.data.table(X)
            # setkeyv(X, c("SUPPLIER_NAME", "PART_NUM", "ETL_LOAD_DATE", "GRP_YEAR_WEEK", "DATA_SOURCE_TYPE", xname))
            setkeyv(
              X,
              c(
                "SUPPLIER_NAME",
                "PART_NUM",
                "ETL_LOAD_DATE",
                "DATA_SOURCE_TYPE",
                xname
              )
            )
            
            X = melt(
              X,
              measure.vars = col ,
              value.name = "VALUE",
              variable.name = "QPM_PARAMETER"
            )
            
            if (sum(is.na(X$VALUE)) > 0) {
              X = X[!is.na(X$VALUE), ]
            }
            
            X = X[, NORMALIZE_VALUE := (VALUE - mean(VALUE, na.rm = T)) / sd(VALUE, na.rm = T) ,
                  by = c("SUPPLIER_NAME",
                         "PART_NUM",
                         "QPM_PARAMETER",
                         "DATA_SOURCE_TYPE")]
            
            dt = X[, list(
              N = .N,
              Mean = mean(VALUE, na.rm = T),
              Std = sd(VALUE , na.rm = T),
              Median = median(VALUE , na.rm = T),
              ETL_LOAD_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)] ,
              YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)]
            ),
            by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "DATA_SOURCE_TYPE",
              xname
            )]
            
            dt = setorderv(
              dt,
              c(
                "SUPPLIER_NAME",
                "PART_NUM",
                "QPM_PARAMETER",
                "DATA_SOURCE_TYPE",
                xname
              ),
              c(1, 1, 1, 1, -1)
            )
            dt = dt[!(is.na(dt$Mean)), ]
            dt = dt[, RANK := 1:.N , by = .(SUPPLIER_NAME,
                                            PART_NUM,
                                            QPM_PARAMETER,
                                            DATA_SOURCE_TYPE)]
            ##create Mean of last 7 point
            dt = dt[, Last_7_Mean := mean(Mean[RANK[1:7]], na.rm = T), by = .(SUPPLIER_NAME,
                                                                              PART_NUM,
                                                                              QPM_PARAMETER,
                                                                              DATA_SOURCE_TYPE)]
            cpk_dt = dt[, c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "QPM_PARAMETER",
              "DATA_SOURCE_TYPE",
              xname,
              "Mean",
              "Std",
              "RANK",
              "ETL_LOAD_DATE"
            ), with = F]
            rmlist = names(dt)
            raw_dt = dt[RANK == 1]
            gc(dt)
            spec = SPEC
            spec = as.data.table(spec)
            
            dt$QPM_PARAMETER <- as.character(dt$QPM_PARAMETER)
            
            if (nrow(spec) > 0) {
              spec = spec[, .(SUPPLIER_NAME,
                              PART_NUM,
                              QPM_PARAMETER,
                              Target,
                              USL,
                              LSL)]
              y = SPEC_2 %>% dplyr::filter(COMMODITY == commodity)
              
              raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
              spec$PART_NUM = as.character(spec$PART_NUM)
              collist = rmlist[!(rmlist %in% c("RANK"))]
              raw_dt = raw_dt[, collist, with = F]
              rm_dup = unique(colnames(raw_dt))
              raw_dt = raw_dt[, rm_dup, with = F]
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
            
            dt <- left_join(dt, cpk_dt)
            
            dt$COMMODITY <- commodity
            
            dt$ETL_LOAD_DATE <- as.character(dt$ETL_LOAD_DATE)
            
            stack_df <- rbind.fill(stack_df, dt)
            
          } else {
            X = as.data.table(X)
            # setkeyv(X, c("SUPPLIER_NAME", "PART_NUM", "ETL_LOAD_DATE", "GRP_YEAR_WEEK", "DATA_SOURCE_TYPE", xname))
            setkeyv(
              X,
              c(
                "SUPPLIER_NAME",
                "PART_NUM",
                "ETL_LOAD_DATE",
                "DATA_SOURCE_TYPE",
                xname
              )
            )
            
            X = melt(
              X ,
              measure.vars = col ,
              value.name = "VALUE",
              variable.name = "QPM_PARAMETER"
            )
            
            if (sum(is.na(X$VALUE)) > 0) {
              X = X[!is.na(X$VALUE), ]
            }
            
            X = X[, NORMALIZE_VALUE := (VALUE - mean(VALUE, na.rm = T)) / sd(VALUE, na.rm = T) ,
                  by = c("SUPPLIER_NAME",
                         "PART_NUM",
                         "DATA_SOURCE_TYPE",
                         "QPM_PARAMETER")]
            
            dt = X[, list(
              N = .N,
              Mean = mean(VALUE, na.rm = T),
              Std = sd(VALUE , na.rm = T),
              Median = median(VALUE , na.rm = T),
              ETL_LOAD_DATE = sort(ETL_LOAD_DATE)[length(ETL_LOAD_DATE)],
              YEAR_WEEK = sort(GRP_YEAR_WEEK)[length(GRP_YEAR_WEEK)],
              GROUP_DATETIME = max(GROUP_DATETIME)
            ), by = c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "DATA_SOURCE_TYPE",
              "QPM_PARAMETER",
              xname
            )]
            
            dt = setorderv(
              dt,
              c(
                "SUPPLIER_NAME",
                "PART_NUM",
                "DATA_SOURCE_TYPE",
                "QPM_PARAMETER",
                "GROUP_DATETIME"
              ),
              c(1, 1, 1, 1, -1)
            )
            dt = dt[!(is.na(dt$Mean)), ]
            dt[, RANK := 1:.N , by = .(SUPPLIER_NAME,
                                       PART_NUM,
                                       DATA_SOURCE_TYPE,
                                       QPM_PARAMETER)]
            dt[, Last_7_Mean := mean(Mean[RANK[1:7]], na.rm = T), by =  .(SUPPLIER_NAME,
                                                                          PART_NUM,
                                                                          DATA_SOURCE_TYPE,
                                                                          QPM_PARAMETER)]
            cpk_dt = dt[, c(
              "SUPPLIER_NAME",
              "PART_NUM",
              "DATA_SOURCE_TYPE",
              "QPM_PARAMETER",
              xname,
              "Mean",
              "Std",
              "RANK",
              "GROUP_DATETIME"
            ), with = F]
            rmlist = names(dt)
            raw_dt = dt[RANK == 1]
            gc(dt)
            spec = SPEC
            spec = as.data.table(spec)
            
            levels(dt$QPM_PARAMETER) <- levels(dt$QPM_PARAMETER)
            dt$QPM_PARAMETER <- as.character(dt$QPM_PARAMETER)
            
            if (nrow(spec) > 0) {
              spec = spec[, .(SUPPLIER_NAME,
                              PART_NUM,
                              QPM_PARAMETER,
                              Target,
                              USL,
                              LSL)]
              y = SPEC_2 %>% dplyr::filter(COMMODITY == commodity)
              
              raw_dt$PART_NUM = as.character(raw_dt$PART_NUM)
              spec$PART_NUM = as.character(spec$PART_NUM)
              collist = rmlist[!(rmlist %in% c("RANK"))]
              raw_dt = raw_dt[, collist, with = F]
              rm_dup = unique(colnames(raw_dt))
              raw_dt = raw_dt[, rm_dup, with = F]
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
            
            dt <- left_join(dt, cpk_dt)
            
            dt$GROUP_DATETIME <- as.character(dt$GROUP_DATETIME)
            dt$ETL_LOAD_DATE <- as.character(dt$ETL_LOAD_DATE)
            
            dt$COMMODITY <- commodity
            # names(dt)[names(dt) %in% "com"] <- "COMMODITY"
            
            dt <- left_join(dt, DIC)
            
            dt <-
              dt[, c(
                "COMMODITY",
                "PRODUCT_NAME",
                "SUPPLIER_NAME",
                "PART_NUM",
                "DATA_SOURCE_TYPE",
                "QPM_PARAMETER",
                "GRP_YEAR_WEEK",
                "N",
                "Mean",
                "Std",
                "Median",
                "ETL_LOAD_DATE",
                "YEAR_WEEK",
                "GROUP_DATETIME",
                "RANK",
                "Last_7_Mean",
                "Target",
                "USL",
                "LSL",
                "Cpk"
              )]
            
            stack_df <- rbind.fill(stack_df, dt)
            
          }
          
        }
      }
      
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
    
  }
  
  wb <-
    openxlsx::loadWorkbook(prep_ex)
  
  for (c in sort(unique(stack_df$COMMODITY))) {
    openxlsx::addWorksheet(wb, sheetName = c)
    
    openxlsx::writeDataTable(wb,
                             c,
                             x = stack_df[stack_df$COMMODITY %in% c, ],
                             startRow = 1,
                             rowNames = FALSE)
    
  }
  
  openxlsx::removeWorksheet(wb, "Summary")
  
  openxlsx::saveWorkbook(
    wb,
    file.path(output_path,"QPM_CIMMARON_SUMMARY.xlsx"),
    overwrite = TRUE
  )
  
  
}
