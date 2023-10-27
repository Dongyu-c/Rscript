custom_data_prep = function(input_path, output_path) {
  
  library(data.table)
  library(dplyr)
  library(highcharter)
  library(nleqslv)
  
  ##### level 1 function
  
  level_one_cpk <- function(data_set, USL = NA, LSL= NA, f.na.rm = TRUE, ci = TRUE, alpha = 0.05 ){
    
    if (is.na(LSL) & is.na(USL)) {
      stop("Require Specification Limit !")
    }
    
    if (!is.numeric(data_set)){
      stop("Require numeric data !")
    }
    
    n = length(data_set)
    m = mean(data_set, na.rm = f.na.rm)
    s = sd(data_set, na.rm = f.na.rm)
    ul = (USL - m) / (3 * s)
    ll = (m - LSL) / (3 * s)
    cpk = min(ul, ll, na.rm = TRUE)
    
    if (ci == FALSE){
      cpk_result = as.numeric(cpk)
    } else {
      cpk_result = c(cpk*(1-(qnorm(1-(alpha/2))*sqrt((1/(9*n*cpk^2))+(1/(2*(n-1)))))), cpk, 
                     cpk*(1+(qnorm(1-(alpha/2))*sqrt((1/(9*n*cpk^2))+(1/(2*(n-1)))))))
    }
    
    return(cpk_result)
    
  }
  
  level_one_diff <- function(data_set, trig_x){
    
    trigger_set = data_set[data_set$X %in% trig_x,]
    base_line_set = data_set[!(data_set$X %in% trig_x),]
    
    alpha = 0.05 
    n = nrow(trigger_set)
    a = 1-alpha/2 # define alpha level
    t = stats::qt(a,n-1) 
    s = sd(data_set$PARAM,na.rm = TRUE) # 7 Group
    st = sd(trigger_set$PARAM,na.rm = TRUE)
    diff = (mean(trigger_set$PARAM, na.rm = TRUE) - mean(base_line_set$PARAM, na.rm = TRUE)) / s
    cl = t * st / s / sqrt(n) 
    
    if (any(is.nan(diff))) {
      diff[is.nan(diff)] <- NA
    }
    
    return(c(diff, cl))
    
  }
  
  level_one_slope <- function(data_set){
    
    alpha = 0.05 
    a = 1-alpha/2
    
    t1 = stats::qt(a, 7-2)
    # lr = stats::lm(PARAM ~ RANK, data = data_set)
    
    data_set = as.data.table(data_set)
    data_set = data_set[,list(Normal_mean = mean(NORMALIZE_VALUE , na.rm = T), Mean = mean(PARAM , na.rm = T)), by = c("SUPPLIER_NAME", "PART_NUM", "RANK")]
    lr = stats::lm(Normal_mean ~ RANK, data = data_set)
    
    slope = summary(lr)$coefficients[2,1]
    std_err = summary(lr)$coefficients[2,2]
    
    return(c(slope, std_err, t1))
    
  }
  
  level_one <- function(raw, trigger, trig_con, com){
    
    if(com %in% c("ACA", "ACT", "COIL", "HOOKUP")){
      xname = "GRP_YEAR_WEEK"
    } else {
      xname = "GROUP_NUM"
    }
    
    Y_Axis_Col = unique(trigger$Parameter)
    raw = as.data.table(raw)
    
    sel_columns = names(raw)[names(raw) %in% c("SUPPLIER_NAME", "PART_NUM", "SBR_NUM", "DATA_SOURCE_TYPE", "STKEVT_CELL_ID", "HEAD_PSN", "GRMLD_CELL_ID", "GRMLD_HEAD_PSN", "CAVITY_NUM", "MOLD_NUM", "GROUP_DATETIME", "GRP_YEAR_WEEK", "ETL_LOAD_DATE", "EVENT_DATE", xname)]
    index_columns = c("SUPPLIER_NAME", "PART_NUM", "Parameter", "CAVITY_NUM", "MOLD_NUM", "DATA_SOURCE_TYPE", "GRP_YEAR_WEEK", "GROUP_NUM", "ETL_LOAD_DATE")
    
    Raw_Plot = raw[,c(sel_columns,Y_Axis_Col), with = F]
    Index_plot = trigger[, names(trigger) %in% index_columns]
    Index_plot = as.data.table(Index_plot)
    
    Temp = data.table()
    
    for(i in 1:nrow(Index_plot)) {
      
      tryCatch({
        
        if (com %in% c("ACA", "ACT", "COIL", "HOOKUP")) {
          
          X = Raw_Plot[(SUPPLIER_NAME ==  Index_plot[i,SUPPLIER_NAME] & PART_NUM  ==  Index_plot[i,PART_NUM])]
          setorderv(X,c("GRP_YEAR_WEEK","GROUP_DATETIME"),c(1,1))
          
          X = X[,c("SUPPLIER_NAME", "PART_NUM", "GRP_YEAR_WEEK", "DATA_SOURCE_TYPE", "ETL_LOAD_DATE", Index_plot[i,Parameter]), with=FALSE]
          
        } else {
          
          X = Raw_Plot[(SUPPLIER_NAME == Index_plot[i,SUPPLIER_NAME] & PART_NUM  ==  Index_plot[i,PART_NUM])]
          
          ifelse(commodity == "MOTOR", setorderv(X,c("GRP_YEAR_WEEK",xname),c(1,1)),setorderv(X,c("GRP_YEAR_WEEK","GROUP_DATETIME"),c(1,1)))
          
          X = X[,c("SUPPLIER_NAME", "PART_NUM", "GRP_YEAR_WEEK", "GROUP_NUM", "DATA_SOURCE_TYPE", "ETL_LOAD_DATE", Index_plot[i,Parameter]), with=FALSE]
          
        }
        
        org_name <- names(X)
        X = data.frame(X)
        names(X) <- org_name
        
        names(X)[names(X) %in% Index_plot[i,Parameter]] <- "PARAM"
        
        names(X)[names(X) %in% xname] <- "X"
        
        lvl1_raw = X
        
        lvl1_raw$COM <- com 
        lvl1_raw$X_AXIS <- xname 
        lvl1_raw$Parameter <- Index_plot[i,]$Parameter 
        
        print("lvl1_raw")
        # print(lvl1_raw)
		
		lvl1_raw$ETL_LOAD_DATE <- as.POSIXct.default(lvl1_raw$ETL_LOAD_DATE)
		# lvl1_raw_data$ETL_LOAD_DATE <- as.POSIXct.default(lvl1_raw_data$ETL_LOAD_DATE)
        
        lvl1_raw_data <<- bind_rows(lvl1_raw, lvl1_raw_data) 
        
        rm(lvl1_raw)
        
        X = X[!is.na(X$PARAM),]
        
        Diff_Value = level_one_diff(X, Index_plot[i,][[xname]])
        
        rank = data.frame(X = unique(X$X), RANK = rank(desc(unique(X$X))), stringsAsFactors = F)
        X = left_join(X, rank)
        
        X = as.data.table(X)
        X = X[,NORMALIZE_VALUE := (PARAM- mean(PARAM, na.rm = T) )/sd(PARAM, na.rm = T), by = c("SUPPLIER_NAME","PART_NUM","DATA_SOURCE_TYPE")]
        X = as.data.frame(X) 
        
        X = X[X$RANK %in% c(1:7),]
        
        Cpk_Value = level_one_cpk(X[X$RANK %in% 1,]$PARAM, USL = trigger[i,]$USL, LSL= trigger[i,]$LSL, f.na.rm = TRUE, ci = TRUE, alpha = 0.05)
        Slope_Value = level_one_slope(X)
        
        temp = left_join(trigger[i,c("PART_NUM", "Parameter", "SUPPLIER_NAME", "TRIGGER_LEVEL", "COMMODITY", "GROUP_NUM", "GRP_YEAR_WEEK", "DATA_SOURCE_TYPE")], trig_con)
        
        temp$Cpk_Value <- Cpk_Value[2]
        temp$Cpk_Max_Value <- Cpk_Value[3]
        temp$Diff_Value <- abs(Diff_Value)[1]
        temp$Diff_CL_Value <- abs(Diff_Value)[2]
        temp$Slope_Value <- Slope_Value[1]
        temp$Slope_std_err_Value <- Slope_Value[2]
        temp$Slope_t1_Value <- Slope_Value[3]
        
        Temp = rbind(Temp, temp)
        
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
      
    }
    
    return(Temp)
    
  }
  
  ##### level 2 function
  
  level_two_plotter <- function(data_set, trigger) {
    
    temp_data <- final_result
    
    if (trigger == "mean") {
      
      temp_data = temp_data[order(temp_data$`Mean Shift`, decreasing = T),]
      
      hc = highchart() %>% hc_chart(type = "column") %>%
        hc_title(text = "<b>Level 2 Analysis -> Config Analysis by Statistics<b>", style = list(useHTML = TRUE)) %>%
        hc_yAxis(title = list(text = "<b>Contribution Percent<b>", style = list(useHTML = TRUE)),
                 plotLines = list(
                   list(color = "#000000", width = 2,
                        value = 0, zIndex = 5
                   )
                 )
        ) %>%
        hc_xAxis(title = list(text = ""), categories = temp_data$ATR) %>%
        hc_add_series(data = temp_data$`Mean Shift`, name = "Mean Shift", type = "column") %>%
        hc_add_series(data = temp_data$m_shift_threshold, name = "Threshold", type = "line", color = "#FF0000", width = 2, dashStyle = "dashdot",  marker = list(enabled = F)) %>% 
        hc_legend(enabled = T, align = "right", verticalAlign = "top", layout = "vertical", itemStyle = list(fontSize = "11px")) 
      
    } else if (trigger == "variance") {
      
      temp_data = temp_data[order(temp_data$`Variance Shift`, decreasing = T),]
      
      hc = highchart() %>% hc_chart(type = "column") %>%
        hc_title(text = "<b>Level 2 Analysis -> Config Analysis by Statistics<b>", style = list(useHTML = TRUE)) %>%
        hc_yAxis(title = list(text = "<b>Contribution Percent<b>", style = list(useHTML = TRUE)),
                 plotLines = list(
                   list(color = "#000000", width = 2,
                        value = 0, zIndex = 5
                   )
                 )
        ) %>%
        hc_xAxis(title = list(text = ""), categories = temp_data$ATR) %>%
        hc_add_series(data = temp_data$`Variance Shift`, name = "Variance + x2 Shift", type = "column") %>%
        hc_add_series(data = temp_data$v_shift_threshold, name = "Threshold", type = "line", color = "#FF0000", width = 2, dashStyle = "dashdot",  marker = list(enabled = F)) %>% 
        hc_legend(enabled = T, align = "right", verticalAlign = "top", layout = "vertical", itemStyle = list(fontSize = "11px")) 
      
    }
    
    return(hc)
    
  }
  
  lvl_two_drift <- function(data_set, attr_name){
    
    temp <- data_set %>% group_by(ATTR, X) %>% dplyr::summarise(m = mean(PARAM), v = var(PARAM))
    
    drift = data.table()
    
    for (attr in unique(temp$ATTR)) {
      
      df_temp = temp[temp$ATTR == attr,]
      df_temp = df_temp %>% mutate(X = dense_rank(X))
      
      # if(max(df_temp$X) < 2){
      # next()
      # }
      
      linearMod <- lm(m ~ X, data = df_temp)
      m_modelCoeffs <- summary(linearMod)$coefficients
      
      m_modelCoeffs <- data.table(variable = row.names(m_modelCoeffs), m_modelCoeffs)
      m_modelCoeffs$Type <- "mean-drift"
      
      linearMod <- lm(v ~ X, data = df_temp)
      v_modelCoeffs <- summary(linearMod)$coefficients
      
      v_modelCoeffs <- data.table(variable = row.names(v_modelCoeffs), v_modelCoeffs)
      v_modelCoeffs$Type <- "var-drift"
      
      df_temp = rbind(m_modelCoeffs, v_modelCoeffs)
      
      df_temp$ATR <- paste0(attr_name,": ",attr)
      
      drift <- rbind(df_temp, drift)
      
    }
    
    if (any(drift$`Pr(>|t|)` > 0.05)) {
      
      drift[drift$`Pr(>|t|)` > 0.05,]$Estimate <- 0
      
    }
    
    drift = drift[variable == "X", c("Estimate", "Type", "ATR")]
    names(drift)[1] <- "Coeff."
    
    temp <- data_set %>% group_by(X) %>% summarise(VAR_mean = mean(PARAM), Var_var = var(PARAM))
    temp <- temp %>% mutate(X = dense_rank(X))
    
    linearMod <- lm(VAR_mean ~ X, data = temp)
    modelCoeffs <- summary(linearMod)$coefficients
    xcoefv  = modelCoeffs["X", "Estimate"]
    xpv = modelCoeffs["X", "Pr(>|t|)"]
    
    linearMod <- lm(Var_var ~ X, data = temp)
    modelCoeffs <- summary(linearMod)$coefficients
    vcoefv = modelCoeffs["X", "Estimate"]
    vpv = modelCoeffs["X", "Pr(>|t|)"]
    
    mean_drift = drift %>% group_by(ATR) %>% filter(Type == 'mean-drift') %>% summarise(`Mean Drift` = ifelse(xpv > 0.05, 0, (Coeff. / xcoefv) * 100))
    var_drift = drift %>% group_by(ATR) %>% filter(Type == 'var-drift') %>% summarise(`Variance Drift` = ifelse(vpv > 0.05, 0, (Coeff. / vcoefv) * 100))
    
    drift = left_join(mean_drift, var_drift)
    
    drift = data.table(drift)
    
    return(drift)
    
  }
  
  lvl_two_shift = function(data_set, trig_x, attr_name){
    
    whole = data_set %>% summarise(bx = mean(PARAM[X != trig_x]), bv = var(PARAM[X != trig_x]), bsum = sum(X != trig_x), 
                                   tx = mean(PARAM[X == trig_x]), tv = var(PARAM[X == trig_x]), tsum = sum(X == trig_x), 
                                   md = tx-bx, vd = tv-bv)
    
    baseline = data_set %>% filter(X != trig_x) %>% group_by(ATTR) %>% summarise(`bx` = mean(PARAM), `bv` = var(PARAM), `bc` = n())
    trigger = data_set %>% filter(X == trig_x) %>% group_by(ATTR) %>% summarise(`tx` = mean(PARAM), `tv` = var(PARAM), `tc` = n())
    
    temp = data.table(left_join(baseline, trigger))
    
    temp[,1] = paste0(attr_name, ": ", temp$ATTR)
    names(temp)[1] <- "ATR"
    
    temp <- temp %>% mutate(bwt = bc / whole$bsum, twt = tc / whole$tsum,
                            `Mean Shift` = (twt*tx-bwt*bx)/whole$md*100,
                            `Variance Shift` = (twt*(tx*tx+tv)-bwt*(bx*bx+bv))/(whole$vd+whole$tx*whole$tx-whole$bx*whole$bx)*100)
    
    temp <- temp[,c("ATR","Mean Shift","Variance Shift")]
    
    return(temp)
    
  }
  
  lvl_two_threshold = function(data_set, con){
    
    df = length(data_set$ATR)
    vals <- qchisq(0.05,df)
    
    if (con == "var") {
      func <- function(x) (vals - sum((data_set$`Variance Shift`-x)**2/x))
    } else {
      func <- function(x) (vals - sum((data_set$`Mean Shift`-x)**2/x))
      
    }
    
    # print(data_set)
    
    threshold = nleqslv(10, func)$x
    
    return(threshold)
    
  }
  
  level_two <- function(raw, trigger, com, attr_list){
    
    if(com %in% c("ACA", "ACT", "COIL", "HOOKUP")){
      xname = "GRP_YEAR_WEEK"
    } else {
      xname = "GROUP_NUM"
    }
    
    Y_Axis_Col = unique(trigger$Parameter)
    raw = as.data.table(raw)
    
    #####
	
	# print("############### Checking")
	# print(head(raw))
    
    raw = left_join(raw, do_num_data, by = c("PART_NUM", "SUPPLIER_NAME", "GROUP_NUM"))
	raw = as.data.table(raw)
	
	# print("############### HEAD")
	# print(head(raw))
    
    #####
    
    sel_columns = names(raw)[names(raw) %in% c(attr_list, "SUPPLIER_NAME","PART_NUM", "SBR_NUM", "DATA_SOURCE_TYPE", "STKEVT_CELL_ID", "HEAD_PSN", "GRMLD_CELL_ID", "GRMLD_HEAD_PSN", "CAVITY_NUM", "MOLD_NUM", "GROUP_DATETIME", "GRP_YEAR_WEEK", "ETL_LOAD_DATE", "EVENT_DATE", "DO_NUM", xname)]
    index_columns = c(attr_list, "SUPPLIER_NAME", "PART_NUM", "Parameter", "CAVITY_NUM", "MOLD_NUM", "DATA_SOURCE_TYPE", "GRP_YEAR_WEEK", "GROUP_NUM", "ETL_LOAD_DATE")
    
    Raw_Plot = raw[,names(raw) %in% c(attr_list,sel_columns,Y_Axis_Col), with = F]
    
    Temp = data.table()
    
    if (com %in% c("ACA", "ACT", "COIL", "HOOKUP")) {
      
      X = Raw_Plot[(SUPPLIER_NAME ==  trigger[,SUPPLIER_NAME] & PART_NUM  ==  trigger[,PART_NUM] & DATA_SOURCE_TYPE == trigger[,DATA_SOURCE_TYPE])]
      setorderv(X,c("GRP_YEAR_WEEK","GROUP_DATETIME"),c(1,1))
      print("OTHER")
      
      X = X[,names(X) %in% c(attr_list, "GRP_YEAR_WEEK", "DATA_SOURCE_TYPE", trigger[,Parameter]), with=FALSE]
      
    } else {
      
      X = Raw_Plot[(SUPPLIER_NAME == trigger[,SUPPLIER_NAME] & PART_NUM  ==  trigger[,PART_NUM] & DATA_SOURCE_TYPE == trigger[,DATA_SOURCE_TYPE])]
      
      ifelse(commodity == "MOTOR", setorderv(X,c("GRP_YEAR_WEEK",xname),c(1,1)),setorderv(X,c("GRP_YEAR_WEEK","GROUP_DATETIME"),c(1,1)))
      print("OTHER")
      
      X = X[,names(X) %in% c(attr_list, "GRP_YEAR_WEEK", "GROUP_NUM", "DATA_SOURCE_TYPE", trigger[,Parameter]), with=FALSE]
      
    }
    
    # print(X)
    
    X = data.frame(X)
    
    names(X)[names(X) %in% trigger[,Parameter]] <- "PARAM"
    
    names(X)[names(X) %in% xname] <- "X"
    
    X = X[,colSums(is.na(X))<nrow(X)]
    
    X = X[!is.na(X$PARAM),]
    
    level_two_result = data.table()
    
    attr_list = attr_list[attr_list %in% names(X)]
	
    for (attr in attr_list) {
	
      print(attr)
      
      data <- X[,c(attr,"X","PARAM"),]
      
      names(data)[1] <- "ATTR"
      
      data$ATTR <- as.character(data$ATTR)
      
      if (any(data$ATTR %in% "")) {
        data[data$ATTR %in% "",]$ATTR <- "missing"
      }
      
      if (any(is.na(data$ATTR))) {
        data[is.na(data$ATTR),]$ATTR <- "missing"
      }
      
      data <- data[data$X %in% sort(unique(data$X), decreasing = T)[1:7],]
      
      # data <- data %>% group_by(ATTR) %>% mutate(X_RANK = dense_rank(X), MAX_X = max(X_RANK))
      
      lvl2_raw = data.frame(data)
      
      lvl2_raw$ATTR_NAME <- attr
      lvl2_raw$COM <- com
      lvl2_raw$X_AXIS <- xname
      lvl2_raw$SUPPLIER_NAME <- trigger$SUPPLIER_NAME 
      lvl2_raw$Parameter <- trigger$Parameter 
      lvl2_raw$PART_NUM <- trigger$PART_NUM
      
      lvl2_raw_data <<- rbind(lvl2_raw, lvl2_raw_data) 
      
      rm(lvl2_raw)
      
      # data <- data[data$MAX_X > 1,] 
      
      # print(data)
      
      # if(length(unique(data$ATTR)) <= 1){
      # next()
      # }
      
      print(paste0("ATTR :",paste0(unique(data$ATTR), collapse = ",")))
      
      tryCatch({
        
        # print(data)
        
        temp_drift = lvl_two_drift(data, attr)
        temp_shift = lvl_two_shift(data, trigger[[xname]], attr)
        
        temp <- left_join(temp_drift,temp_shift)
        
        level_two_result <- rbind(temp, level_two_result)
        
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
      
    }
    
    print("##### level_two_result #####")
    print(level_two_result)
    print("##### #####")
    
    if(nrow(level_two_result) > 0){
      
      if(all(complete.cases(level_two_result$`Variance Shift`))){
        v_shift_prep = level_two_result[order(level_two_result$`Variance Shift`, decreasing = T), c("ATR","Variance Shift")]
        v_shift_threshold = lvl_two_threshold(v_shift_prep, "var")
      } else {
        print(level_two_result)
        v_shift_threshold = NA
      }
      
      if(all(complete.cases(level_two_result$`Mean Shift`))){
        m_shift_prep = level_two_result[order(level_two_result$`Mean Shift`, decreasing = T),c("ATR","Mean Shift")]
        m_shift_threshold = lvl_two_threshold(m_shift_prep, "mean")
      } else {
        m_shift_threshold = NA
      }
      
      level_two_result$v_shift_threshold <- v_shift_threshold
      level_two_result$m_shift_threshold <- m_shift_threshold
      
    }
    
    return(level_two_result)
    
  }
  
  #####
  
  QPM_folder <- file.path(ResultsPath, "E1072320", "RDA_MOD") # QPM INP data path
  QPM_sample_folder <- file.path(ResultsPath, "E1065238", "RDA_MOD") # QPM INP sampling data path
  QPM_SPEC_folder <- file.path(ResultsPath, "E1082262", "QPM_transfer") # QPM Spec path
  QPM_DO_folder <- file.path(ResultsPath, "E1052810", "RDA_MOD") # QPM Do path
  
  # QPM_folder <- input_path
  # QPM_sample_folder <- input_path
  # QPM_SPEC_folder <- file.path(input_path)
  # QPM_DO_folder <- file.path(input_path)
  
  # print(list.files(QPM_folder))
  
  lvl1_raw_data <- data.frame() # Create empty data.frame for raw data stacking
  lvl2_raw_data <- data.frame() # Create empty data.frame for raw data stacking
  
  CONFIG_PATH <- file.path(output_path, "CONFIG")
  LVL_1_PATH <- file.path(output_path, "LVL_1")
  LVL_2_PATH <- file.path(output_path, "LVL_2")
  
  if (!dir.exists(LVL_1_PATH)) {
    dir.create(file.path(LVL_1_PATH))
  }
  
  if (!dir.exists(LVL_2_PATH)) {
    dir.create(file.path(LVL_2_PATH))
  }
  
  LvL_1_result <- data.table() # Create empty data.frame for result data stacking
  LvL_2_result <- data.table() # Create empty data.frame for result data stacking
  
  files_info <- file.info(list.files(CONFIG_PATH, full.names = T))
  trigger_file = row.names(files_info[files_info$mtime == max(files_info$mtime),])
  trigger_list = readRDS(trigger_file)
  
  trigger_list$ETL_LOAD_DATE <- as.POSIXct.default(trigger_list$ETL_LOAD_DATE)
  
  trig_list_looper = sort(unique(trigger_list$COMMODITY))
  
  print("##### LvL_1 started")
  
  # trig_list_looper <- "HOOKUP"
  
  for (com in trig_list_looper) {
    
    commodity <- com 
    
    if(commodity == 'HSA'){
      next()
    }
    
    tryCatch({
      
      print(commodity)
      
      if (commodity %in% c("HOOKUP", "MOTOR")) {
        raw <- readRDS(file.path(QPM_sample_folder, paste0('INP_QPM_DASHBOARD_',com,'.rda')))
        
      } else {
        raw <- readRDS(file.path(QPM_folder, paste0('INP_QPM_DASHBOARD_',com,'.rda')))
        
      }
      
      raw$PART_NUM <- as.character(raw$PART_NUM)
      
      if ("DATA_SOURCE_TYPE" %in% names(raw) == FALSE) {
        if (any(grepl("DATA_SOURCE_TYPE", names(raw)))) {
          names(raw)[grepl("DATA_SOURCE_TYPE", names(raw))] <-
            "DATA_SOURCE_TYPE"
        }
        
      }
      
      if ("GROUP_DATETIME" %in% names(raw) == FALSE) {
        if (any(grepl("GROUP_DATETIME", names(raw)))) {
          names(raw)[grepl("GROUP_DATETIME", names(raw))] <- "GROUP_DATETIME"
        } else {
          raw$GROUP_DATETIME <- "N/A"
        }
        
      }
      
      if (commodity == "MOTOR") {
        tmp = as.character(raw$GROUP_NUM)
        
        raw$GROUP_NUM = paste0(substr(tmp, 4, 4), "-", substr(tmp, 2, 3), "-", substr(tmp, 1, 1), substr(tmp, 5, 6))
        rm(tmp)
        
      } else if (commodity == "DSP") {
        raw$MAT_TYPE =  ifelse(raw$SUPPLIER_NAME == 'HI-P INTERNATIONAL LIMITED', 'Plastic', 'Metal')
        
      } else if (commodity == "PCBA") {
        raw$GROUP_NUM = raw$LOT_NUM
        col_number = unique(raw$PARAMETER_NAME)
        raw$samples <- rownames(raw)
        raw = spread(raw, key = 'PARAMETER_NAME', value = 'PARAMETER_VALUE', fill = NA)
        raw = as.data.table(raw)
        raw[, (col_number) := lapply(.SD, as.numeric), .SDcols = col_number]
        raw = raw[, !("samples"), with = F]
        raw = as.data.frame(raw)
        
      } else if(commodity == "EXT_HSA_GRAMLOAD"){
        raw$SUPPLIER_NAME = raw$LOCATION_CODE
      } else if(commodity == "EXT_HSA_MARPOSS"){
        raw$SUPPLIER_NAME = raw$LOCATION_CODE
      } else if(commodity == "EXT_HSA_RESONANCE_SUM"){
        raw$SUPPLIER_NAME = raw$LOCATION_CODE
      } else if(commodity == "HGA"){
        raw$SUPPLIER_NAME = raw$LOCATION_CODE
      } 
      
      trigger_con = readRDS(file.path(QPM_folder,"QPM_TRIGGER.rda"))
      
      trigger_con = as.data.table(trigger_con[complete.cases(trigger_con),])
      trigger_con = trigger_con[,c("TRIGGER_LEVEL", "Parameter", "VALUE")]
      trigger_con = melt(trigger_con, id.vars = c("TRIGGER_LEVEL", "Parameter"))
      trigger_con = trigger_con[complete.cases(trigger_con),]
      trigger_con = dcast(trigger_con, TRIGGER_LEVEL ~ Parameter, drop = TRUE)
      
      trigger_con = trigger_con[,(c('Cpk', 'Diff', 'Slope')) := lapply(.SD, as.numeric), .SDcols = c('Cpk', 'Diff', 'Slope')] 
      
      names(trigger_con)[2:4] <- c("Cpk_Limit", "Diff_Limit", "Slope_Limit")
      
      names(trigger_list)[names(trigger_list) == "YEAR_WEEK"] <- "GRP_YEAR_WEEK"
      trigger = trigger_list[trigger_list$COMMODITY %in% commodity,]
      
      trigger$TRIGGER_LEVEL <- as.character(trigger$TRIGGER_LEVEL)
      trigger_con$TRIGGER_LEVEL <- as.character(trigger_con$TRIGGER_LEVEL)
      
      LvL_1_table <- level_one(raw, trigger, trigger_con, commodity)
      
      # LvL_1_table <- left_join(LvL_1_table)
      
      if (nrow(LvL_1_table) > 0) {
        LvL_1_table <- LvL_1_table[, c("Cpk_Significance", "Diff_Significance", "Slope_Significance") := list(Cpk_Max_Value < Cpk_Limit, (abs(Diff_Value)  - Diff_Limit) > Diff_CL_Value, (abs(Slope_Value) - Slope_Limit) > Slope_std_err_Value * Slope_t1_Value)] 
        
        # LvL_1_table <- LvL_1_table %>% dplyr::mutate(dplyr::across(c(Cpk_Significance,Diff_Significance,Slope_Significance), na_if, FALSE))
        
        LvL_1_table[is.na(LvL_1_table)] <- FALSE
        
        LvL_1_result <- rbind(LvL_1_table, LvL_1_result)
        
      }
      
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
    
    #####
    
    do_num_data <- readRDS(file.path(QPM_folder, 'QPM_DASHBOARD_DO.rda'))
    
    do_num_data <- unique(do_num_data[,c("PART_NUM", "SUPPLIER_NAME", "GROUP_NUM", "DO_NUM")])
    
    #####
    
    attr_dic = read.csv(file.path(UploadPath,GID,'QPM_INP_CONFIG_ATTR_DIC.csv'), stringsAsFactors = F)
    
    # con = LvL_1_table$Cpk_Significance | LvL_1_table$Diff_Significance | LvL_1_table$Slope_Significance
    
    # LvL_2_loop <- LvL_1_table[con,]
    
	print("LVL 2")
	
    for (i in 1:nrow(LvL_1_table)) {
     
      data_set = LvL_1_table[i,]
      
      if (any(attr_dic$COMMODITY %in% data_set$COMMODITY)) {
        
        attr_list = attr_dic[attr_dic$COMMODITY %in% data_set$COMMODITY,]$ATTR
        
        tryCatch({
          
          LvL_2_table <- level_two(raw, data_set, data_set$COMMODITY, attr_list)
          
          if (nrow(LvL_2_table) > 0) {
            
            LvL_2_table <- cbind(data_set, LvL_2_table)
            
            LvL_2_result <- rbind(LvL_2_table, LvL_2_result)
            
          }
          
        }, error = function(e) {
          cat("ERROR :", conditionMessage(e), "\n")
        })
        
      }
      
    }
    
  }
  
  print("##### LvL_1 competed")
  
  LvL_1_result <- LvL_1_result[order(LvL_1_result$COMMODITY, LvL_1_result$PART_NUM, LvL_1_result$Parameter, LvL_1_result$SUPPLIER_NAME),] 
  
  # print(file.path(LVL_1_PATH, "LvL_1_table.csv"))
  
  write.csv(LvL_1_result, file.path(LVL_1_PATH, "LvL_1_table.csv"), row.names= F, na="") 
  
  ###### level two looping ###### 
  
  print("##### LvL_2 competed")
  
  write.csv(LvL_2_result, file.path(LVL_2_PATH, "LvL_2_table.csv"), row.names= F, na="") 
  
  ###### level one + two raw data ######
  
  write.csv(lvl1_raw_data, file.path(LVL_2_PATH, "LvL_1_raw.csv"), row.names= F, na="") 
  write.csv(lvl2_raw_data, file.path(LVL_2_PATH, "LvL_2_raw.csv"), row.names= F, na="") 
  
}