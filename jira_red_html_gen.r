custom_data_prep = function(input_path, output_path) {

  require(dplyr)
  library(reshape2)
  library(tidyr)
  require(highcharter)
  library(knitr)
  library(kableExtra)
  library(flextable)
  library(officer)
  require(shinyWidgets)

  ########## Grouping Condition ##########
  
  LVL_2_DETECTED = rlang::exprs(
    .$TYPE %in% c(
      'MSCE External Supplier',
      'MSCE Internal Korat',
      'MSCE Internal Wuxi',
      'SQE'
    ) &
      .$ISSUE_DETECT_FUNCTION %in% c('Subtier >1', 'Subtier', 'External Supplier') ~ 'Supplier Ext',
    .$TYPE %in% c(
      'MSCE External Supplier',
      'MSCE Internal Korat',
      'MSCE Internal Wuxi',
      'SQE'
    ) & .$ISSUE_DETECT_FUNCTION == 'SQE' ~ 'SQE',
    .$TYPE %in% c(
      'Factory Quality',
      'MSCE External Supplier',
      'MSCE Internal Korat',
      'MSCE Internal Wuxi',
      'SQE'
    ) & .$ISSUE_DETECT_FUNCTION == 'Internal Supplier' ~ 'Supplier Int',
    .$TYPE %in% c(
      'MSCE External Supplier',
      'MSCE Internal Korat',
      'MSCE Internal Wuxi'
    ) & .$ISSUE_DETECT_FUNCTION == 'MSCE Monitoring' ~ 'MSCE',
    .$TYPE %in% c(
      'MSCE External Supplier',
      'MSCE Internal Korat',
      'MSCE Internal Wuxi',
      'SQE'
    ) & .$ISSUE_DETECT_FUNCTION == 'IQC' ~ "IQC",
    .$TYPE %in% c(
      'Factory Quality',
      'MSCE External Supplier',
      'MSCE Internal Korat',
      'MSCE Internal Wuxi'
    ) &
      .$ISSUE_DETECT_FUNCTION %in% c(
        'FOF',
        'In Process Quality',
        'CERT',
        'FVMI',
        'SWL',
        'ODT',
        'Factory Reli'
      ) ~ 'Factory',
    .$TYPE %in% c(
      'Customer Issue',
      'MSCE External Supplier',
      'MSCE Internal Korat',
      'MSCE Internal Wuxi'
    ) &
      .$ISSUE_DETECT_FUNCTION %in% c(
        'FAI',
        'FYZA',
        'aDPPM',
        'Customer Integration',
        'Customer Field'
      ) ~ 'Customer',
    .$TYPE %in% c(
      'SQE',
      'Factory Quality',
      'Customer Issue',
      'MSCE Internal Korat',
      'MSCE Internal Wuxi',
      'MSCE External supplier'
    ) & .$ISSUE_DETECT_FUNCTION == 'Design Center' ~ 'Design Center'
    # .$TYPE %in% c('SQE','Factory Quality','Customer Issue','MSCE Internal Korat', 'MSCE Internal Wuxi', 'MSCE External supplier') & .$ISSUE_DETECT_FUNCTION == 'Qualification' ~ 'Qualification'
  )
  
  LVL_2_DETECTED_group = rlang::exprs(
    .$LVL_2_DETECTED %in% c('Supplier Ext', 'SQE', 'Supplier Int', 'MSCE', 'IQC') ~ 'Components',
    .$LVL_2_DETECTED %in% c('Factory', 'Customer') ~ 'Drive'
  )
  
  LVL_1_DETECTED = rlang::exprs(
    .$LVL_2_DETECTED == 'Design Center' ~ 'Design',
    .$LVL_2_DETECTED %in% c('Supplier Ext', 'SQE') ~ 'External',
    .$LVL_2_DETECTED == 'Supplier Int' ~ 'Internal',
    .$LVL_2_DETECTED %in% c('MSCE', 'IQC') ~ 'Incoming Component',
    .$LVL_2_DETECTED %in% c('Factory') &
      .$ISSUE_DETECT_FUNCTION %in% c('FOF', 'In Process Quality') ~ 'Drive Assembly',
    .$LVL_2_DETECTED %in% c('Factory') &
      .$ISSUE_DETECT_FUNCTION %in% c('CERT') ~ 'Drive Cert',
    .$LVL_2_DETECTED_GROUP == 'Drive'  &
      .$ISSUE_DETECT_FUNCTION %in% c('FVMI', 'SWL', 'ODT', 'Factory Reli', 'FAI', 'FYZA', 'aDPPM') ~ 'Drive Quality',
    .$LVL_2_DETECTED %in% c('Customer') &
      .$ISSUE_DETECT_FUNCTION == 'Customer Integration' ~ 'Integration',
    .$LVL_2_DETECTED %in% c('Customer') &
      .$ISSUE_DETECT_FUNCTION == 'Customer Field' ~ 'Field'
  )
  
  LVL_1_DETECTED_group = rlang::exprs(
    .$LVL_1_DETECTED == 'Design' ~ 'Design',
    .$LVL_1_DETECTED %in% c('External', 'Internal') ~ 'Supplier OQ&A',
    .$LVL_1_DETECTED %in% c(
      'Incoming Component',
      'Drive Assembly',
      'Drive Cert',
      'Drive Quality'
    ) ~ 'Factory OQ&A',
    .$LVL_1_DETECTED %in% c('Integration', 'Field') ~ 'Customer'
  )
  
  lvl_1 = rlang::exprs(
    .$LOCATION == 'Design Center' ~ 'Design',
    .$LOCATION %in% c('Subtier >1', 'Subtier', 'External Supplier', 'SQE') ~ 'External',
    .$LOCATION == 'Internal Supplier' ~ 'Internal',
    .$LOCATION %in% c('MSCE Monitoring', 'IQC') ~ 'Incoming Component',
    .$LOCATION %in% c('FOF', 'In Process Quality') ~ 'Drive Assembly',
    .$LOCATION %in% c('CERT') ~ 'Drive Cert',
    .$LOCATION %in% c('FVMI', 'SWL', 'ODT', 'Factory Reli') ~ 'Drive Quality',
    .$LOCATION %in% c('FAI', 'FYZA', 'aDPPM') ~ 'Drive Quality',
    .$LOCATION == 'Customer Integration' ~ 'Integration',
    .$LOCATION == 'Customer Field' ~ 'Field'
  )
  
  lvl_1_group = rlang::exprs(
    .$LVL_1 == 'Design' ~ 'Design',
    .$LVL_1 %in% c('External', 'Internal') ~ 'Supplier OQ&A',
    .$LVL_1 %in% c(
      'Incoming Component',
      'Drive Assembly',
      'Drive Cert',
      'Drive Quality'
    ) ~ 'Factory OQ&A',
    .$LVL_1 %in% c('Integration', 'Field') ~ 'Customer'
  )
  
  ########## Unique Column conditions ##########
  
  ROOT_unique_col = c(
    "ISSUENUM",
    "SEVERITY",
    "ISSUE_YEAR_MONTH",
    "ISSUE_DETECT_FUNCTION",
    "ROOT_CAUSE_LOCATION",
    "LVL_2_DETECTED",
    "LVL_2_DETECTED_GROUP",
    "LVL_1_DETECTED",
    "LVL_1_DETECTED_GROUP"
  )
  ESCAPE_unique_col = c(
    "ISSUENUM",
    "SEVERITY",
    "ISSUE_YEAR_MONTH",
    "ISSUE_DETECT_FUNCTION",
    "ESCAPE_LOCATION",
    "LVL_2_DETECTED",
    "LVL_2_DETECTED_GROUP",
    "LVL_1_DETECTED",
    "LVL_1_DETECTED_GROUP"
  )
  DETECT_unique_col = c(
    "ISSUENUM",
    "SEVERITY",
    "ISSUE_YEAR_MONTH",
    "ISSUE_DETECT_FUNCTION",
    "LVL_2_DETECTED",
    "LVL_2_DETECTED_GROUP",
    "LVL_1_DETECTED",
    "LVL_1_DETECTED_GROUP"
  )
  
  ########## Column ordering / arrangements conditions ##########
  
  TABLE_ONE_TEMP <- data.frame(
    "Metric" = NA,
    "Design" = NA,
    "External" = NA,
    "Internal" = NA,
    "Component Incoming Control" = NA,
    "Drive Assembly" = NA,
    "Drive Cert" = NA,
    "Drive Quality" = NA,
    "Integration" = NA,
    "Field" = NA,
    "Pending" = NA
  )
  
  TABLE_ONE_NAME <-
    c(
      "Metric",
      "Design",
      "External",
      "Internal",
      "Incoming Component",
      "Drive Assembly",
      "Drive Cert",
      "Drive Quality",
      "Integration",
      "Field",
      'Pending'
    )
  
  names(TABLE_ONE_TEMP) <- TABLE_ONE_NAME
  
  LEVEL_TWO_TEMP <- data.frame(
    "Detection by" = NA,
    "Metric" = NA,
    "Design Center" = NA,
    "Qualification" = NA,
    "Subtier >1" = NA,
    "Subtier" = NA,
    "External Supplier" = NA,
    "SQE" = NA,
    "Internal Supplier" = NA,
    "MSCE Monitoring" = NA,
    "IQC" = NA,
    "FOF" = NA,
    "In Process Quality" = NA,
    "CERT" = NA,
    "FVMI" = NA,
    "SWL" = NA,
    "ODT" = NA,
    "Factory Reli" = NA,
    "FAI" = NA,
    "FYZA" = NA,
    "aDPPM" = NA,
    "Customer Integration" = NA,
    "Customer Field" = NA,
    "Pending" = NA
  )
  
  TABLE_TWO_NAME <- c(
    "Detection by",
    "Metric",
    "Design Center",
    "Qualification",
    "Subtier >1",
    "Subtier",
    "External Supplier",
    "SQE",
    "Internal Supplier",
    "MSCE Monitoring",
    "IQC",
    "FOF",
    "In Process Quality",
    "CERT",
    "FVMI",
    "SWL",
    "ODT",
    "Factory Reli",
    "FAI",
    "FYZA",
    "aDPPM",
    "Customer Integration",
    "Customer Field",
    "Pending"
  )
  
  names(LEVEL_TWO_TEMP) <- TABLE_TWO_NAME
  
  TABLE_TWO_ROW_ORDER <-
    c(
      "Respective Detect",
      "Supplier Ext",
      "SQE",
      "Supplier Int",
      "MSCE",
      "IQC",
      "Factory",
      "Customer"
    )
  
  LEVEL_THERE_TEMP <- data.frame(
    main_list = c(
      "Design Center",
      "Qualification",
      rep("Supplier Ext", 3),
      "SQE",
      "Supplier Int",
      "MSCE",
      "IQC",
      rep("Factory", 7),
      rep("Customer", 5)
    ),
    sub_list = c(
      "Design Center",
      "Qualification",
      "Subtier >1",
      "Subtier",
      "External Supplier",
      "SQE",
      "Internal Supplier",
      "MSCE Monitoring",
      "IQC",
      "FOF",
      "In Process Quality",
      "CERT",
      "FVMI",
      "SWL",
      "ODT",
      "Factory Reli",
      "FAI",
      "FYZA",
      "aDPPM",
      "Customer Integration",
      "Customer Field"
    )
  )
  
  ########## Color table prep ##########
  
  SEVERITY_color = data.frame(
    SEVERITY = c("Critical", "High", "Medium", "Low", "No Risk"),
    color = c("red", "orange", "yellow", "#a6a6a6", "#bfbfbf")
  )
  SEVERITY_color$SEVERITY <-
    ordered(SEVERITY_color$SEVERITY,
            c("Critical", "High", "Medium", "Low", "No Risk"))
  
  LVL_2_color = data.frame(
    xcolumn = c(
      "Supplier Ext",
      "SQE",
      "Supplier Int",
      "MSCE",
      "IQC",
      "Factory",
      "Customer"
    ),
    colors = c(
      "#C4D79B",
      "#8064A2",
      "#76933C",
      "#C5D9F1",
      "#FFFF00",
      "#FFC000",
      "#DA9694"
    )
  )
  
  LVL_3_color = data.frame(
    xcolumn = c(
      "Design Center",
      "Qualification",
      "Subtier >1",
      "Subtier",
      "External Supplier",
      "SQE",
      "Internal Supplier",
      "MSCE Monitoring",
      "IQC",
      "FOF",
      "In Process Quality",
      "CERT",
      "FVMI",
      "SWL",
      "ODT",
      "Factory Reli",
      "FAI",
      "FYZA",
      "aDPPM",
      "Customer Integration",
      "Customer Field",
      "Pending"
    ),
    colors = c(
      "#525155",
      "#878787",
      "#7E9B3E",
      "#A4C164",
      "#99BA52",
      "#8064A2",
      "#76933C",
      "#C5D9F1",
      "#FFFF00",
      "#B38600",
      "#CC9A00",
      "#FF8000",
      "#FF9600",
      "#FFAB00",
      "#FFC000",
      "#FFD500",
      "#C65E5B",
      "#CD716E",
      "#D38381",
      "#DA9694",
      "#E1A9A7",
      "#262626"
    )
  )
  
  ########## Raw Data prep ##########
  
  JIRA_raw <- readRDS(file.path(input_path, "QITAllrda"))
  
  names(JIRA_raw) <- toupper(names(JIRA_raw))
  
  JIRA_raw[JIRA_raw$ISSUE_DETECT_FUNCTION == 'MSCE_Monitoring', ]$ISSUE_DETECT_FUNCTION <-
    "MSCE Monitoring"
  
  Date_dic <- read.csv(file.path(input_path, "FW_EVENT_DATE.csv"))
  
  Date_dic <- Date_dic[, -1]
  
  Date_dic <- Date_dic %>% mutate_all(as.character)
  
  Date_dic$ACTUAL_DATE <-
    as.Date(Date_dic$ACTUAL_DATE, tryFormats = c("%m-%d-%Y"))
  
  Current_month <-
    Date_dic[Date_dic$ACTUAL_DATE == Sys.Date(), ]$FISCAL_YEAR_MONTH
  Date_dic <- Date_dic[Date_dic$ACTUAL_DATE < Sys.Date(), ]
  Date_dic$ISSUE_WEEK <-
    paste0(
      "WW",
      substr(Date_dic$FISCAL_YEAR_WEEK, 3, 4),
      substr(Date_dic$FISCAL_YEAR_WEEK, 6, 7)
    )
  
  ISSUE_DATE_DIC <-
    unique(Date_dic[c("FISCAL_YEAR_MONTH", "ISSUE_WEEK")])
  names(ISSUE_DATE_DIC)[1] <- c("ISSUE_YEAR_MONTH")
  
  ISSUE_DATE_DIC <- data.table::as.data.table(ISSUE_DATE_DIC)
  
  ISSUE_DATE_DIC <-
    data.table::setorderv(ISSUE_DATE_DIC, c("ISSUE_WEEK", "ISSUE_YEAR_MONTH"), c(1, 1))
  
  ISSUE_DATE_DIC <- ISSUE_DATE_DIC[, RANK := 1:.N, by = .(ISSUE_WEEK)]
  
  ISSUE_DATE_DIC <- ISSUE_DATE_DIC[ISSUE_DATE_DIC$RANK == 1, ]
  
  sev_con <- c("Medium", "High", "Critical")
  
  JIRA_raw <- left_join(JIRA_raw, ISSUE_DATE_DIC, by = "ISSUE_WEEK")
  
  Report_month <-
    sort(unique(JIRA_raw$ISSUE_YEAR_MONTH)[unique(JIRA_raw$ISSUE_YEAR_MONTH) < Current_month], decreasing = T)[1:6]
  
  Sel_month <- format(as.Date(Sys.Date() - 367), format = "%Y-%m")
  JIRA_prep <-
    JIRA_raw[JIRA_raw$STATUS != "Canceled" &
               JIRA_raw$ISSUE_YEAR_MONTH > Sel_month, ]
  JIRA_prep <-
    JIRA_prep[complete.cases(JIRA_prep$ISSUE_YEAR_MONTH), ]
  
  month_select_list <- unique(JIRA_prep$ISSUE_YEAR_MONTH)
  
  JIRA_prep <-
    JIRA_prep %>% dplyr::mutate(
      ESCAPE_LOCATION = stringr::str_split_fixed(ESCAPE_LOCATION_ESCAPE_CAUSE, '-', 2)[, 1],
      ESCAPE_CAUSE = stringr::str_split_fixed(ESCAPE_LOCATION_ESCAPE_CAUSE, '-', 2)[, 2]
    )
  
  JIRA_prep <- unique(JIRA_prep)
  
  JIRA_prep <-
    JIRA_prep %>% dplyr::mutate(LVL_2_DETECTED = case_when(!!!LVL_2_DETECTED))
  JIRA_prep <-
    JIRA_prep %>% dplyr::mutate(LVL_2_DETECTED_GROUP = case_when(!!!LVL_2_DETECTED_group))
  JIRA_prep <-
    JIRA_prep %>% dplyr::mutate(LVL_1_DETECTED = case_when(!!!LVL_1_DETECTED))
  JIRA_prep <-
    JIRA_prep %>% dplyr::mutate(LVL_1_DETECTED_GROUP = case_when(!!!LVL_1_DETECTED_group))
  
  JIRA_set_sel <- function() {
    JIRA_set_sel = JIRA_prep[JIRA_prep$ISSUE_YEAR_MONTH %in% month_select_list, ]
    
    return(JIRA_set_sel)
  }
  
  ######## Display Data Preparation #########
  
  level_1_data_prep <- function() {
    JIRA_set = JIRA_set_sel()
    
    data_prep = JIRA_set[JIRA_set$SEVERITY %in% sev_con &
                           complete.cases(JIRA_set$LVL_2_DETECTED), ]
    
    LVL_1_ROOT = unique(data_prep[, ROOT_unique_col])
    LVL_1_ROOT = LVL_1_ROOT %>% group_by(ROOT_CAUSE_LOCATION) %>% dplyr::summarise(Metric = "R", QTY = n())
    names(LVL_1_ROOT)[1] = "LOCATION"
    LVL_1_ROOT = LVL_1_ROOT %>% dplyr::mutate(LVL_1 = case_when(!!!lvl_1))
    LVL_1_ROOT = LVL_1_ROOT %>% dplyr::mutate(LVL_1_GROUP = case_when(!!!lvl_1_group))
    LVL_1_ROOT = LVL_1_ROOT[, c("LVL_1", "LVL_1_GROUP", "Metric", "QTY")]
    LVL_1_ESCAPE = LVL_1_ROOT %>%  group_by(LVL_1, LVL_1_GROUP, Metric) %>% dplyr::summarise(QTY = sum(QTY))
    names(LVL_1_ROOT)[1:2] = c('Location', 'Group')
    
    LVL_1_ESCAPE = as.data.frame(unique(data_prep[, ESCAPE_unique_col]), stringsAsFactors = F)
    LVL_1_ESCAPE = LVL_1_ESCAPE %>% group_by(ESCAPE_LOCATION) %>% dplyr::summarise(Metric = "E", QTY = n())
    names(LVL_1_ESCAPE)[1] = "LOCATION"
    LVL_1_ESCAPE = LVL_1_ESCAPE %>% dplyr::mutate(LVL_1 = case_when(!!!lvl_1))
    LVL_1_ESCAPE = LVL_1_ESCAPE %>% dplyr::mutate(LVL_1_GROUP = case_when(!!!lvl_1_group))
    LVL_1_ESCAPE = LVL_1_ESCAPE[, c("LVL_1", "LVL_1_GROUP", "Metric", "QTY")]
    LVL_1_ESCAPE = LVL_1_ESCAPE %>%  group_by(LVL_1, LVL_1_GROUP, Metric) %>% dplyr::summarise(QTY = sum(QTY))
    names(LVL_1_ESCAPE)[1:2] = c('Location', 'Group')
    
    LVL_1_DETECT = unique(data_prep[, DETECT_unique_col])
    LVL_1_DETECT = LVL_1_DETECT %>% group_by(LVL_1_DETECTED, LVL_1_DETECTED_GROUP) %>% dplyr::summarise(Metric = "D", QTY = n())
    names(LVL_1_DETECT)[1:2] = c('Location', 'Group')
    
    LVL_1_DETECT_ALL = unique(JIRA_set[, DETECT_unique_col])
    LVL_1_DETECT_ALL = JIRA_set %>% group_by(LVL_1_DETECTED, LVL_1_DETECTED_GROUP) %>% dplyr::summarise(Metric = "D Total", QTY = n())
    names(LVL_1_DETECT_ALL)[1:2] = c('Location', 'Group')
    
    LEVEL_1_TALBE = bind_rows(LVL_1_ROOT, LVL_1_ESCAPE, LVL_1_DETECT, LVL_1_DETECT_ALL)
    LEVEL_1_TALBE = LEVEL_1_TALBE[, c("Location", "Metric", "QTY")]
    LEVEL_1_TALBE[is.na(LEVEL_1_TALBE$Location), ]$Location = "Pending"
    
    LEVEL_1_TALBE = dcast(LEVEL_1_TALBE, Metric ~ Location, fun.aggregate = sum)
    
    if (length(TABLE_ONE_NAME) > ncol(LEVEL_1_TALBE)) {
      LEVEL_1_TALBE <- bind_rows(TABLE_ONE_TEMP[-1, ], LEVEL_1_TALBE)
    }
    
    LEVEL_1_TALBE = LEVEL_1_TALBE[order(match(LEVEL_1_TALBE$Metric, c('R', 'E', 'D', 'D Total'))), TABLE_ONE_NAME]
    
    row.names(LEVEL_1_TALBE) = c()
    LEVEL_1_TALBE[is.na(LEVEL_1_TALBE)] = ""
    
    return(LEVEL_1_TALBE)
    
  }
  
  level_3_data_prep <- function() {
    JIRA_set = JIRA_set_sel()
    
    data_prep = JIRA_set[JIRA_set$SEVERITY %in% sev_con &
                           complete.cases(JIRA_set$LVL_2_DETECTED), ]
    
    LVL_3_ROOT <- unique(data_prep[, ROOT_unique_col])
    LVL_3_ROOT <-
      LVL_3_ROOT %>% group_by(LVL_2_DETECTED, ROOT_CAUSE_LOCATION) %>% dplyr::summarise(Metric = "R", QTY = n())
    names(LVL_3_ROOT)[2] <- "columns"
    
    LVL_3_ESCAPE <- unique(data_prep[, ESCAPE_unique_col])
    LVL_3_ESCAPE <-
      LVL_3_ESCAPE %>% group_by(LVL_2_DETECTED, ESCAPE_LOCATION) %>% dplyr::summarise(Metric = "E", QTY = n())
    names(LVL_3_ESCAPE)[2] <- "columns"
    
    LVL_3_DETECT <- unique(data_prep[, DETECT_unique_col])
    LVL_3_DETECT <-
      LVL_3_DETECT %>% group_by(ISSUE_DETECT_FUNCTION) %>% dplyr::summarise(Metric = "D", QTY = n())
    names(LVL_3_DETECT)[1] <- "columns"
    LVL_3_DETECT$LVL_2_DETECTED <- "Respective Detect"
    LVL_3_DETECT <-
      LVL_3_DETECT[, c("LVL_2_DETECTED", "columns", "Metric", "QTY")]
    
    LEVEL_3_TALBE <-
      bind_rows(LVL_3_DETECT, LVL_3_ROOT, LVL_3_ESCAPE)
    
    LEVEL_3_TALBE[LEVEL_3_TALBE$columns %in% c(NA, ""), ]$columns <-
      "Pending"
    
    names(LEVEL_3_TALBE)[1] <- "Detection by"
    
    LEVEL_3_TALBE <-
      dcast(LEVEL_3_TALBE, `Detection by` + Metric ~ columns)
    
    LEVEL_3_TALBE <- bind_rows(LEVEL_TWO_TEMP, LEVEL_3_TALBE)
    
    LEVEL_3_TALBE <-
      LEVEL_3_TALBE[complete.cases(LEVEL_3_TALBE$`Detection by`), TABLE_TWO_NAME]
    
    row.names(LEVEL_3_TALBE) <- c()
    LEVEL_3_TALBE[is.na(LEVEL_3_TALBE)] <- ""
    
    LEVEL_3_TALBE <-
      LEVEL_3_TALBE[order(
        match(LEVEL_3_TALBE$`Detection by`, TABLE_TWO_ROW_ORDER),
        match(LEVEL_3_TALBE$Metric, c("R", "E", "D"))
      ), ]
    
    row.names(LEVEL_3_TALBE) <- c()
    LEVEL_3_TALBE[is.na(LEVEL_3_TALBE)] <- ""
    
    return(LEVEL_3_TALBE)
    
  }
  
  level_2_data_prep <- function(grouping, type) {
    JIRA_set = JIRA_set_sel()
    
    data_perp = unique(JIRA_set[, DETECT_unique_col])
    
    if (grouping == 'Customer') {
      data_perp = data_perp[data_perp$SEVERITY %in% sev_con &
                              complete.cases(data_perp$LVL_2_DETECTED) &
                              data_perp$LVL_2_DETECTED == 'Customer', ]
      names(data_perp)[names(data_perp) %in% 'ISSUE_DETECT_FUNCTION'] <-
        "xcolumn"
    } else if (grouping == 'Components') {
      data_perp = data_perp[data_perp$SEVERITY %in% sev_con &
                              complete.cases(data_perp$LVL_2_DETECTED) &
                              data_perp$LVL_2_DETECTED_GROUP == 'Components', ]
      names(data_perp)[names(data_perp) %in% 'LVL_2_DETECTED'] <-
        "xcolumn"
    } else {
      data_perp = data_perp[data_perp$SEVERITY %in% sev_con &
                              complete.cases(data_perp$LVL_2_DETECTED) &
                              data_perp$LVL_2_DETECTED_GROUP == 'Drive', ]
      names(data_perp)[names(data_perp) %in% 'LVL_2_DETECTED'] <-
        "xcolumn"
    }
    
    data_perp = data_perp %>% group_by(xcolumn, SEVERITY) %>% dplyr::summarise(QTY = n())
    
    data_perp_percent = data_perp %>% group_by(xcolumn) %>% dplyr::summarise(QTY = sum(QTY))
    data_perp_percent$Total = sum(data_perp_percent$QTY)
    data_perp_percent = data_perp_percent %>% group_by(xcolumn) %>% dplyr::summarise(Total = Total, Percent = round((QTY /
                                                                                                                       Total) * 100, 2))
    data_perp = data.frame(data_perp) %>% complete(xcolumn, SEVERITY)
    data_perp = left_join(data_perp, data_perp_percent)
    
    sum_order = data_perp %>% group_by(xcolumn) %>% dplyr::summarise(QTY = sum(QTY, na.rm = T))
    sum_order = sum_order[order(sum_order$QTY, sort(sum_order$QTY), decreasing = T), ]
    data_perp = data_perp[order(match(data_perp$xcolumn, sum_order$xcolumn)), ]
    
    return(data_perp)
    
  }
  
  ########## LEVEL 1 Table ##########
  
  data_set = level_1_data_prep()
  
  X <- flextable(data_set, col_keys = names(data_set)) %>%
    add_header(
      "Metric" = "",
      "Design" = "Design",
      "External" = 'Supplier OQ&A',
      "Internal" = 'Supplier OQ&A',
      "Incoming Component" = "Supplier OQ&A",
      "Drive Assembly" = "Factory OQ&A",
      "Drive Cert" = "Factory OQ&A",
      "Drive Quality" = "Factory OQ&A",
      "Integration" = "Customer",
      "Field" = "Customer",
      "Pending" = "Pending",
      top = T
    ) %>%
    theme_box() %>% width(width = 1)
  
  X <- X %>%
    merge_v(part = "header") %>%
    merge_h(part = "header") %>%
    merge_at(i = 1:2, j = 1, part = "header") %>%
    color(color = "white", part = "header") %>%
    bg(bg = "#008000", part = "header") %>%
    bg(i = 1,
       j = 1,
       bg = "white",
       part = "header")
  
  X <-
    X %>% border(border = fp_border(color = "#dddddd"), part = "all") %>%
    border(
      i = 1,
      j = 1,
      border.top = fp_border(color = "white"),
      border.left = fp_border(color = "white"),
      part = "header"
    ) %>%
    font(fontname = "Helvetica", part = "all") %>%
    fontsize(size = 14, part = "all") %>%
    align(align = "center", part = "all") %>%
    padding(padding = 8, part = "all") %>%
    bold(part = "header") %>%
    bold(j = 1, part = "body")
  
  X <- X %>% autofit() %>% width(width = 1.25)
  
  X <- X %>% htmltools_value()
  
  htmltools::save_html(X, file = file.path(output_path, "jira_lvl_one.html"))

  ########## LEVEL 3 Table ##########
  
  data_set = level_3_data_prep()
  
  display_table = data_set[, -1]
  
  X <- flextable(display_table, col_keys = names(display_table)) %>%
    theme_box %>%
    add_header(
      "Metric" = " ",
      "Design Center" = "Design Center",
      "Qualification" = "Qualification",
      "Subtier >1" = "Supplier Ext",
      "Subtier" = "Supplier Ext",
      "External Supplier" = "Supplier Ext",
      "SQE" = "SQE",
      "Internal Supplier" = "Supplier Int",
      "MSCE Monitoring" = "MSCE",
      "IQC" = "IQC",
      "FOF" = "Factory",
      "In Process Quality" = "Factory",
      "CERT" = "Factory",
      "FVMI" = "Factory",
      "SWL" = "Factory",
      "ODT" = "Factory",
      "Factory Reli" = "Factory",
      "FAI" = "Customer",
      "FYZA" = "Customer",
      "aDPPM" = "Customer",
      "Customer Integration" = "Customer",
      "Customer Field" = "Customer",
      "Pending" = "Pending",
      top = T
    ) %>%
    add_header(
      "Metric" = " ",
      "Design Center" = "Design Center",
      "Qualification" = "Qualification",
      "Subtier >1" = "Components",
      "Subtier" = "Components",
      "External Supplier" = "Components",
      "SQE" = "Components",
      "Internal Supplier" = "Components",
      "MSCE Monitoring" = "Components",
      "IQC" = "Components",
      "FOF" = "Drive",
      "In Process Quality" = "Drive",
      "CERT" = "Drive",
      "FVMI" = "Drive",
      "SWL" = "Drive",
      "ODT" = "Drive",
      "Factory Reli" = "Drive",
      "FAI" = "Drive",
      "FYZA" = "Drive",
      "aDPPM" = "Drive",
      "Customer Integration" = "Drive",
      "Customer Field" = "Drive",
      "Pending" = "Pending",
      top = T
    )
  
  X <- X %>%
    merge_v(j = 1, part = "body")
  
  if ("Supplier Ext" %in% data_set$`Detection by`) {
    X <- X %>% bg(j = 4:6, bg = "#C4D79B", part = "all") %>%
      bg(
        i = which(data_set$`Detection by` %in% "Supplier Ext"),
        bg = "#C4D79B",
        part = "body"
      )
  }
  
  if ("SQE" %in% data_set$`Detection by`) {
    X <- X %>%
      bg(j = 7, bg = "#8064A2", part = "all") %>%
      bg(
        i = which(data_set$`Detection by` %in% "SQE"),
        bg = "#8064A2",
        part = "body"
      )
  }
  
  if ("Supplier Int" %in% data_set$`Detection by`) {
    X <- X %>%
      bg(j = 8, bg = "#76933C", part = "all") %>%
      bg(
        i = which(data_set$`Detection by` %in% "Supplier Int"),
        bg = "#76933C",
        part = "body"
      )
  }
  
  if ("MSCE" %in% data_set$`Detection by`) {
    X <- X %>%
      bg(j = 9, bg = "#C5D9F1", part = "all") %>%
      bg(
        i = which(data_set$`Detection by` %in% "MSCE"),
        bg = "#C5D9F1",
        part = "body"
      )
  }
  
  if ("IQC" %in% data_set$`Detection by`) {
    X <- X %>%
      bg(j = 10, bg = "#FFFF00", part = "all") %>%
      bg(
        i = which(data_set$`Detection by` %in% "IQC"),
        bg = "#FFFF00",
        part = "body"
      )
  }
  
  if ("Factory" %in% data_set$`Detection by`) {
    X <- X %>%
      bg(j = 11:17,
         bg = "#FFC000",
         part = "all") %>%
      bg(
        i = which(data_set$`Detection by` %in% "Factory"),
        bg = "#FFC000",
        part = "body"
      )
  }
  
  if ("Customer" %in% data_set$`Detection by`) {
    X <- X %>%
      bg(j = 18:22,
         bg = "#DA9694",
         part = "all") %>%
      bg(
        i = which(data_set$`Detection by` %in% "Customer"),
        bg = "#DA9694",
        part = "body"
      )
  }
  
  X <- X %>% bg(i = 1, bg = "white", part = "header") %>%
    bg(i = 1, bg = "grey", part = "body") %>%
    merge_h(i = 1:3, part = "header") %>%
    merge_v(j = 1:23, part = "header") %>%
    merge_at(i = 1:3, j = 1, part = "header") %>%
    border(border = fp_border(color = "#dddddd"), part = "all") %>%
    border(
      i = 1,
      j = 1,
      border.top = fp_border(color = "white"),
      border.left = fp_border(color = "white"),
      part = "header"
    ) %>%
    font(fontname = "Helvetica", part = "all") %>%
    fontsize(size = 13, part = "all") %>%
    fontsize(size = 15, part = "body") %>%
    align(align = "center", part = "all") %>%
    padding(padding = 8, part = "all") %>%
    bold(part = "header") %>%
    bold(i = 1, part = "body") %>%
    bold(j = 1, part = "body")
  
  X <- X %>% autofit() %>% width(width = 1)
  
  X <- X %>% htmltools_value()

  htmltools::save_html(X, file = file.path(output_path, "jira_lvl_three.html"))
  
  ######### Level 2 ploting #########
  
  ######### Level 2 Components ploting
  
  data_set = level_2_data_prep('Components', 'side')
  
  sev_color = SEVERITY_color[SEVERITY_color$SEVERITY %in% data_set$SEVERITY, ]
  
  hc = highchart() %>%
    hc_add_series(
      data_set,
      "bar",
      hcaes(
        x = xcolumn,
        y = QTY,
        group = SEVERITY,
        customTotal = Total
      ),
      name = sev_color$SEVERITY,
      color = sev_color$color,
      dataLabels = list(
        color = "black",
        enabled = TRUE,
        format = "{point.y}"
      )
    ) %>%
    hc_title(text = "<b>Components Detection (by QTY)</b>",
             align = "left",
             style = list(useHTML = TRUE)) %>%
    hc_yAxis(
      reversedStacks = FALSE,
      stackLabels = list(
        color = "black",
        fontWeight = "bold",
        style = list(fontSize = "13px"),
        enabled = TRUE,
        formatter = JS(
          paste0(
            "function() {
                                                          var sumtotal = 0;

                                                          for (i = 0; i < this.axis.series.length; i++) {
                                                            for(j = 0; j < this.axis.series[i].yData.length; j++) {
                                                              sumtotal += this.axis.series[i].yData[j];
                                                            }
                                                          }

                                                          var pcnt = (this.total / sumtotal) * 100;
                                                          return Highcharts.numberFormat(pcnt) + '%';
                                                        }"
          )
        )
      )
    ) %>%
    hc_xAxis(categories = unique(data_set$xcolumn),
             labels = list(style = list(
               color = "black",
               fontWeight = "bold",
               fontSize = "13px"
             ))) %>%
    hc_plotOptions(series = list(stacking = 'normal')) %>%
    hc_legend(itemStyle = list(
      color = "black",
      fontWeight = "bold",
      fontSize = "13px"
    ))
  
  htmlwidgets::saveWidget(widget = hc,
                          file = file.path(output_path, "com_detect_lvl2.html"),
                          selfcontained = TRUE)
  
  ######### Level 2 Drive ploting
  
  data_set = level_2_data_prep('Drive', 'slide')
  
  sev_color = SEVERITY_color[SEVERITY_color$SEVERITY %in% data_set$SEVERITY, ]
  
  hc = highchart() %>%
    hc_add_series(
      data_set,
      "bar",
      hcaes(
        x = xcolumn,
        y = QTY,
        group = SEVERITY,
        customTotal = Total
      ),
      name = sev_color$SEVERITY,
      color = sev_color$color,
      dataLabels = list(
        color = "black",
        enabled = TRUE,
        format = "{point.y}"
      )
    ) %>%
    hc_title(text = "<b>Drive Detection (by QTY)</b>",
             align = "left",
             style = list(useHTML = TRUE)) %>%
    hc_yAxis(
      reversedStacks = FALSE,
      stackLabels = list(
        color = "black",
        fontWeight = "bold",
        style = list(fontSize = "13px"),
        enabled = TRUE,
        formatter = JS(
          paste0(
            "function() {
                                                          var sumtotal = 0;

                                                          for (i = 0; i < this.axis.series.length; i++) {
                                                            for(j = 0; j < this.axis.series[i].yData.length; j++) {
                                                              sumtotal += this.axis.series[i].yData[j];
                                                            }
                                                          }

                                                          var pcnt = (this.total / sumtotal) * 100;
                                                          return Highcharts.numberFormat(pcnt) + '%';
                                                        }"
          )
        )
      )
    ) %>%
    hc_xAxis(categories = unique(data_set$xcolumn),
             labels = list(style = list(
               color = "black",
               fontWeight = "bold",
               fontSize = "13px"
             ))) %>%
    hc_plotOptions(series = list(stacking = 'normal')) %>%
    hc_legend(itemStyle = list(
      color = "black",
      fontWeight = "bold",
      fontSize = "13px"
    ))
  
  htmlwidgets::saveWidget(widget = hc,
                          file = file.path(output_path, "drive_detect_lvl2.html"),
                          selfcontained = TRUE)
  
  ######### Level 2 Customer ploting
  
  data_set = level_2_data_prep('Customer', 'side')
  
  sev_color = SEVERITY_color[SEVERITY_color$SEVERITY %in% data_set$SEVERITY, ]
  
  hc = highchart() %>%
    hc_add_series(
      data_set,
      "bar",
      hcaes(
        x = xcolumn,
        y = QTY,
        group = SEVERITY,
        customTotal = Total
      ),
      name = sev_color$SEVERITY,
      color = sev_color$color,
      dataLabels = list(
        color = "black",
        enabled = TRUE,
        format = "{point.y}"
      )
    ) %>%
    hc_title(text = "<b>Customer Detection (by QTY)</b>",
             align = "left",
             style = list(useHTML = TRUE)) %>%
    hc_yAxis(
      reversedStacks = FALSE,
      stackLabels = list(
        color = "black",
        fontWeight = "bold",
        style = list(fontSize = "13px"),
        enabled = TRUE,
        formatter = JS(
          paste0(
            "function() {
                                                          var sumtotal = 0;

                                                          for (i = 0; i < this.axis.series.length; i++) {
                                                            for(j = 0; j < this.axis.series[i].yData.length; j++) {
                                                              sumtotal += this.axis.series[i].yData[j];
                                                            }
                                                          }

                                                          var pcnt = (this.total / sumtotal) * 100;
                                                          return Highcharts.numberFormat(pcnt) + '%';
                                                        }"
          )
        )
      )
    ) %>%
    hc_xAxis(categories = unique(data_set$xcolumn),
             labels = list(style = list(
               color = "black",
               fontWeight = "bold",
               fontSize = "13px"
             ))) %>%
    hc_plotOptions(series = list(stacking = 'normal')) %>%
    hc_legend(itemStyle = list(
      color = "black",
      fontWeight = "bold",
      fontSize = "13px"
    ))
  
  htmlwidgets::saveWidget(widget = hc,
                          file = file.path(output_path, "cus_detect_lvl2.html"),
                          selfcontained = TRUE)
  
}