custom_data_prep = function(input_path, output_path) {
  
  library(data.table)
  library(RPresto)
  library(stringr)
  library(lubridate)
  library(glue)
  
  ##### TGA - QPM
  
  qpm <- readRDS(file.path(input_path, "QPM_TGA.rda"))
  rsnce <- readRDS(file.path(input_path, "HGA_TGA.rda"))
  
  qpm <- data.table(qpm)
  rsnce <- data.table(rsnce)
  
  names(rsnce) <- tolower(names(rsnce))
  
  # define mpt tray_1d_barcode lot decode function
  mpt_1d2lot <-  function(tray_1d = NULL) {
    if (length(tray_1d) == 0) {
      return(NA)
    }
    lkp <- c(
      c(1:9),
      sapply(LETTERS[1:8]  , utf8ToInt) - 55,
      sapply(LETTERS[10:14], utf8ToInt) - 56,
      sapply(LETTERS[16:26], utf8ToInt) - 57
    )
    names(lkp)[1:9] <- 1:9
    lkp[1:9] <- sprintf("%02d", 1:9)
    do.call('c', lapply(tray_1d, function(x) {
      if (nchar(x) != 18 | is.na(x)) {
        return(NA)
      }
      x <- toupper(x)
      d1_4 <- substr(x, 10, 13)   #digit 1-4
      d5 <-
        ifelse(substr(x, 18, 18) == 'U', 1, 2) #digit 5, up/dn
      d6_7 <- substr(x, 7, 8)     #digit 6-7
      d8_9 <- substr(x, 4, 4)     #digit 8-9
      d10_11 <- substr(x, 2, 2)   #digit 10-11
      d12_13 <- substr(x, 3, 3)   #digit 12-13
      d14 <- substr(x, 14, 14)
      d.value <-
        paste0(d1_4, d5, d6_7,
               lkp[names(lkp) == d8_9],
               lkp[names(lkp) == d10_11],
               lkp[names(lkp) == d12_13],
               d14, '00')
    }))
  }
  
  ### define is.NaN replace function for list
  is.NaN <- function(x) {
    do.call(cbind, lapply(x, is.nan))  
  }
  
  ### handle important attributes
  setnames(qpm, toupper(names(qpm)))
  qpm[, SITE := gsub("[( )]","",str_extract(SUPPLIER_NAME, "\\((.*?)[ )]"))]
  qpm[, SUPPLIER_NAME := str_extract(SUPPLIER_NAME, "[^ ]+")]
  qpm[, SITE := glue_data(.SD, '{SUPPLIER_NAME}({SITE})')]
  qpm[, DRAWING_NUM := ifelse(is.na(DRAWING_NUM), NA,
                              ifelse(is.na(DRAWING_REV), DRAWING_NUM,
                                     paste0(DRAWING_NUM, '_', DRAWING_REV)
                              )
  )]
  qpm[, PART_REV := ifelse(is.na(PRODUCT_PART_NUM), NA,
                           ifelse(is.na(PART_REV), PRODUCT_PART_NUM,
                                  paste0(PRODUCT_PART_NUM, '_', PART_REV)
                           )
  )]
  qpm[, PERIOD := floor_date(ETL_LOAD_DATE, unit = 'day')]
  
  
  system.time(qpm[, TGA_LOT_NUM :=
                    ifelse(SITE == 'NHK(KOMAGANE)', paste0(substr(TRAY_1D_BARCODE, 9, 15), 'X'),
                           ifelse(SITE == 'NHK(THAILAND)', paste0(substr(TRAY_1D_BARCODE, 9, 16), 'X'),
                                  ifelse(SITE %like% 'MAGNECOMP', mpt_1d2lot(TRAY_1D_BARCODE),
                                         ifelse(SITE %like% 'HUTCHINSON', substr(TRAY_1D_BARCODE,14,17),'NA'))))])
  
  ### handle duplicated record using latest date per tray_1d per record_seq
  cols <- c('TGA_LOT_NUM', 'TGA_LOT_TYPE', 'TRAY_1D_BARCODE', 'RECORD_SEQ', 'RECORD_DATETIME','ETL_LOAD_DATE','INP_ETL_LOAD_DATE')
  setorderv(qpm, cols, c(1,1,1,1,-1,-1,-1), na.last = TRUE)
  qpm <- unique(qpm, by=cols[1:4])
  
  ### set columns order
  cols_order <-
    c(
      'SUPPLIER_NAME',
      'SITE',
      'PERIOD',
      'PRODUCT',
      'PRODUCT_PART_NUM',
      'PART_REV',
      'DATECODE',
      'SHIP_DATE',
      'ETL_LOAD_DATE',
      'LAST_UPDATED_DATE',
      'ETL_SOURCE_NAME',
      'GROUP_NUM',
      'GROUP_DATETIME',
      'TGA_LOT_NUM',
      'LAT_LOT_NUM',
      'SBR_NUM',
      'SUB_LOT_NUM',
      'TGA_LOT_TYPE',
      'LOT_CONFIG',
      'LOT_QTY',
      'TRAY_1D_BARCODE',
      'RECORD_SEQ',
      'RECORD_DATETIME',
      'PO_NUM',
      'LINE_NUM',
      'DRAWING_NUM',
      'DRAWING_REV',
      'BASEPLATE_LOT_NUM',
      'CA_LOT_NUM',
      'DAMPER_LOT_NUM',
      'DAMPER_REEL_NUM',
      'FLEXURE_LOT_NUM',
      'LOADBEAM_LOT_NUM',
      'NCA_LOT_NUM',
      'PZT_NEGATIVE_LOT_NUM',
      'PZT_NEGATIVE_WAFER_NUM',
      'PZT_POSITIVE_LOT_NUM',
      'PZT_POSITIVE_WAFER_NUM',
      'INP_ATTR_FLAG',
      'INP_ETL_LOAD_DATE',
      'INP_LAST_UPDATED_DATE',
      'INP_BP_WASH',
      'INP_BP_SORTER',
      'INP_LB_WASH',
      'INP_LB_FORM',
      'INP_LASER_LAPPING',
      'INP_FLEX_SEPARATION',
      'INP_FLEX_PRE_SING',
      'INP_FLEX_PANEL_TRIM',
      'INP_FLEX_FINAL_SING',
      'INP_FLEX_SORTER',
      'INP_PZT_ATTACH',
      'INP_FLEX_FORM_SORT',
      'INP_LASER_WELD',
      'INP_TWO_IN_ONE_DETAB',
      'INP_WASHING1',
      'INP_PL_FORM_FINAL_DETAB',
      'INP_PL_FORM',
      'INP_ANNEALING',
      'INP_OVEN_CURE',
      'INP_FINAL_DETAB',
      'INP_WASHING2',
      'INP_DAMPER_ATTACH',
      'INP_GL_SA_ADJUST',
      'INP_CAPA_RESIST',
      'GRAM_LOAD',
      'PSA',
      'RSA',
      'ASSY_BASEPLATE_TIP_HEIGHT',
      'ASSY_DIMPLE_POS_X',
      'ASSY_DIMPLE_POS_Y',
      'TAIL_ALIGN_X',
      'TAIL_ALIGN_Y',
      'ASSY_LIFT_TAB_OFFSET',
      'STROKE',
      'T1_NOTCH_LOC',
      'LOAD_BEAM_TWIST',
      'SAG_ANGLE',
      'DCF',
      'ESR',
      'CAPACITANCE',
      'LEFT_PZT_CAPACITANCE',
      'RIGHT_PZT_CAPACITANCE',
      'LEFT_PZT_RESISTANCE',
      'RIGHT_PZT_RESISTANCE',
      'LEFT_PZT_HEIGHT_ABOVE_SST',
      'RIGHT_PZT_HEIGHT_ABOVE_SST',
      'LEFT_PZT_PARALLELISM',
      'RIGHT_PZT_PARALLELISM',
      'LEFT_PZT_CA_HEIGHT_BOTTOM',
      'RIGHT_PZT_CA_HEIGHT_BOTTOM',
      'LEFT_PZT_POSITION_X',
      'RIGHT_PZT_POSITION_X',
      'LEFT_PZT_POSITION_Y',
      'RIGHT_PZT_POSITION_Y',
      'COMP_PZT_LENGTH',
      'COMP_PZT_WIDTH',
      'COMP_PZT_THICKNESS',
      'COMP_PZT_GAP3',
      'COMP_PZT_GAP4',
      'COMP_PZT_NI_THICKNESS',
      'COMP_PZT_AU_THICKNESS',
      'COMP_PZT_D31',
      'COMP_LB_DIMPLE_POS_X',
      'COMP_LB_DIMPLE_POS_Y',
      'COMP_LB_DIMPLE_DELTA_X',
      'COMP_LB_DIMPLE_DELTA_Y',
      'LB_1ST_TORSION_FREQ',
      'LB_1ST_TORSION_GAIN',
      'LB_1ST_SWAY_FREQ',
      'LB_1ST_SWAY_GAIN',
      'LB_1ST_BEND_FREQ',
      'LB_1ST_BEND_GAIN',
      'LB_2ND_TORSION_FREQ',
      'LB_2ND_TORSION_GAIN',
      'LB_2ND_SWAY_FREQ',
      'LB_2ND_SWAY_GAIN',
      'BOSS_ID',
      'BOSS_OD_X1',
      'BOSS_OD_Y1',
      'BOSS_OD',
      'UPZT_LB1T_N_FREQ',
      'UPZT_LB1T_N_GAIN',
      'UPZT_LB1SWAY_N_FREQ',
      'UPZT_LB1SWAY_N_GAIN',
      'UPZT_G1T_N_FREQ',
      'UPZT_G1T_N_GAIN',
      'UPZT_G2T_N_FREQ',
      'UPZT_G2T_N_GAIN',
      'UPZT_G3T_N_FREQ',
      'UPZT_G3T_N_GAIN',
      'UPZT_GYAW_N_FREQ',
      'UPZT_GYAW_N_GAIN',
      'UPZT_BP1T_N_FREQ',
      'UPZT_BP1T_N_GAIN',
      'PZTFRF_BASELINE_GAIN',
      'FID_HOLE_X1',
      'FID_HOLE_X2',
      'FID_HOLE_Y1',
      'FID_HOLE_Y2',
      'GIMBAL_PITCH_STIFFNESS',
      'GIMBAL_ROLL_STIFFNESS',
      'GIMBAL_1ST_FREQ',
      'GIMBAL_1ST_GAIN',
      'GIMBAL_ALIGN_X1',
      'GIMBAL_ALIGN_X2',
      'GIMBAL_ALIGN_X3',
      'GIMBAL_ALIGN_X4',
      'GIMBAL_ALIGN_X5',
      'GIMBAL_ALIGN_X6',
      'GIMBAL_ALIGN_X7',
      'GIMBAL_ALIGN_X8',
      'GIMBAL_ALIGN_X9',
      'GIMBAL_ALIGN_X10',
      'GIMBAL_ALIGN_X11',
      'GIMBAL_ALIGN_X12',
      'GIMBAL_ALIGN_X13',
      'GIMBAL_ALIGN_Y1',
      'GIMBAL_ALIGN_Y2',
      'GIMBAL_ALIGN_Y3',
      'GIMBAL_ALIGN_Y4',
      'GIMBAL_ALIGN_Y5',
      'GIMBAL_ALIGN_Y6',
      'GIMBAL_ALIGN_Y7',
      'GIMBAL_ALIGN_Y8',
      'GIMBAL_ALIGN_Y9',
      'GIMBAL_ALIGN_Y10',
      'GIMBAL_ALIGN_Y11',
      'GIMBAL_ALIGN_Y12',
      'GIMBAL_ALIGN_Y13',
      'GIMBAL_WIDTH_PAD1',
      'GIMBAL_WIDTH_PAD2',
      'GIMBAL_WIDTH_PAD3',
      'GIMBAL_WIDTH_PAD4',
      'GIMBAL_WIDTH_PAD5',
      'GIMBAL_WIDTH_PAD6',
      'GIMBAL_WIDTH_PAD7',
      'GIMBAL_WIDTH_PAD8',
      'GIMBAL_WIDTH_PAD9',
      'GIMBAL_WIDTH_PAD10',
      'GIMBAL_WIDTH_PAD11',
      'GIMBAL_WIDTH_PAD12',
      'GIMBAL_WIDTH_PAD13',
      'BASEPLATE_CUPPING_LOC_P',
      'BASEPLATE_CUPPING_LOC_Q',
      'BASEPLATE_CUPPING_LOC_R',
      'BASEPLATE_CUPPING_LOC_S'
    )
  
  setcolorder(qpm, cols_order)
  
  ##### TGA - HGA
  
  ### decode tga_lot_num, convert column names to upper (to be joined with qpm_aggr)
  rsnce[, tga_lot_num := ifelse(vendor == 'NHK-J', paste0(substr(fsa_lot_num, 9, 15), 'X'),
                                ifelse(vendor == 'NHK-T', paste0(substr(fsa_lot_num, 9, 16), 'X'),
                                       ifelse(vendor %like% 'MPT', mpt_1d2lot(fsa_lot_num),
                                              ifelse(vendor %like% 'HTO', substr(fsa_lot_num,14,17),NA))))]
  setnames(rsnce, toupper(names(rsnce))) 
  rsnce[1:2] ## this is hga resonance data both tester_type=vts/hts to be join with qpm_aggr (using tga_lot_num)
  
  ### process qpm data (group by tga_lot to be joined with hga rsnce data)
  num_vars <- c('GRAM_LOAD','PSA','RSA','ASSY_DIMPLE_POS_X','ASSY_DIMPLE_POS_Y','TAIL_ALIGN_X','TAIL_ALIGN_Y','ASSY_LIFT_TAB_OFFSET','STROKE','T1_NOTCH_LOC','LOAD_BEAM_TWIST','DCF','LEFT_PZT_CAPACITANCE','RIGHT_PZT_CAPACITANCE','LEFT_PZT_RESISTANCE','RIGHT_PZT_RESISTANCE','LEFT_PZT_CA_HEIGHT_BOTTOM','RIGHT_PZT_CA_HEIGHT_BOTTOM','LEFT_PZT_POSITION_X','RIGHT_PZT_POSITION_X','LEFT_PZT_POSITION_Y','RIGHT_PZT_POSITION_Y','COMP_PZT_LENGTH','COMP_PZT_WIDTH','COMP_PZT_THICKNESS','COMP_PZT_GAP3','COMP_PZT_GAP4','COMP_PZT_AU_THICKNESS','COMP_PZT_D31','COMP_LB_DIMPLE_POS_X','COMP_LB_DIMPLE_POS_Y','LB_1ST_TORSION_FREQ','LB_1ST_TORSION_GAIN','LB_1ST_SWAY_FREQ','LB_1ST_SWAY_GAIN','BOSS_ID','BOSS_OD','UPZT_LB1T_N_FREQ','UPZT_LB1T_N_GAIN','UPZT_LB1SWAY_N_FREQ','UPZT_LB1SWAY_N_GAIN','UPZT_G1T_N_FREQ','UPZT_G1T_N_GAIN','UPZT_G2T_N_FREQ','UPZT_G2T_N_GAIN','UPZT_GYAW_N_FREQ','UPZT_GYAW_N_GAIN','UPZT_BP1T_N_FREQ','UPZT_BP1T_N_GAIN','FID_HOLE_X1','FID_HOLE_X2','FID_HOLE_Y1','FID_HOLE_Y2','GIMBAL_1ST_FREQ','GIMBAL_1ST_GAIN','GIMBAL_ALIGN_X1','GIMBAL_ALIGN_X2','GIMBAL_ALIGN_X3','GIMBAL_ALIGN_X4','GIMBAL_ALIGN_X5','GIMBAL_ALIGN_X6','GIMBAL_ALIGN_X7','GIMBAL_ALIGN_X8','GIMBAL_ALIGN_X9','GIMBAL_ALIGN_X10','GIMBAL_ALIGN_X11','GIMBAL_ALIGN_Y1','GIMBAL_ALIGN_Y2','GIMBAL_ALIGN_Y3','GIMBAL_ALIGN_Y4','GIMBAL_ALIGN_Y5','GIMBAL_ALIGN_Y6','GIMBAL_ALIGN_Y7','GIMBAL_ALIGN_Y8','GIMBAL_ALIGN_Y9','GIMBAL_ALIGN_Y10','GIMBAL_ALIGN_Y11','GIMBAL_WIDTH_PAD1','GIMBAL_WIDTH_PAD2','GIMBAL_WIDTH_PAD3','GIMBAL_WIDTH_PAD4','GIMBAL_WIDTH_PAD5','GIMBAL_WIDTH_PAD6','GIMBAL_WIDTH_PAD7','GIMBAL_WIDTH_PAD8','GIMBAL_WIDTH_PAD9','GIMBAL_WIDTH_PAD10','GIMBAL_WIDTH_PAD11')
  attr_vars <- c('SUPPLIER_NAME','SITE','PRODUCT','PRODUCT_PART_NUM','PART_REV','ETL_LOAD_DATE','ETL_SOURCE_NAME','TGA_LOT_NUM','LAT_LOT_NUM','SBR_NUM','SUB_LOT_NUM','TGA_LOT_TYPE','TRAY_1D_BARCODE','RECORD_SEQ','RECORD_DATETIME','LINE_NUM','DRAWING_NUM','DRAWING_REV','BASEPLATE_LOT_NUM','CA_LOT_NUM','DAMPER_LOT_NUM','DAMPER_REEL_NUM','FLEXURE_LOT_NUM','LOADBEAM_LOT_NUM','NCA_LOT_NUM','PZT_NEGATIVE_LOT_NUM','PZT_NEGATIVE_WAFER_NUM','PZT_POSITIVE_LOT_NUM','PZT_POSITIVE_WAFER_NUM','INP_ATTR_FLAG','INP_BP_WASH','INP_BP_SORTER','INP_LB_WASH','INP_LB_FORM','INP_LASER_LAPPING','INP_FLEX_SEPARATION','INP_FLEX_PRE_SING','INP_FLEX_PANEL_TRIM','INP_FLEX_FINAL_SING','INP_FLEX_SORTER','INP_PZT_ATTACH','INP_FLEX_FORM_SORT','INP_LASER_WELD','INP_TWO_IN_ONE_DETAB','INP_WASHING1','INP_PL_FORM_FINAL_DETAB','INP_PL_FORM','INP_ANNEALING','INP_OVEN_CURE','INP_FINAL_DETAB','INP_WASHING2','INP_DAMPER_ATTACH','INP_GL_SA_ADJUST','INP_CAPA_RESIST')
  model_columns <- c(attr_vars, num_vars)
  
  ### subset qpm data for evansbp num_vars, attr_vars
  qpm_raw <- qpm[,.SD,.SDcol=model_columns]
  
  ### group columns
  grp_cols <- c('SUPPLIER_NAME', 'SITE', 'PRODUCT', 'PRODUCT_PART_NUM', 'TGA_LOT_NUM')
  
  ### concatenate multiple -or- single attribute(s) per grp_cols -- note this attr_cols is subset of attr_vars, some are not needed
  attr_cols <- c('LAT_LOT_NUM','SBR_NUM','SUB_LOT_NUM','TGA_LOT_TYPE','LINE_NUM','DRAWING_NUM','PART_REV','DRAWING_REV','BASEPLATE_LOT_NUM','CA_LOT_NUM','DAMPER_LOT_NUM','DAMPER_REEL_NUM','FLEXURE_LOT_NUM','LOADBEAM_LOT_NUM','NCA_LOT_NUM','PZT_NEGATIVE_LOT_NUM','PZT_NEGATIVE_WAFER_NUM','PZT_POSITIVE_LOT_NUM','PZT_POSITIVE_WAFER_NUM','INP_BP_WASH','INP_BP_SORTER','INP_LB_WASH','INP_LB_FORM','INP_LASER_LAPPING','INP_FLEX_SEPARATION','INP_FLEX_PRE_SING','INP_FLEX_PANEL_TRIM','INP_FLEX_FINAL_SING','INP_FLEX_SORTER','INP_PZT_ATTACH','INP_FLEX_FORM_SORT','INP_LASER_WELD','INP_TWO_IN_ONE_DETAB','INP_WASHING1','INP_PL_FORM_FINAL_DETAB','INP_PL_FORM','INP_ANNEALING','INP_OVEN_CURE','INP_FINAL_DETAB','INP_WASHING2','INP_DAMPER_ATTACH','INP_GL_SA_ADJUST','INP_CAPA_RESIST')
  system.time(qpm_attr <-
                qpm_raw[, c(
                  COUNT = length(.I), #same as .N with column renamed
                  RECORD_DATETIME = min(RECORD_DATETIME), # use min(record_datetime) per grp_cols
                  ETL_LOAD_DATE = min(ETL_LOAD_DATE), # use last etl_date per grp_cols
                  lapply(.SD, function(x) { # concatenate attrs per grp_cols, replace NA with '.'
                    tx <- sort(unique(na.omit(x)))
                    if (length(tx) == 0) {
                      '.'
                    } else {
                      paste(sprintf("%s", tx), collapse = ',')
                    }
                  })
                ),
                .SDcols = attr_cols,
                grp_cols])
  #   user  system elapsed
  # 34.251   0.088  34.265
  
  ### find group mean, sigma, n for parameter num_vars per grp_cols
  qpm_mean <- qpm_raw[, lapply(.SD, mean, na.rm = TRUE), .SDcols = num_vars, grp_cols]
  qpm_sd <- qpm_raw[, lapply(.SD, sd, na.rm = TRUE), .SDcols = num_vars, grp_cols]
  qpm_n <- qpm_raw[, lapply(.SD, length), .SDcols = num_vars, grp_cols]
  
  ### handle NaN, replace with NA (see is.NaN() function at top page)
  qpm_mean[is.NaN(qpm_mean)] <- NA # found many, need handle
  qpm_sd[is.NaN(qpm_sd)] <- NA # found none, but to be sured
  qpm_n[is.NaN(qpm_n)] <- NA # found none, but to be sured
  
  ### ensure all key columns are exactly equal for all qpm_attr, qpm_mean, qpm_sd, qpm_n
  # identical(qpm_attr[,.SD,.SDcols=grp_cols],qpm_mean[,.SD,.SDcols=grp_cols])
  # identical(qpm_attr[,.SD,.SDcols=grp_cols],qpm_sd[,.SD,.SDcols=grp_cols])
  # identical(qpm_attr[,.SD,.SDcols=grp_cols],qpm_n[,.SD,.SDcols=grp_cols])
  
  ### prepare dataset for column merge (attr, mean, sd, n)--remove duplicated grp_cols
  qpm_aggr <- qpm_attr
  qpm_mean[, (grp_cols) := NULL]
  qpm_sd[, (grp_cols) := NULL]
  qpm_n[, (grp_cols) := NULL]
  
  ### prefix mean, sd, n from 3 datasets
  setnames(qpm_mean, num_vars, paste0('mean.', num_vars))
  setnames(qpm_sd, num_vars, paste0('sd.', num_vars))
  setnames(qpm_n, num_vars, paste0('n.', num_vars))
  
  ### merge 4 datasets (qpm aggregate by tga_lot_num)
  qpm_aggr <- cbind(
    qpm_aggr,
    qpm_mean,
    qpm_sd,
    qpm_n
  )
  
  ### merge qpm_aggr by tga_lot with hga resonance aggr by tray_1d_barcode(fsa_lot_num) using 'tga_lot_num' key
  qpm_rsnce_join <- merge(rsnce, qpm_aggr, by='TGA_LOT_NUM', all.x = TRUE)
  qpm_rsnce_join <- qpm_rsnce_join[!is.na(PRODUCT)]
  setnames(qpm_rsnce_join, 'ETL_LOAD_DATE.y', 'ETL_LOAD_DATE')
  qpm_rsnce_join[1:2]
  
  ### write csv, or you can direct output to ftp server
  fwrite(qpm_rsnce_join, file = file.path(output_path, 'TGADashboardAnalysis@qpm_rsnce_join.csv'), quote = TRUE) # quote = TRUE required for tableau comma separated value within same attr_column
  
  ##### Reshape Data #####
  
  print("Reshaping Data")
  
  key_col <- c('TGA_LOT_NUM','VENDOR','CODENAME','TESTER_TYPE','FSA_LOT_NUM','TEST_DATE','ETL_LOAD_DATE.x','RADIUS','PARM_ID','TAB',
               'TESTER_ID','TEST_STATUS','WORK_ORDER','SPEC_VER','RSNCE_STATUS','RSNCE_ABOVE_12K','RSNCE_YIELD','PASS','SUPPLIER_NAME',
               'SITE','PRODUCT','PRODUCT_PART_NUM','COUNT','RECORD_DATETIME','ETL_LOAD_DATE','LAT_LOT_NUM','SBR_NUM','SUB_LOT_NUM',
               'TGA_LOT_TYPE','LINE_NUM','DRAWING_NUM','PART_REV','DRAWING_REV','BASEPLATE_LOT_NUM','CA_LOT_NUM','DAMPER_LOT_NUM',
               'DAMPER_REEL_NUM','FLEXURE_LOT_NUM','LOADBEAM_LOT_NUM','NCA_LOT_NUM','PZT_NEGATIVE_LOT_NUM','PZT_NEGATIVE_WAFER_NUM',
               'PZT_POSITIVE_LOT_NUM','PZT_POSITIVE_WAFER_NUM','INP_BP_WASH','INP_BP_SORTER','INP_LB_WASH','INP_LB_FORM','INP_LASER_LAPPING',
               'INP_FLEX_SEPARATION','INP_FLEX_PRE_SING','INP_FLEX_PANEL_TRIM','INP_FLEX_FINAL_SING','INP_FLEX_SORTER','INP_PZT_ATTACH',
               'INP_FLEX_FORM_SORT','INP_LASER_WELD','INP_TWO_IN_ONE_DETAB','INP_WASHING1','INP_PL_FORM_FINAL_DETAB','INP_PL_FORM','INP_ANNEALING',
               'INP_OVEN_CURE','INP_FINAL_DETAB','INP_WASHING2','INP_DAMPER_ATTACH','INP_GL_SA_ADJUST','INP_CAPA_RESIST')
  
  var_col <- c('GT1_FREQ','GT1_GAIN','GT2_FREQ','GT2_GAIN','BP_FREQ','BP_GAIN','SWAY_FREQ','SWAY_GAIN','GT4_FREQ','GT4_GAIN','YAW_FREQ','YAW_GAIN','UACT_STROKE','PSA_IN','PSA_OUT','RSA_IN','RSA_OUT',
               'mean.GRAM_LOAD','mean.PSA','mean.RSA','mean.ASSY_DIMPLE_POS_X','mean.ASSY_DIMPLE_POS_Y','mean.TAIL_ALIGN_X','mean.TAIL_ALIGN_Y','mean.ASSY_LIFT_TAB_OFFSET','mean.STROKE','mean.T1_NOTCH_LOC',
               'mean.LOAD_BEAM_TWIST','mean.DCF','mean.LEFT_PZT_CAPACITANCE','mean.RIGHT_PZT_CAPACITANCE','mean.LEFT_PZT_RESISTANCE','mean.RIGHT_PZT_RESISTANCE','mean.LEFT_PZT_CA_HEIGHT_BOTTOM',
               'mean.RIGHT_PZT_CA_HEIGHT_BOTTOM','mean.LEFT_PZT_POSITION_X','mean.RIGHT_PZT_POSITION_X','mean.LEFT_PZT_POSITION_Y','mean.RIGHT_PZT_POSITION_Y','mean.COMP_PZT_LENGTH','mean.COMP_PZT_WIDTH',
               'mean.COMP_PZT_THICKNESS','mean.COMP_PZT_GAP3','mean.COMP_PZT_GAP4','mean.COMP_PZT_AU_THICKNESS','mean.COMP_PZT_D31','mean.COMP_LB_DIMPLE_POS_X','mean.COMP_LB_DIMPLE_POS_Y','mean.LB_1ST_TORSION_FREQ',
               'mean.LB_1ST_TORSION_GAIN','mean.LB_1ST_SWAY_FREQ','mean.LB_1ST_SWAY_GAIN','mean.BOSS_ID','mean.BOSS_OD','mean.UPZT_LB1T_N_FREQ','mean.UPZT_LB1T_N_GAIN','mean.UPZT_LB1SWAY_N_FREQ','mean.UPZT_LB1SWAY_N_GAIN',
               'mean.UPZT_G1T_N_FREQ','mean.UPZT_G1T_N_GAIN','mean.UPZT_G2T_N_FREQ','mean.UPZT_G2T_N_GAIN','mean.UPZT_GYAW_N_FREQ','mean.UPZT_GYAW_N_GAIN','mean.UPZT_BP1T_N_FREQ','mean.UPZT_BP1T_N_GAIN','mean.FID_HOLE_X1',
               'mean.FID_HOLE_X2','mean.FID_HOLE_Y1','mean.FID_HOLE_Y2','mean.GIMBAL_1ST_FREQ','mean.GIMBAL_1ST_GAIN','mean.GIMBAL_ALIGN_X1','mean.GIMBAL_ALIGN_X2','mean.GIMBAL_ALIGN_X3','mean.GIMBAL_ALIGN_X4',
               'mean.GIMBAL_ALIGN_X5','mean.GIMBAL_ALIGN_X6','mean.GIMBAL_ALIGN_X7','mean.GIMBAL_ALIGN_X8','mean.GIMBAL_ALIGN_X9','mean.GIMBAL_ALIGN_X10','mean.GIMBAL_ALIGN_X11','mean.GIMBAL_ALIGN_Y1','mean.GIMBAL_ALIGN_Y2',
               'mean.GIMBAL_ALIGN_Y3','mean.GIMBAL_ALIGN_Y4','mean.GIMBAL_ALIGN_Y5','mean.GIMBAL_ALIGN_Y6','mean.GIMBAL_ALIGN_Y7','mean.GIMBAL_ALIGN_Y8','mean.GIMBAL_ALIGN_Y9','mean.GIMBAL_ALIGN_Y10','mean.GIMBAL_ALIGN_Y11',
               'mean.GIMBAL_WIDTH_PAD1','mean.GIMBAL_WIDTH_PAD2','mean.GIMBAL_WIDTH_PAD3','mean.GIMBAL_WIDTH_PAD4','mean.GIMBAL_WIDTH_PAD5','mean.GIMBAL_WIDTH_PAD6','mean.GIMBAL_WIDTH_PAD7','mean.GIMBAL_WIDTH_PAD8',
               'mean.GIMBAL_WIDTH_PAD9','mean.GIMBAL_WIDTH_PAD10','mean.GIMBAL_WIDTH_PAD11','sd.GRAM_LOAD','sd.PSA','sd.RSA','sd.ASSY_DIMPLE_POS_X','sd.ASSY_DIMPLE_POS_Y','sd.TAIL_ALIGN_X','sd.TAIL_ALIGN_Y',
               'sd.ASSY_LIFT_TAB_OFFSET','sd.STROKE','sd.T1_NOTCH_LOC','sd.LOAD_BEAM_TWIST','sd.DCF','sd.LEFT_PZT_CAPACITANCE','sd.RIGHT_PZT_CAPACITANCE','sd.LEFT_PZT_RESISTANCE','sd.RIGHT_PZT_RESISTANCE',
               'sd.LEFT_PZT_CA_HEIGHT_BOTTOM','sd.RIGHT_PZT_CA_HEIGHT_BOTTOM','sd.LEFT_PZT_POSITION_X','sd.RIGHT_PZT_POSITION_X','sd.LEFT_PZT_POSITION_Y','sd.RIGHT_PZT_POSITION_Y','sd.COMP_PZT_LENGTH','sd.COMP_PZT_WIDTH',
               'sd.COMP_PZT_THICKNESS','sd.COMP_PZT_GAP3','sd.COMP_PZT_GAP4','sd.COMP_PZT_AU_THICKNESS','sd.COMP_PZT_D31','sd.COMP_LB_DIMPLE_POS_X','sd.COMP_LB_DIMPLE_POS_Y','sd.LB_1ST_TORSION_FREQ','sd.LB_1ST_TORSION_GAIN',
               'sd.LB_1ST_SWAY_FREQ','sd.LB_1ST_SWAY_GAIN','sd.BOSS_ID','sd.BOSS_OD','sd.UPZT_LB1T_N_FREQ','sd.UPZT_LB1T_N_GAIN','sd.UPZT_LB1SWAY_N_FREQ','sd.UPZT_LB1SWAY_N_GAIN','sd.UPZT_G1T_N_FREQ','sd.UPZT_G1T_N_GAIN',
               'sd.UPZT_G2T_N_FREQ','sd.UPZT_G2T_N_GAIN','sd.UPZT_GYAW_N_FREQ','sd.UPZT_GYAW_N_GAIN','sd.UPZT_BP1T_N_FREQ','sd.UPZT_BP1T_N_GAIN','sd.FID_HOLE_X1','sd.FID_HOLE_X2','sd.FID_HOLE_Y1','sd.FID_HOLE_Y2',
               'sd.GIMBAL_1ST_FREQ','sd.GIMBAL_1ST_GAIN','sd.GIMBAL_ALIGN_X1','sd.GIMBAL_ALIGN_X2','sd.GIMBAL_ALIGN_X3','sd.GIMBAL_ALIGN_X4','sd.GIMBAL_ALIGN_X5','sd.GIMBAL_ALIGN_X6','sd.GIMBAL_ALIGN_X7','sd.GIMBAL_ALIGN_X8',
               'sd.GIMBAL_ALIGN_X9','sd.GIMBAL_ALIGN_X10','sd.GIMBAL_ALIGN_X11','sd.GIMBAL_ALIGN_Y1','sd.GIMBAL_ALIGN_Y2','sd.GIMBAL_ALIGN_Y3','sd.GIMBAL_ALIGN_Y4','sd.GIMBAL_ALIGN_Y5','sd.GIMBAL_ALIGN_Y6','sd.GIMBAL_ALIGN_Y7',
               'sd.GIMBAL_ALIGN_Y8','sd.GIMBAL_ALIGN_Y9','sd.GIMBAL_ALIGN_Y10','sd.GIMBAL_ALIGN_Y11','sd.GIMBAL_WIDTH_PAD1','sd.GIMBAL_WIDTH_PAD2','sd.GIMBAL_WIDTH_PAD3','sd.GIMBAL_WIDTH_PAD4','sd.GIMBAL_WIDTH_PAD5',
               'sd.GIMBAL_WIDTH_PAD6','sd.GIMBAL_WIDTH_PAD7','sd.GIMBAL_WIDTH_PAD8','sd.GIMBAL_WIDTH_PAD9','sd.GIMBAL_WIDTH_PAD10','sd.GIMBAL_WIDTH_PAD11','CNT_HD','n.GRAM_LOAD','n.PSA','n.RSA','n.ASSY_DIMPLE_POS_X',
               'n.ASSY_DIMPLE_POS_Y','n.TAIL_ALIGN_X','n.TAIL_ALIGN_Y','n.ASSY_LIFT_TAB_OFFSET','n.STROKE','n.T1_NOTCH_LOC','n.LOAD_BEAM_TWIST','n.DCF','n.LEFT_PZT_CAPACITANCE','n.RIGHT_PZT_CAPACITANCE','n.LEFT_PZT_RESISTANCE',
               'n.RIGHT_PZT_RESISTANCE','n.LEFT_PZT_CA_HEIGHT_BOTTOM','n.RIGHT_PZT_CA_HEIGHT_BOTTOM','n.LEFT_PZT_POSITION_X','n.RIGHT_PZT_POSITION_X','n.LEFT_PZT_POSITION_Y','n.RIGHT_PZT_POSITION_Y','n.COMP_PZT_LENGTH',
               'n.COMP_PZT_WIDTH','n.COMP_PZT_THICKNESS','n.COMP_PZT_GAP3','n.COMP_PZT_GAP4','n.COMP_PZT_AU_THICKNESS','n.COMP_PZT_D31','n.COMP_LB_DIMPLE_POS_X','n.COMP_LB_DIMPLE_POS_Y','n.LB_1ST_TORSION_FREQ',
               'n.LB_1ST_TORSION_GAIN','n.LB_1ST_SWAY_FREQ','n.LB_1ST_SWAY_GAIN','n.BOSS_ID','n.BOSS_OD','n.UPZT_LB1T_N_FREQ','n.UPZT_LB1T_N_GAIN','n.UPZT_LB1SWAY_N_FREQ','n.UPZT_LB1SWAY_N_GAIN','n.UPZT_G1T_N_FREQ',
               'n.UPZT_G1T_N_GAIN','n.UPZT_G2T_N_FREQ','n.UPZT_G2T_N_GAIN','n.UPZT_GYAW_N_FREQ','n.UPZT_GYAW_N_GAIN','n.UPZT_BP1T_N_FREQ','n.UPZT_BP1T_N_GAIN','n.FID_HOLE_X1','n.FID_HOLE_X2','n.FID_HOLE_Y1','n.FID_HOLE_Y2',
               'n.GIMBAL_1ST_FREQ','n.GIMBAL_1ST_GAIN','n.GIMBAL_ALIGN_X1','n.GIMBAL_ALIGN_X2','n.GIMBAL_ALIGN_X3','n.GIMBAL_ALIGN_X4','n.GIMBAL_ALIGN_X5','n.GIMBAL_ALIGN_X6','n.GIMBAL_ALIGN_X7','n.GIMBAL_ALIGN_X8',
               'n.GIMBAL_ALIGN_X9','n.GIMBAL_ALIGN_X10','n.GIMBAL_ALIGN_X11','n.GIMBAL_ALIGN_Y1','n.GIMBAL_ALIGN_Y2','n.GIMBAL_ALIGN_Y3','n.GIMBAL_ALIGN_Y4','n.GIMBAL_ALIGN_Y5','n.GIMBAL_ALIGN_Y6','n.GIMBAL_ALIGN_Y7',
               'n.GIMBAL_ALIGN_Y8','n.GIMBAL_ALIGN_Y9','n.GIMBAL_ALIGN_Y10','n.GIMBAL_ALIGN_Y11','n.GIMBAL_WIDTH_PAD1','n.GIMBAL_WIDTH_PAD2','n.GIMBAL_WIDTH_PAD3','n.GIMBAL_WIDTH_PAD4','n.GIMBAL_WIDTH_PAD5','n.GIMBAL_WIDTH_PAD6',
               'n.GIMBAL_WIDTH_PAD7','n.GIMBAL_WIDTH_PAD8','n.GIMBAL_WIDTH_PAD9','n.GIMBAL_WIDTH_PAD10','n.GIMBAL_WIDTH_PAD11','FRF_12KHZ_HIGAIN','FRF_12KHZ_DIRECT','FRF_16KHZ','FRF_20KHZ','FRF_36KHZ_ALIAS','FRF_12KHZ_ALIAS','VARITY_DEFECT')
  
  qpm_rsnce_join <- qpm_rsnce_join[,mget(c(key_col, var_col))]
  
  print(paste0("Before Reshaping Data rows cnt: ", nrow(qpm_rsnce_join)))
  
  # data <- head(data, 1000)
  
  qpm_rsnce_join <- melt(qpm_rsnce_join, id.vars = key_col)
  
  print(paste0("After Reshaping Data rows cnt: ", nrow(qpm_rsnce_join)))
  
  qpm_rsnce_join <- qpm_rsnce_join[complete.cases(qpm_rsnce_join$value),]
  
  print(paste0("After Reshaping Data and remove na rows cnt: ", nrow(qpm_rsnce_join)))
  
  naming_case = rlang::exprs(
    .$variable %in% c('GT1_FREQ','GT1_GAIN','GT2_FREQ','GT2_GAIN','BP_FREQ','BP_GAIN','SWAY_FREQ','SWAY_GAIN','GT4_FREQ','GT4_GAIN','YAW_FREQ','YAW_GAIN','UACT_STROKE','PSA_IN','PSA_OUT','RSA_IN','RSA_OUT') ~ 'CTQ_AVG',
    grepl('mean.',.$variable) ~ 'CTQ_AVG',
    grepl('sd.',.$variable) ~ 'CTQ_SD',
    .$variable %in% c('CNT_HD','FRF_12KHZ_HIGAIN','FRF_12KHZ_DIRECT','FRF_16KHZ','FRF_20KHZ','FRF_36KHZ_ALIAS','FRF_12KHZ_ALIAS','VARITY_DEFECT') ~ 'CTQ_N',
    grepl('n.',.$variable) ~ 'CTQ_N'
  )
  
  ctq_naming_case = rlang::exprs(
    .$variable %in% c('GT1_FREQ','GT1_GAIN','GT2_FREQ','GT2_GAIN','BP_FREQ','BP_GAIN','SWAY_FREQ','SWAY_GAIN','GT4_FREQ','GT4_GAIN','YAW_FREQ','YAW_GAIN','UACT_STROKE','PSA_IN','PSA_OUT','RSA_IN','RSA_OUT','CNT_HD') ~ paste0('HGA.', .$variable),
    .$variable %in% c('FRF_12KHZ_HIGAIN','FRF_12KHZ_DIRECT','FRF_16KHZ','FRF_20KHZ','FRF_36KHZ_ALIAS','FRF_12KHZ_ALIAS','VARITY_DEFECT') ~ paste0('FRF.', .$variable), 
    grepl('mean.|sd.|n.',.$variable)  ~ paste0('TGA.', stringr::str_remove_all(.$variable, "mean.|sd.|n.")) 
  )
  
  qpm_rsnce_join <- qpm_rsnce_join %>% dplyr::mutate(CTQ_NAME = case_when(!!! ctq_naming_case), variable = case_when(!!! naming_case))
  
  qpm_rsnce_join <- dcast(qpm_rsnce_join, TGA_LOT_NUM+VENDOR+CODENAME+TESTER_TYPE+FSA_LOT_NUM+TEST_DATE+ETL_LOAD_DATE.x+RADIUS+PARM_ID+TAB+TESTER_ID+TEST_STATUS+WORK_ORDER+SPEC_VER+RSNCE_STATUS+RSNCE_ABOVE_12K+RSNCE_YIELD+PASS+SUPPLIER_NAME+SITE+PRODUCT+PRODUCT_PART_NUM+COUNT+RECORD_DATETIME+ETL_LOAD_DATE+LAT_LOT_NUM+SBR_NUM+SUB_LOT_NUM+TGA_LOT_TYPE+LINE_NUM+DRAWING_NUM+PART_REV+DRAWING_REV+BASEPLATE_LOT_NUM+CA_LOT_NUM+DAMPER_LOT_NUM+DAMPER_REEL_NUM+FLEXURE_LOT_NUM+LOADBEAM_LOT_NUM+NCA_LOT_NUM+PZT_NEGATIVE_LOT_NUM+PZT_NEGATIVE_WAFER_NUM+PZT_POSITIVE_LOT_NUM+PZT_POSITIVE_WAFER_NUM+INP_BP_WASH+INP_BP_SORTER+INP_LB_WASH+INP_LB_FORM+INP_LASER_LAPPING+INP_FLEX_SEPARATION+INP_FLEX_PRE_SING+INP_FLEX_PANEL_TRIM+INP_FLEX_FINAL_SING+INP_FLEX_SORTER+INP_PZT_ATTACH+INP_FLEX_FORM_SORT+INP_LASER_WELD+INP_TWO_IN_ONE_DETAB+INP_WASHING1+INP_PL_FORM_FINAL_DETAB+INP_PL_FORM+INP_ANNEALING+INP_OVEN_CURE+INP_FINAL_DETAB+INP_WASHING2+INP_DAMPER_ATTACH+INP_GL_SA_ADJUST+INP_CAPA_RESIST+CTQ_NAME ~ variable)
  
  qpm_rsnce_join[qpm_rsnce_join$CTQ_NAME %in% c("FRF.FRF_12KHZ_HIGAIN", "FRF.FRF_12KHZ_DIRECT", "FRF.FRF_16KHZ", "FRF.FRF_20KHZ", "FRF.FRF_36KHZ_ALIAS", "FRF.FRF_12KHZ_ALIAS", "FRF.VARITY_DEFECT") & qpm_rsnce_join$CTQ_N > 0,]$CTQ_AVG <- 1
  
  print(paste0("Final Reshaping Data rows cnt: ", nrow(qpm_rsnce_join)))
  
  qpm_rsnce_join <- unique(qpm_rsnce_join)
  
  print(paste0("Final Reshaping Data unique rows cnt: ", nrow(qpm_rsnce_join)))
  
  qpm_rsnce_join <- qpm_rsnce_join[qpm_rsnce_join$ETL_LOAD_DATE.x > unique(Sys.Date() - 62),]
  
  print(paste0("Only latest two month data : ", nrow(qpm_rsnce_join)))
  
  fwrite(qpm_rsnce_join, file = file.path(output_path, "Reshape_data.csv"), quote = TRUE)
  
}