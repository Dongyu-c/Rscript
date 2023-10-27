custom_data_prep = function(input_path, output_path){

	library(dplyr)
  
	QPM_SPEC_folder <- file.path(ResultsPath, "E1082262", "QPM_transfer")
	
	SPEC <- readRDS(file.path(QPM_SPEC_folder, "QPM_SPEC.rda"))
	SUP_DIC <- readRDS(file.path(QPM_SPEC_folder, "QPM_SUP_DIC.rda"))

	SPEC <- SPEC[,c("COMMODITY", "PART_NUM", "SUPPLIER_NAME", "QPM_PARAMETER", "Key_CTQ",  "Target", "USL", "LSL", "UPDATE_DATE", "SUPPLIER_CODE")]

	SPEC[SPEC$COMMODITY %in% 'MO',]$COMMODITY <- 'MOTOR'
	SPEC$HAVE_SPEC <- "YES"
	SPEC$QPM_PARAMETER <- toupper(SPEC$QPM_PARAMETER)
	SPEC <- SPEC[,!names(SPEC) %in% 'SUPPLIER_CODE']
	
	QPM_LAT <- read.csv(file.path(ResultsPath, "E1087845", "SPEC_CHECKER_LAT.csv"), stringsAsFactors = F)

	# QPM_LAT$QPM_DATA_QUERY_TYPE <- "LAT"

	QPM_INP <- read.csv(file.path(ResultsPath, "E1092865", "SPEC_CHECKER_INP.csv"), stringsAsFactors = F)

	# QPM_INP$QPM_DATA_QUERY_TYPE <- "INP"

	QPM_COM <- rbind(QPM_LAT, QPM_INP)
	
	QPM_COM <- unique(QPM_COM)
	
	QPM_PARAMETER_DIC <- readRDS(file.path(ResultsPath, "E1111203", "QPM_PARAMETER_DIC.rda")) 
	
	QPM_INP <- read.csv(file.path(ResultsPath, "E1092865", "SPEC_CHECKER_INP.csv"), stringsAsFactors = F)
	
	QPM_COL_DIC <- read.csv(file.path(ResultsPath, "E1082262", "QPM_COL_DIC.csv"), stringsAsFactors = F)

	QPM_COL_DIC <- QPM_COL_DIC[QPM_COL_DIC$PARM_ATTR_TYPE %in% 'A' & QPM_COL_DIC$STATUS %in% 'Y', c('COMMODITY_TYPE', 'TARGET_TABLE_NAME', 'PARM_ATTR_NAME_NEW')]

	QPM_COL_DIC <- unique(QPM_COL_DIC)

	names(QPM_COL_DIC)[3] <- "PARARMETER_NAME.IN.EDW"
	
	QPM_COL_DIC <- left_join(QPM_COL_DIC, QPM_PARAMETER_DIC)

	remove_list = c("ACA_LINE_NUM", "ACTTR_CBORE_PSN", "ACTTR_HOLE_PSN", "ACTTR_POST_PSN", 
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
                      "MOTOR_ID_ROUNDNESS", "MOTOR_ID_TRUE_PSN", "MOTOR_OD_TRUE_PSN", 
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
                      "VCM_TOP_THREAD_PSN","LOT_NUM", "RECORD_SEQ", "PART_NUM.1", "SUPPLIER_KEY", "MACHINE_NUM", "NA.", "SAMPLE_QTY", "SUPPLIER_KEY.1")
					  
	remove_list = c(remove_list, unique(QPM_PARAMETER_DIC[QPM_PARAMETER_DIC$`ATTRIBUTE?`%in% 'Y',]$PARARMETER_NAME.IN.DFD))
	
	remove_list = unique(remove_list, unique(QPM_COL_DIC$PARARMETER_NAME.IN.DFD))
	
	rm(QPM_PARAMETER_DIC)
	
	####
	
	QPM_COM <- QPM_COM[!grepl('NAME|SPARE', QPM_COM$QPM_PARAMETER),]
	
	####
	
	QPM_COM <- QPM_COM[!QPM_COM$QPM_PARAMETER %in% remove_list,]
	
	write.csv(QPM_COM, file = file.path(output_path, "Spec_table_com.csv"), row.names = F)

	QPM_COM <- QPM_COM[complete.cases(QPM_COM$SUPPLIER_NAME),]

	QPM_COM <- left_join(QPM_COM, SUP_DIC)

	QPM_COM <- left_join(QPM_COM, SPEC)

	QPM_COM <- QPM_COM %>% mutate(HAVE_SPEC = ifelse(is.na(HAVE_SPEC), 'NO', 'YES'))
	
	##### Add in addtionall data #####
	
	CTQ_by_commodity <- unique(QPM_COM[QPM_COM$Key_CTQ %in% 'YES',c("COMMODITY", "QPM_PARAMETER", "Key_CTQ")])
	
	NO_CTQ_by_commodity <- unique(QPM_COM[QPM_COM$Key_CTQ %in% 'NO',c("COMMODITY", "QPM_PARAMETER", "Key_CTQ")])

	NO_CTQ_by_commodity <- anti_join(NO_CTQ_by_commodity, CTQ_by_commodity, by = c("COMMODITY", "QPM_PARAMETER"))

	CTQ_by_commodity <- bind_rows(CTQ_by_commodity, NO_CTQ_by_commodity)
	
	names(CTQ_by_commodity)[3] <- "Missing_KEY_CTQ"
	CTQ_by_commodity$HAVE_SPEC <- "NO"
	
	QPM_COM <- left_join(QPM_COM, CTQ_by_commodity) 
	
	QPM_COM <- QPM_COM %>% mutate(Missing_KEY_CTQ = ifelse(HAVE_SPEC == 'YES' & Key_CTQ %in% c('YES', 'NO'), "NO", Missing_KEY_CTQ)) 
	
	QPM_COM <- QPM_COM %>% 
	  group_by(COMMODITY, PART_NUM) %>% 
	  mutate(PART_NUM_ALL_NO_SPEC = ifelse(all(HAVE_SPEC %in% 'NO'), "ALL MISSING", ifelse(all(HAVE_SPEC %in% 'YES'), "NO MISSING", "PARTIALLY MISSING"))) 

	DIC = readRDS(file.path(QPM_SPEC_folder, "QPM_DIC.rda"))
	
	DIC = unique(DIC)
	DIC[] = lapply(DIC, as.character)
	DIC$PART_NUM = stringr::str_remove_all(DIC$PART_NUM,c("\\bXXX|\\b "))
	DIC$ODT_PRODUCT_NAME = toupper(DIC$ODT_PRODUCT_NAME)
	DIC[DIC==""] = "N/A"

	DIC$PRODUCT_TYPE[DIC$PRODUCT_TYPE=="MO"] = "MOTOR"
	DIC$PRODUCT_TYPE[DIC$PRODUCT_TYPE=="SEALEDDRIVEBASEPLA"] = "SEALEDDRIVEBASEPLATE"
	ccp_com = unique(DIC$PRODUCT_TYPE[grep('CCP',DIC$PRODUCT_TYPE)])
	DIC = as.data.table(DIC)
	DIC = DIC[!(PRODUCT_TYPE %in% c(ccp_com))]
	DIC = as.data.frame(DIC)

	DIC = DIC[,c('ODT_PRODUCT_NAME', 'PART_NUM', 'PRODUCT_TYPE')]
	names(DIC) = c("PRODUCT_INTERNAL_NAME", "PART_NUM", "COMMODITY")

	DIC = DIC[order(DIC$COMMODITY, DIC$PART_NUM, DIC$PRODUCT_INTERNAL_NAME),]
	
	QPM_COM = left_join(QPM_COM, DIC)
	
	write.csv(QPM_COM, file = file.path(output_path, "QPMmissingCTQ@Spec_table.csv"), row.names = F)
	
} 