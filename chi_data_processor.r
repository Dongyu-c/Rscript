custom_data_prep = function(input_path, output_path){
  
  process_list = c("CHI_CIMARRON_RAW.rda", "CHI_CIMARRONBP_RAW.rda", "CHI_SKYBOLT_RAW.rda")
  
  chi_wash_eff <- read.csv(file = file.path(input_path, "CHI_WASH_SPEC.csv"), stringsAsFactors = T)

  names(chi_wash_eff)[which(names(chi_wash_eff) == "build_line")] <- 'line_num'
  names(chi_wash_eff)[which(names(chi_wash_eff) == "product_detail_name")] <- 'product_name'

  chi_wash_eff$line_num <- stringr::str_replace_all(chi_wash_eff$line_num, "L", '')
  chi_wash_eff$product_name <- stringr::str_replace_all(toupper(chi_wash_eff$product_name), ' ', '_')
  
  print("chi_wash_eff end")
  
  for(loop in 1:length(process_list)){
    
	tryCatch({
	
		print(paste("read file:", process_list[loop]))
	
		chi_data <- readRDS(file.path(input_path, process_list[loop]))

		chi_data <- chi_data %>% 
		  mutate_at(.vars = c("particle_lpc_03_mba", "particle_lpc_03_vcmb", "particle_lpc_03_vcmt",  "particle_lpc_03_stmcv", "particle_lpc_03_hsa", "hsi", "bco", "dsi", "blm", "hsr", "csr", "blp", "csi", "lai", "hmm", "tci", "bud", "dsr", "bdl", "lcr", "pri", "ucr", "sni",  "cab", "uvr", "bcu", "scr", "blv",  "uai"), funs(as.numeric(.)))
		
		drive_particle_idx <- chi_data[,c("drive_serial_num", "product_internal_name", "product_name", "run_type", "line_num", "dom")]
		
		##### Motor Cal #####
		
		print("##### Motor Cal #####")
		
		mba_data_raw <- chi_data[,c("drive_serial_num", "product_name", "run_type", "line_num", "dom", "mba_run_type", "mba_part_num", "mba_vend", "dom_mba", "particle_lpc_03_mba")]
		
		mba_data_raw <- unique(mba_data_raw)
		
		mba_data_raw <- left_join(mba_data_raw, chi_wash_eff[chi_wash_eff$product %in% 'MBA',])
		
		mba_data_raw <- mba_data_raw %>% group_by(drive_serial_num, run_type, line_num, dom) %>% dplyr::summarise(mba_par_idx = particle_lpc_03_mba * surface_area)
		
		drive_particle_idx <- unique(left_join(drive_particle_idx, mba_data_raw))
		
		rm(mba_data_raw)
		
		##### Bot Cover Cal #####
		
		print("##### Bot Cover Cal #####")
		
		vcmb_data_raw <- chi_data[,c("drive_serial_num", "product_name", "run_type", "line_num", "dom", "vcmb_run_type", "vcmb_part_num", "vcmb_vend", "dom_vcmb", "particle_lpc_03_vcmb")]
		
		vcmb_data_raw <- unique(vcmb_data_raw)
		
		vcmb_data_raw <- left_join(vcmb_data_raw, chi_wash_eff[chi_wash_eff$product %in% 'Bottom VCM',])
		
		vcmb_data_raw <- vcmb_data_raw %>% group_by(drive_serial_num, run_type, line_num, dom) %>% dplyr::summarise(vcmb_par_idx = particle_lpc_03_vcmb * surface_area * lpc_alpc_coefficient)
		
		drive_particle_idx <- unique(left_join(drive_particle_idx, vcmb_data_raw))
		
		rm(vcmb_data_raw)
		
		##### Top Cover Cal #####
		
		print("##### Top Cover Cal #####")
		
		vcmt_data_raw <- chi_data[,c("drive_serial_num", "product_name", "run_type", "line_num", "dom", "vcmt_run_type", "vcmt_part_num", "vcmt_vend", "dom_vcmt", "particle_lpc_03_vcmt")]
		
		vcmt_data_raw <- unique(vcmt_data_raw)
		
		vcmt_data_raw <- left_join(vcmt_data_raw, chi_wash_eff[chi_wash_eff$product %in% 'Upper VCM',])
		
		vcmt_data_raw <- vcmt_data_raw %>% group_by(drive_serial_num, run_type, line_num, dom) %>% dplyr::summarise(vcmt_par_idx = particle_lpc_03_vcmt * surface_area * lpc_alpc_coefficient)
		
		drive_particle_idx <- unique(left_join(drive_particle_idx, vcmt_data_raw))
		
		rm(vcmt_data_raw)
		
		##### STMCV Cal #####
		
		print("##### STMCV Cal #####")
		
		stmcv_data_raw <- chi_data[,c("drive_serial_num", "product_name", "run_type", "line_num", "dom", "stmcv_part_num", "stmcv_vend", "dom_stmcv", "particle_lpc_03_stmcv")]
		
		stmcv_data_raw <- unique(stmcv_data_raw)
		
		# stmcv_data_raw$stmcv_run_type <- "prime"
		# stmcv_data_raw <- stmcv_data_raw[, c(1, 8, 2:7)]
		
		stmcv_data_raw <- left_join(stmcv_data_raw, chi_wash_eff[chi_wash_eff$product %in% 'Top Cover',])
		
		stmcv_data_raw <- stmcv_data_raw %>% group_by(drive_serial_num, run_type, line_num, dom) %>% dplyr::summarise(stmcv_par_idx = particle_lpc_03_stmcv * surface_area * lpc_alpc_coefficient * `prime`)
		
		drive_particle_idx <- unique(left_join(drive_particle_idx, stmcv_data_raw))
		
		rm(stmcv_data_raw)
		
		##### HSA Cal #####
		
		print("##### HSA Cal #####")
		
		hsa_data_raw <- chi_data[,c("drive_serial_num", "product_name", "run_type", "line_num", "dom", "hsa_run_type", "hsa_part_num", "hsa_vend", "dom_hsa", "particle_lpc_03_hsa")]
		
		hsa_data_raw <- unique(hsa_data_raw)
		
		hsa_data_raw <- left_join(hsa_data_raw, chi_wash_eff[chi_wash_eff$product %in% 'HSA',])
		
		hsa_data_raw <- hsa_data_raw %>% group_by(drive_serial_num, run_type, line_num, dom, dom_hsa) %>% dplyr::summarise(hsa_par_idx = particle_lpc_03_hsa * surface_area)
		
		drive_particle_idx <- unique(left_join(drive_particle_idx, hsa_data_raw))
		
		rm(hsa_data_raw)
		
		##### Final Summary Cal #####
		
		print("##### Final Summary Cal #####")
		
		# drive_particle_idx <- drive_particle_idx %>% group_by(drive_serial_num, product_internal_name, run_type, line_num, dom) %>% dplyr::summarise(remove_index = , particle_index = sum(mba_par_idx, vcmb_par_idx,  vcmt_par_idx, stmcv_par_idx, hsa_par_idx, na.rm = t))
		
		drive_particle_idx <- drive_particle_idx %>% 
								group_by(drive_serial_num, product_internal_name, run_type, line_num, dom) %>% 
								dplyr::summarise(particle_index = sum(mba_par_idx, vcmb_par_idx,  vcmt_par_idx, stmcv_par_idx, hsa_par_idx, na.rm = t),
												 missing_ccp = unique(!(complete.cases(mba_par_idx) & !complete.cases(vcmb_par_idx) & !complete.cases(vcmt_par_idx) & !complete.cases(stmcv_par_idx) & !complete.cases(hsa_par_idx))))
												 
		if (any(drive_particle_idx$missing_ccp %in% T)) {
  
		  drive_particle_idx[drive_particle_idx$missing_ccp %in% T,]$particle_index <- NA
		  
		}
		
		##### RTPAD #####
		
		print("##### RTPAD #####")
		
		drive_rtpad_idx <- unique(chi_data[,c("drive_serial_num", "product_internal_name", "product_name", "run_type", "line_num", "dom", "rtpad_tot_prtcl_cnt")])
		
		names(drive_rtpad_idx)[7] <- "rtpad"
		
		drive_particle_idx <- unique(left_join(drive_particle_idx, drive_rtpad_idx))
		
		rm(drive_rtpad_idx)
		
		##### LMS #####
		
		print("##### LMS #####")
		
		drive_lms_idx <- chi_data %>% filter(complete.cases(sn)) %>% select("sn", "hsi", "bco", "dsi", "blm", "hsr", "csr", "blp", "csi", "lai", "hmm", "tci", "bud", "dsr", "bdl", "lcr", "pri", "ucr", "sni",  "cab", "uvr", "bcu", "scr", "blv",  "uai")
		
		drive_lms_idx <- unique(drive_lms_idx)
		
		drive_lms_idx <- drive_lms_idx %>% group_by(sn) %>% mutate(lms_sum = sum(c(hsi, bco, dsi, blm, hsr, csr, blp, csi, lai, hmm, tci, bud, dsr, bdl, lcr, pri, ucr, sni, cab, uvr, bcu, scr, blv, uai), na.rm = T))
		
		drive_lms_idx <- drive_lms_idx %>% group_by(sn) %>% filter(lms_sum > 0) %>% mutate(lms_mean = mean(c(hsi, bco, dsi, blm, hsr, csr, blp, csi, lai, hmm, tci, bud, dsr, bdl, lcr, pri, ucr, sni, cab, uvr, bcu, scr, blv, uai), na.rm = T))
		
		names(drive_lms_idx)[1] <- "drive_serial_num"
		
		drive_lms_idx <- unique(drive_lms_idx[,c('drive_serial_num', 'lms_sum', 'lms_mean')])

		drive_particle_idx <- unique(left_join(drive_particle_idx, drive_lms_idx))
		
		saveRDS(drive_particle_idx, file.path(input_path, stringr::str_replace_all(process_list[loop], "_RAW", '_SUMMARY')))
		
		rm(chi_data)
		gc()
	
	}, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
    
  }
  
} 