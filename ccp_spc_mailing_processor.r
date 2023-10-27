custom_data_prep =  function(input_path, output_path){
  
	process_list = data.frame(excel_file = c("CCP Auto Trigger Subscription.xlsx"),
							  rda_file = c("ccp_auto_trigger_mail_list.rda"), stringsAsFactors = F)

	for(loop in 1:nrow(process_list)){
	  
	  tryCatch({
		
		library(xlsx)
		library(dplyr)
		
		excel_name = process_list[loop,]$excel_file
		rda_name = process_list[loop,]$rda_file
		
		tracking_name = stringr::str_replace(excel_name, "Auto Trigger Subscription", "subscription list")
		tracking_name = stringr::str_replace(tracking_name, ".xlsx", ".rda")
		
		mail_sub <- xlsx::read.xlsx(file.path(input_path, excel_name), sheetIndex = 2, stringsAsFactors=FALSE)
		
		print(paste("Processing :", excel_name))
		
		mail_sub <- mail_sub[complete.cases(mail_sub),]
		
		if(nrow(mail_sub) <= 0){
		  print("Nothing to process")
		  next()
		}
		
		mail_sub <- mail_sub[,c('Email','Are.you.subscribing.or.unsubscribing.','Please.select.your.commodity')]
		
		if(all(mail_sub == "")){
		  break()
		}
		
		names(mail_sub) <- c("Email", 'Action', 'Commodity')
		
		mail_sub <- mail_sub %>% tidyr::separate_rows(Commodity)
		
		mail_sub <- mail_sub[mail_sub$Commodity != "",]
		
		Act_con <- unique(mail_sub$Action)
		
		if(file.exists(file.path(input_path, "MAIL_LIST", rda_name))) {
		  mail_list <- readRDS(file.path(input_path, "MAIL_LIST", rda_name))
		} else {
		  mail_list <- data.frame(Email = NA, Commodity = NA, stringsAsFactors = F)
		  mail_list[-1,]
		}
		
		for (con in Act_con) {
		  
		  temp = mail_sub[mail_sub$Action == con,]
		  temp = temp[,-2]
		  
		  if (con == 'Subscribe') {
			mail_list <- bind_rows(mail_list, temp)
			
			if (any(temp$Commodity %in% c('All', 'Commodities'))) {
			  
			  temp_email = sort(unique(temp[temp$Commodity %in% c('All', 'Commodities'),]$Email))
			  
			  temp_com = sort(unique(temp[!temp$Commodity %in% c('All', 'Commodities'),]$Commodity))
			  
			  temp_all = merge(data.frame(Email = temp_email), data.frame(Commodity = temp_com))
			  
			  mail_list <- bind_rows(mail_list, temp_all)
			  
			}
			
		  } else {
			mail_list <- anti_join(mail_list, temp, by = c('Email', 'Commodity'))
			
			if (any(temp$Commodity %in% c('All', 'Commodities'))) {
			  mail_list <- anti_join(mail_list, temp[temp$Commodity %in% c('All', 'Commodities'),], by = c('Email'))
			}
			
		  }
		  
		  mail_list <- mail_list[!mail_list$Commodity %in% c('All', 'Commodities') & complete.cases(mail_list$Email),]
		  
		  mail_list <- unique(mail_list[with(mail_list, order(Email, Commodity)),])
		  
		  saveRDS(mail_list, file.path(output_path, rda_name))
		  
		}
		
		saveRDS(mail_list, file.path(input_path, tracking_name))
		
	  }, error = function(e) {
		cat("ERROR :", conditionMessage(e), "\n")
	  })
	  
	}
  
}