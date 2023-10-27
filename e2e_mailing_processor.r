custom_data_prep =  function(input_path, output_path){
  
	sub_request <- readRDS(file.path(input_path, 'e2e_oqa_sub_list.rda'))

	mailing_list <- readRDS(file.path(input_path, 'e2e_oqa_final_mailing_list.rda'))
	
	sub_list <- unique(sub_request[sub_request$`Are.you.subscribing.or.unsubscribing?` %in% "Subscribe",]$Email)

	unsub_list <- unique(sub_request[sub_request$`Are.you.subscribing.or.unsubscribing?` %in% "Unsubscribe",]$Email)

	if (length(sub_list) > 0) {
	  
	  mailing_list <- rbind(mailing_list, sub_list) 
	  
	  mailing_list <- data.frame('Subscriber' = unique(mailing_list[order(mailing_list$Subscriber),]))
	  
	  sub_list <- data.frame('Subscriber' = c(sub_list, NA))
	  
	} else {
	
	  sub_list <- data.frame('Subscriber' = NA)
	  
	}

	if (length(unsub_list) > 0) {

	  mailing_list <- data.frame('Subscriber' = unique(mailing_list[!mailing_list$Subscriber %in% unsub_list,]))

	  unsub_list <- data.frame('Subscriber' = c(unsub_list, NA))

	} else {
	
	  unsub_list <- data.frame('Subscriber' = NA)
	  
	}
	
	saveRDS(sub_list, file.path(input_path, 'e2e_sub_list.rda'))
	saveRDS(unsub_list, file.path(input_path, 'e2e_unsub_list.rda'))

	saveRDS(mailing_list, file.path(input_path, 'e2e_oqa_final_mailing_list.rda'))
	
}