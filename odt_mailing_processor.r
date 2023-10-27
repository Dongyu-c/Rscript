custom_data_prep =  function(input_path, output_path){
  
  process_list = data.frame(excel_file = c("ODT Daily Auto report  subscription.xlsx", "ODT SMT Auto report  subscription.xlsx", "ODT Daily-Weekly WKU Auto report  subscription.xlsx", "Weekly CTS - ODT WKU Auto report subscription.xlsx"),
                            rda_file = c("daily_odt_mail_list.rda", "weekly_odt_mail_list.rda", "odt_wku_mail_list.rda", "cts_odt_wku_mail_list.rda"), stringsAsFactors = F)
  
  for(loop in 1:nrow(process_list)){
    
    tryCatch({
      
      library(xlsx)
      library(dplyr)
      
      excel_name = process_list[loop,]$excel_file
      rda_name = process_list[loop,]$rda_file
      
      tracking_name = stringr::str_replace(excel_name, "Auto report  subscription", "subscription list")
	  tracking_name = stringr::str_replace(excel_name, "Auto report subscription", "subscription list")
      tracking_name = stringr::str_replace(tracking_name, ".xlsx", ".rda")
      
	  if(grepl("Weekly CTS - ODT", excel_name)) {
		mail_sub <- xlsx::read.xlsx(file.path(input_path, excel_name), sheetIndex = 1, stringsAsFactors=FALSE)
	  } else {
		mail_sub <- xlsx::read.xlsx(file.path(input_path, excel_name), sheetIndex = 2, stringsAsFactors=FALSE)
	  }
      
      print(paste("Processing :", excel_name))
      
	  mail_sub <- mail_sub[,!(names(mail_sub) %in% "X__PowerAppsId__")]
	  
      mail_sub <- mail_sub[complete.cases(mail_sub),]
      
      if(nrow(mail_sub) <= 0){
        print("Nothing to process")
        next()
      }
	  
	  print(paste0("Tracking Sheet Name: ", tracking_name))
	  
      if(grepl("WKU", tracking_name) & !grepl("Weekly CTS - ODT WKU", tracking_name)){
        
        mail_sub <- mail_sub[,c('Email','Are.you.subscribing.or.unsubscribing.','Location')]
        
        if(all(mail_sub == "")){
          break()
        }
        
        names(mail_sub) <- c("Email", 'Action', 'Location')
        
        Act_con <- unique(mail_sub$Action)
        
        mail_list <- readRDS(file.path(input_path, "MAIL_LIST", rda_name))
        
        for (con in Act_con) {
          
          temp = mail_sub[mail_sub$Action == con,]
          temp = temp[,-2]
          
          if (con == 'Subscribe') {
            mail_list <- bind_rows(mail_list, temp)
          } else {
            mail_list <- anti_join(mail_list, temp, by = c('Email', 'Location'))
          }
          
          mail_list <- unique(mail_list[with(mail_list, order(Email, Location)),])
          
          saveRDS(mail_list, file.path(output_path, rda_name))
          
        }
        
      } else if (grepl("Weekly CTS - ODT WKU", tracking_name)) {
	  
		mail_sub <- mail_sub[,c('Email', 'Are.you.subscribing.or.unsubscribing.')]
        
        if(all(mail_sub == "")){
          break()
        }

        names(mail_sub) <- c("Email", "Action")
        
        Act_con <- unique(mail_sub$Action)
        
        mail_list <- readRDS(file.path(input_path, "MAIL_LIST", rda_name))
		
        for (con in Act_con) {
          
          temp = mail_sub[mail_sub$Action == con,]
          temp = unique(temp[,-2])
          
          if (con == 'Subscribe') {
            mail_list <- bind_rows(mail_list, data.frame(Email = temp, stringsAsFactors = F))
          } else {
            mail_list <- anti_join(mail_list, data.frame(Email = temp, stringsAsFactors = F), by = c('Email'))
          }
          
          mail_list <- unique(mail_list[with(mail_list, order(Email)),])
          
          saveRDS(mail_list, file.path(output_path, rda_name))
          
        }
		
	  } else {
        
        mail_sub <- mail_sub[,c('Email','Are.you.subscribing.or.unsubscribing.','Please.select.your.product','Location')]
        
        if(all(mail_sub == "")){
          break()
        }
        
        names(mail_sub) <- c("Email", 'Action', 'Product', 'Location')
        
        mail_sub <- mail_sub %>% tidyr::separate_rows(Product)
        
        mail_sub <- mail_sub[mail_sub$Product != "",]
        
        Act_con <- unique(mail_sub$Action)
        
        mail_list <- readRDS(file.path(input_path, "MAIL_LIST", rda_name))
        
        for (con in Act_con) {
          
          temp = mail_sub[mail_sub$Action == con,]
          temp = temp[,-2]
		  
		  print(temp)
          
          if (con == 'Subscribe') {
            mail_list <- bind_rows(mail_list, temp)
          } else {
            mail_list <- anti_join(mail_list, temp, by = c('Email', 'Product', 'Location'))
          }
          
          mail_list <- unique(mail_list[with(mail_list, order(Email, Product, Location)),])
          
          saveRDS(mail_list, file.path(output_path, rda_name))
          
        }
        
      }
      
      saveRDS(mail_list, file.path(input_path, tracking_name))
      
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
    
  }
  
}