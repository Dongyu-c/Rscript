custom_data_prep =  function(input_path, output_path){
	
	library(dplyr)
	
	working_data <- read.csv(input_path, stringsAsFactors = F)

	account_month_week <- working_data[,c("FISCAL_YEAR_WEEK", "ACCOUNTING_PERIOD_NAME", "CALENDAR_YEAR", "DATE_KEY", "ACTUAL_DATE")]

	account_month_week <- unique(account_month_week[account_month_week$ACCOUNTING_PERIOD_NAME != "(null)",])
	
	month_naming <- unique(working_data[,c("CALENDAR_YEAR", "CALENDAR_MONTH", "MONTH_YEAR_NAME")])

	month_naming$ACCOUNTING_PERIOD_NAME <- paste0(stringr::str_sub(month_naming$MONTH_YEAR_NAME, 1, 3), "-", stringr::str_sub(month_naming$CALENDAR_YEAR, 3, 4))

	month_naming <- month_naming[,c("CALENDAR_MONTH", "ACCOUNTING_PERIOD_NAME")]

	final_fiscal_week_month <- left_join(account_month_week, month_naming, by = "ACCOUNTING_PERIOD_NAME")

	final_fiscal_week_month <- final_fiscal_week_month[order(final_fiscal_week_month$ACTUAL_DATE),c("ACTUAL_DATE","CALENDAR_YEAR","ACCOUNTING_PERIOD_NAME","CALENDAR_MONTH","FISCAL_YEAR_WEEK","DATE_KEY")]

	fiscal_fiscal_quarter <- working_data[,c("FISCAL_QUARTER", "FISCAL_YEAR", "FISCAL_YEAR_WEEK")]

	final_fiscal_week_month <- left_join(final_fiscal_week_month, fiscal_fiscal_quarter, by = "FISCAL_YEAR_WEEK")

	write.csv(final_fiscal_week_month, file.path(output_path, "FINANCIAL_MONTH_FW_DIC.csv"), row.names = F)

} 