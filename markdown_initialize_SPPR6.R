Header<-readxl::read_xlsx(path = "data_checks/01_daily_data_xlsx/BGD1703_SPP_Round6 - all versions - False - 2019-12-01-04-46-00.xlsx", sheet = "BGD1703_SPP_Round6")

Content<-readxl::read_xlsx(path = "data_checks/01_daily_data_xlsx/BGD1703_SPP_Round6 - all versions - False - 2019-12-01-04-46-00.xlsx",sheet = "inividual_info")

write.csv(Header,"data_checks/Header.csv")
write.csv(Content,"data_checks/Content.csv")
phase<-c("pilot", "data_collection")[2]

data_collection_day_number<-21
day_since_dowload<-3
days_since_previous_download<-4

date_previously_downloaded<-Sys.Date()-days_since_previous_download
date_current_download<-Sys.Date()-day_since_dowload

rmarkdown::render('01_DailyReport_SPP_R6.Rmd')


new_report_name<-paste0(date_current_download %>% stringr::str_replace_all("-", ''), "_DailyReport_SPP_R6.html")



file.copy(from = "01_DailyReport_SPP_R6.html", to =paste0(dropbox_base_path, new_report_name),overwrite = TRUE)
