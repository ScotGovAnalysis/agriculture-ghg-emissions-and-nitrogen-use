##RScript which actually runs the main R script and then builds the visual summary


#Choose the range of *crop* years of interest here. 
#For the 2024 publication, this was 2019:2022. In future years, the upper range will increase by 1 each year.
cropyear_range <- c(2019:2022)
#Specify the farmtypes to be included, and their order. 
#1: Cereal, 2: General cropping, 3: Dairy,
#4: LFA Sheep, 5: LFA Cattle, 6: LFA Cattle & Sheep, 7: Lowland livestock, 8: Mixed
#9: All farm types, 10: All LFA livestock types
Output_types = c(9,1,2,3,4,5,6,7,8)

#Use cropyears to determine sampyears
sampyear_range <- cropyear_range + 1
#Figure out the financial years associated with each sampyear. Eg., sampyear "2020" is financial year "2019/20"
financial_years_start <- sampyear_range - 1
financial_years_end <- sampyear_range - 2000
financial_years <- paste0(financial_years_start,"-",financial_years_end)

#Specify the document title
title_line1 <- paste0("Farm Business Survey ", max(financial_years),":")
title_line2 <- "Farm level emissions and nitrogen usage"
title_single_line <- paste0(title_line1,title_line2)
#Specify the filename for the visual summary's html file. The file created with this name can be uploaded to both eRDM and the self-publishing rubric.
html_filename <- paste0("Farm Business Survey - Research - Carbon Audit Analysis ", max(cropyear_range)+2, " - SEFARI - Publication - Visual summary.html")

#specify urls for publication. Some of these won't actually exist when the html file is first created, you'll need to check they all work when published.
publication_url <- paste0("https://www.gov.scot/publications/farm-business-survey-",max(cropyear_range),"-",max(cropyear_range)-1999,"-farm-level-emissions-and-nitrogen-usage/")
documents_url <- paste0(publication_url,"documents/")


##Run the two R scripts and the Rmarkdown document
source('scripts/CN_analysis.R')
source('scripts/Plots.R')
#source('scripts/a11ytable_output.R')


#rmarkdown::render('scripts/CN_analysis_md.Rmd', output_file = html_filename)
# rmarkdown::render('CN_slides.Rmd')
# rmarkdown::render('CN_ppt.Rmd')


# # Create individual csvs and xls for each farmtype
 types <- unique(Table_1$`Farm type`)
# 
# 
# # Absolute
# # 
# export_data1 <- function(t) {
#   # Filter data for the current type
#   df_filtered <- Table_1 %>% filter(`Farm type` == t)
# 
#   # Create file names
#   csv_file <- paste0("download/Absolute_", gsub(" ", "", t), ".csv")
#   xlsx_file <- paste0("download/Absolute_", gsub(" ", "", t), ".xlsx")
# 
#   names(df_filtered)<-NULL
# 
# 
#   df_filtered<-rbind(c("Absolute gross greenhouse gas (GHG) emissions, 2019-20 to 2022-23", rep("",5)), rep("",6), c("Unit", "", "tCO2e/ha", rep("",3)), rep("",6),
#                      c("Farm type", "Measure", "2019-20", "2020-21", "2021-22", "2022-23"), df_filtered)
# 
#   # Export to CSV
#   write.csv(df_filtered, csv_file, row.names = FALSE)
# 
#   # Export to XLSX
#   writexl::write_xlsx(df_filtered, xlsx_file)
# }
# 
# # Apply the function to each type
# lapply(types, export_data1)
# 
# 
# # Intensity
# 
# 
# export_data2 <- function(t) {
#   # Filter data for the current type
#   df_filtered <- Table_2 %>% filter(`Farm type` == t)
# 
#   # Create file names
#   csv_file <- paste0("download/Intensity_", gsub(" ", "", t), ".csv")
#   xlsx_file <- paste0("download/Intensity_", gsub(" ", "", t), ".xlsx")
# 
#   names(df_filtered)<-NULL
# 
# 
#   df_filtered<-rbind(c("Greenhouse gas (GHG) emission intensity, 2019-20 to 2022-23", rep("",5)), rep("",6), c("Unit", "", "kg CO2e/kg output", rep("",3)), rep("",6),
#                      c("Farm type", "Measure", "2019-20", "2020-21", "2021-22", "2022-23"), df_filtered)
# 
#   # Export to CSV
#   write.csv(df_filtered, csv_file, row.names = FALSE)
# 
#   # Export to XLSX
#   writexl::write_xlsx(df_filtered, xlsx_file)
# }
# 
# # Apply the function to each type
# lapply(types, export_data2)
# 
# 
# 
# # Nitrogen balance
# 
# 
# export_data3 <- function(t) {
#   # Filter data for the current type
#   df_filtered <- Table_3 %>% filter(`Farm type` == t)
# 
#   # Create file names
#   csv_file <- paste0("download/Nbalance_", gsub(" ", "", t), ".csv")
#   xlsx_file <- paste0("download/Nbalance_", gsub(" ", "", t), ".xlsx")
# 
#   names(df_filtered)<-NULL
# 
# 
#   df_filtered<-rbind(c("Nitrogen balance, 2019-20 to 2022-23", rep("",5)), rep("",6), c("Unit", "", "kg N surplus/ha", rep("",3)), rep("",6),
#                      c("Farm type", "Measure", "2019-20", "2020-21", "2021-22", "2022-23"), df_filtered)
# 
#   # Export to CSV
#   write.csv(df_filtered, csv_file, row.names = FALSE)
# 
#   # Export to XLSX
#   writexl::write_xlsx(df_filtered, xlsx_file)
# }
# 
# # Apply the function to each type
# lapply(types, export_data3)
# 
# 
# # NUE
# 
# 
# export_data4 <- function(t) {
#   # Filter data for the current type
#   df_filtered <- Table_4 %>% filter(`Farm type` == t)
# 
#   # Create file names
#   csv_file <- paste0("download/NUE_", gsub(" ", "", t), ".csv")
#   xlsx_file <- paste0("download/NUE_", gsub(" ", "", t), ".xlsx")
# 
#   names(df_filtered)<-NULL
# 
# 
#   df_filtered<-rbind(c("Nitrogen use efficiency, 2019-20 to 2022-23", rep("",5)), rep("",6), c("Unit", "", "%", rep("",3)), rep("",6),
#                      c("Farm type", "Measure", "2019-20", "2020-21", "2021-22", "2022-23"), df_filtered)
# 
#   # Export to CSV
#   write.csv(df_filtered, csv_file, row.names = FALSE)
# 
#   # Export to XLSX
#   writexl::write_xlsx(df_filtered, xlsx_file)
# }
# 
# # Apply the function to each type
# lapply(types, export_data4)



