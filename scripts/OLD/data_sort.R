#File sorts and loads data 
#Data include dataframes used in plots and data tables
#load libraries ----
library(readxl) 
library(tidyverse)
library(data.table)
library(kableExtra)

# set parameters ----
CurrentYear = 2022
sectors <- c("Arable", "Dairy", "Dairy beef", "Other", "Sheep", "Suckler beef"
#, "Total"
)

national_data <- "C:/Users/U456727/OneDrive - SCOTS Connect/Economic Statistics/Data lab 2024/GHG inventory data 2022.xlsx"
tables <- "C:/Users/U456727/OneDrive - SCOTS Connect/Economic Statistics/GHG emissions and N use report/Data/GHG inventory tables in report.xlsx"

#load data----
#read in national totals
nat_tot <- read_excel(national_data, sheet = "national_total")


#read in sector totals
sec_tot <- read_excel(national_data, sheet = "subsector_total")
#names(sec_tot) <- gsub("X", "", names(sec_tot))

# read in subsector gas compostion for latest year bar chart
#sec_comp_latest <- read_excel(national_data, sheet = "subsector_comp_latest")

# # read in subsector emisison sources for latest year bar chart
sec_source <- read_excel(national_data, sheet = "subsector_source")

# read in table 1
table_1 <- read_excel(tables, sheet = "table 1")

# read in table 2
table_2 <- read_excel(tables, sheet = "table 2 ")

#sort data----
# industry gross totals - fig 1
# pivot longer
nat_tot <- nat_tot %>% pivot_longer(cols = where(is.numeric), names_to = "Year", values_to = "ghg_emiss")
nat_tot <- nat_tot %>% filter(Industry != "TOTAL") 
nat_tot$Year <- as.numeric(nat_tot$Year)

# Industry in cols - pivot wider 
nat_tot <- nat_tot %>% pivot_wider(names_from = Industry, values_from = c(ghg_emiss))
industry_names <- colnames(nat_tot[-1]  )

nat_tot_98 <- nat_tot %>% filter(Year >1995)
nat_tot_95 <- nat_tot %>% filter(Year == 1995)
latest_year_gross_data <- nat_tot_98 %>% filter(Year %in% CurrentYear)
nineties_gross_data <- nat_tot %>% filter(Year == 1990)



# fig 2----
# sect totals - fig 2

sec_tot <- sec_tot %>%  select(Subsector, Year =last_col()) %>% 
  # remove total
  filter(Subsector !="Total") %>% 
  #order for plotly
 dplyr::arrange(Year)
#for plotly- it ignores order of dataset - need to get order and then set
#Industry as factor
sec_order <- sec_tot$Subsector
sec_tot$Subsector<- factor(sec_tot$Subsector, levels = sec_order)


# fig 3 ----
# sector source - transpose to get y values as source  - fig 4
# reorder cols
sec_source <- sec_source %>% select(Source, all_of(sectors)) 

# first remember the names
source_names <- sec_source$Source

# transpose all but the first column (name)
sec_source<- as.data.frame(t(sec_source[,-1]))
colnames(sec_source) <- source_names

# create sector column for x and reorder
sec_source$Sector <- rownames(sec_source)
sec_source <- sec_source %>% select(Sector, everything(),
                                    -`Urea application`, 
                                    -`Non-energy products from fuels and solvent use`)

# order  data for bar chart - fig 4
# create total first
sec_source$total <- rowSums(sec_source[-1])
sec_source <- sec_source %>% dplyr::arrange(total)
#for plotly- it ignores order of dataset - need to get order and then set Industry as factor
sec_order <- sec_source$Sector
sec_source$Sector<- factor(sec_source$Sector, levels = sec_order)


# table 1 ----
# change values to percentages
table_1_pct <- table_1 %>% mutate(across(where(is.numeric), ~round(.x,2)*100)) %>% 
  mutate(across(where(is.numeric), ~paste0(.x, "%")))

# table 2 ----
#for display - "add merge suffix"
table_2_merge <- table_2
table_2_merge$`Category in Scottish agriculture GHG emissions and nitrogen use` <-  replace(table_2$`Category in Scottish agriculture GHG emissions and nitrogen use`,
                  duplicated(table_2$`Category in Scottish agriculture GHG emissions and nitrogen use`), "")


# # export plot data csvs and xlsx ----
# 
# #Fig 1
#   # select just agriculture and total
#   df <- nat_tot %>% select(c(Year, industry_names[1], 
#                              industry_names[8]))
# 
#   # Create file names
#   csv_file <- paste0("download/Scot_ghg_emiss",".csv")
#   xlsx_file <- paste0("download/Scot_ghg_emiss", ".xlsx")
# 
#   names(df)<-NULL
# 
# 
#   df<-rbind(c("Total and agriculture greenhouse gas emissions in Scotland, 1990 to 2022",
#                rep("",2)), 
#              rep("",3),
#              c("Unit", "", "MtCO2e"),
#              rep("",3),
#             c("Year", industry_names[1], "Total Scotland"), df)
# 
#   # Export to CSV
#   write.csv(df, csv_file, row.names = FALSE)
# 
#   # Export to XLSX
#   writexl::write_xlsx(df, xlsx_file)
# 
# # Fig 2----
#   
#   df <- sec_tot
#   
#   
#   # Create file names
#   csv_file <- paste0("download/agr_ss_ghg_emiss",".csv")
#   xlsx_file <- paste0("download/agr_ss_ghg_emiss", ".xlsx")
#   
#   #change back to character 
#   df$Subsector <- as.character(df$Subsector)
#    
#   names(df)<-NULL
#   
#  
#   
#   df<-rbind(c("Agriculture greenhouse gas (GHG) emissions in Scotland, by subsector, 2022",
#               ""), 
#             rep("",2),
#             c("Unit", "MtCO2e"),
#             rep("",2),
#              c("Subsector", "GHG emissions" ), df)
#   
#   # Export to CSV
#   write.csv(df, csv_file, row.names = FALSE)
#   
#   # Export to XLSX
#   writexl::write_xlsx(df, xlsx_file)
# 
#   # Fig 3----
#   
#   df <- sec_source %>% select(-total)
#   
#   # round to 2dp
#   df <- df %>% mutate(across(where(is.numeric), ~round(.x, 2)))
#   
#   # Create file names
#   csv_file <- paste0("download/agr_source_ghg_emiss",".csv")
#   xlsx_file <- paste0("download/agr_source_ghg_emiss", ".xlsx")
#   
#   #change back to character 
#   df$Sector <- as.character(df$Sector)
#   
#   headings <- names(df)
#   
#   names(df) <- NULL
#   
#   df<-rbind(c("Agriculture greenhouse gas (GHG) emissions in Scotland, by subsector and emission source, 2022",
#               rep("", 6)), 
#             rep("",7),
#             c("Unit", "", "MtCO2e", rep("", 4)),
#             rep("",7),
#             c(headings ), df)
#   
#   # Export to CSV
#   write.csv(df, csv_file, row.names = FALSE)
#   
#   # Export to XLSX
#   writexl::write_xlsx(df, xlsx_file)


# Save table_1 as xlsx and csv

writexl::write_xlsx(table_1_pct, "Table_1.xlsx")
write.csv(table_1_pct, "Table_1.csv")

# Save table_2 as xlsx and csv

writexl::write_xlsx(table_2, "Table_2.xlsx")
write.csv(table_2, "Table_2.csv")


