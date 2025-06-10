source("tables/publication tables.R")
source("scripts/html tables.R")
load("data/tablebothmethods.rda")


# Figures 1 to 3 ----------------------------------------------------------

# Figure 1

row1<-c("Total and agriculture greenhouse gas emissions in Scotland, 1990 to 2023", rep("", 2))
row2<-rep("", 3)
row3<-(c("Unit", "MtCO2e", ""))
row4<-rep("", 3)
row5<-(c("Year", "Agriculture", "Total Scotland"))
text1 <- rbind(row1,row2,row3,row4,row5)
text1<-data.frame(text1)

Table_1<-table_1_df %>% 
  pivot_longer(-Category, names_to = "Year", values_to = "Value") %>% 
  pivot_wider(names_from = Category, values_from = Value)%>% 
  mutate(across(where(is.numeric), ~ round_half_up(.x, 3)))
  

names(text1)<-names(Table_1)

Table_1 <- rbind(text1, Table_1)

names(Table_1)<-NULL

# Export to XLSX

csv_file <- paste0("download/Agriculture_emissions.csv")
xlsx_file <- paste0("download/Agriculture_emissions.xlsx")

write.csv(Table_1, csv_file, row.names = FALSE)
writexl::write_xlsx(Table_1, xlsx_file)

# Figure 2

row1<-c("Agriculture greenhouse gas (GHG) emissions in Scotland, by subsector, 2023", "")
row2<-rep("", 2)
row3<-(c("Unit", "MtCO2e"))
row4<-rep("", 2)
row5<-(c("Subsector", "GHG emissions"))
text2 <- rbind(row1,row2,row3,row4,row5)
text2<-data.frame(text2)

Table_2<-table_2_df %>% 
  select(Subsector, `2023`)%>% 
  mutate(across(where(is.numeric), ~ round_half_up(.x, 3)))


names(text2)<-names(Table_2)

Table_2 <- rbind(text2, Table_2)

names(Table_2)<-NULL

# Export to XLSX

csv_file <- paste0("download/Subsector_emissions.csv")
xlsx_file <- paste0("download/Subsector_emissions.xlsx")

write.csv(Table_2, csv_file, row.names = FALSE)
writexl::write_xlsx(Table_2, xlsx_file)


# Figure 3

row1<-c("Agriculture greenhouse gas (GHG) emissions in Scotland, by subsector and emission source, 2023", rep("",6))
row2<-rep("", 7)
row3<-(c("Unit", "MtCO2e", rep("",5)))
row4<-rep("", 7)
text3 <- rbind(row1,row2,row3,row4)
text3<-data.frame(text3)

Table_3<-table_3_df %>% 
  select(-`Total agriculture`) %>% 
  filter(Source!="Total emissions") %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column(var = "Sector") %>% 
  mutate(Sector=gsub("Source", "Sector", Sector))%>% 
  mutate(across(where(is.numeric), ~ round_half_up(.x, 3)))


names(text3)<-names(Table_3)

Table_3 <- rbind(text3, Table_3)

names(Table_3)<-NULL


  

# Export to XLSX

csv_file <- paste0("download/Subsector_source_emissions.csv")
xlsx_file <- paste0("download/Subsector_source_emissions.xlsx")

write.csv(Table_3, csv_file, row.names = FALSE)
writexl::write_xlsx(Table_3, xlsx_file)


# Absolute emissions ------------------------------------------------------

measure_levels <- c("Average (median)",
                    "95% CI (lower limit)",
                    "95% CI (upper limit)")
type_levels<-c(9,1:8)

tablegross<-table1

tablegross<-tablegross %>% 
  rename('95% CI (lower limit)'=Lower,
         '95% CI (upper limit)'=Upper,
         'Average (median)'=Median,
         Method=method)

tablegross<-tablegross %>% 
  select(type, sampyear, Method, 'Average (median)', '95% CI (lower limit)', '95% CI (upper limit)') %>% 
  gather(all_of(measure_levels), key="Measure",value="Value")%>% 
  select(type,sampyear, Method, Measure,Value) %>% 
  spread(key=sampyear, Value) %>% 
  mutate(Measure = factor(Measure, levels = measure_levels)) %>%
  apply_type_formats() %>% 
  mutate(type = factor(type, levels = type_levels)) %>%
  arrange(type, Method, Measure) %>% 
  select(-type) %>% 
  select(8,1:7) %>% 
  rename("Farm type" = farmtype) %>% 
  mutate(Method=ifelse(Method=="improved", "Improved - Agrecalc Cloud", "Previous - Agrecalc Web"))

# # Create individual csvs and xls for each farmtype
types <- unique(tablegross$`Farm type`)
 
   row1<-c("Absolute gross greenhouse gas (GHG) emissions, 2019-20 to 2023-24", rep("", 5))
   row2<-rep("", 6)
   row3<-(c("Unit", "tCO2e/ha", rep("", 4)))
   row4<-rep("", 6)
   row5<-(c("Farm type", "Method", "Measure", "2021-22", "2022-23", "2023-24"))
   row1b<-rep("", 6)
   row5b<-(c("Farm type", "Method", "Measure", "2019-20", "2020-21", "2021-22"))
text4 <- rbind(row1,row2,row3,row4,row5)
text4<-data.frame(text4)

text4b<-rbind(row1b,row2,row3,row4,row5b)
text4b<-data.frame(text4b)

# Absolute

export_data1 <- function(t) {
  # Filter data for the current type
  df_filtered <- tablegross %>% filter(`Farm type` == t & Method=="Improved - Agrecalc Cloud") %>% 
    select(1:3,6:8)
  
  df_filteredb <- tablegross %>% filter(`Farm type` == t & Method=="Previous - Agrecalc Web") %>% 
    select(1:6)

  names(text4)<-names(df_filtered)
  names(df_filteredb)<-names(df_filtered)
  names(text4b)<-names(df_filtered)
  
  
  df_filtered <- rbind(text4, df_filtered, text4b, df_filteredb)
  
  names(df_filtered)<-NULL
  
  
  # Create file names
  csv_file <- paste0("download/Absolute_", gsub(" ", "", t), ".csv")
  xlsx_file <- paste0("download/Absolute_", gsub(" ", "", t), ".xlsx")
  
   
  # Export to CSV
  write.csv(df_filtered, csv_file, row.names = FALSE)

  # Export to XLSX
  writexl::write_xlsx(df_filtered, xlsx_file)
}

# Apply the function to each type
lapply(types, export_data1)

# 

# 
# Nitrogen balance

Table_5<-table_5_df%>%
  filter(Measure!="Lower quartile" & Measure!="Upper quartile")

row1<-c("Nitrogen balance, 2019-20 to 2023-24", rep("", 6))
row2<-rep("", 7)
row3<-(c("Unit", "kg N surplus/ha", rep("", 5)))
row4<-rep("", 7)
row5<-(c("Farm type", "Measure", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24"))
text5 <- rbind(row1,row2,row3,row4,row5)
text5<-data.frame(text5)


export_data2 <- function(t) {
  # Filter data for the current type
  df_filtered <- Table_5 %>% filter(`Farm type` == t)
  
  # Create file names
  csv_file <- paste0("download/Nbalance_", gsub(" ", "", t), ".csv")
  xlsx_file <- paste0("download/Nbalance_", gsub(" ", "", t), ".xlsx")
  
  
  
  names(text5)<-names(df_filtered)
  
  
  df_filtered <- rbind(text5, df_filtered)
  
  names(df_filtered)<-NULL
  
  # Export to CSV
  write.csv(df_filtered, csv_file, row.names = FALSE)
  
  # Export to XLSX
  writexl::write_xlsx(df_filtered, xlsx_file)
}

# Apply the function to each type
lapply(types, export_data2)
# 
# 
# # NUE
# 

Table_6<-table_6_df%>%
  filter(Measure!="Lower quartile" & Measure!="Upper quartile")
row1<-c("Nitrogen use efficiency, 2019-20 to 2023-24", rep("", 6))
row2<-rep("", 7)
row3<-(c("Unit", "%", rep("", 5)))
row4<-rep("", 7)
row5<-(c("Farm type", "Measure", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24"))
text6 <- rbind(row1,row2,row3,row4,row5)
text6<-data.frame(text6)

# 
export_data3 <- function(t) {
  # Filter data for the current type
  df_filtered <- Table_6 %>% filter(`Farm type` == t)
  
  # Create file names
  csv_file <- paste0("download/NUE_", gsub(" ", "", t), ".csv")
  xlsx_file <- paste0("download/NUE_", gsub(" ", "", t), ".xlsx")
  
  
  
  names(text6)<-names(df_filtered)
  
  
  df_filtered <- rbind(text6, df_filtered)
  
  names(df_filtered)<-NULL
  
  # Export to CSV
  write.csv(df_filtered, csv_file, row.names = FALSE)
  
  # Export to XLSX
  writexl::write_xlsx(df_filtered, xlsx_file)
}

# Apply the function to each type
lapply(types, export_data3)


# Enterprise --------------------------------------------------------------

# Beef

row1<-c("Cattle livestock farm beef emission intensities, 2023-24", rep("",2))
row2<-rep("", 3)
row3<-c("Unit", "kgCO2e/kg dwt","")
row4<-rep("", 3)
row5<-(c("Farm type", "Measure", "2023-24"))
text7 <- rbind(row1,row2,row3,row4, row5)
text7<-data.frame(text7)

Table_7<-table_2_download %>% 
  select(1,2,4) 

names(text7)<-names(Table_7)

Table_7 <- rbind(text7, Table_7)

names(Table_7)<-NULL

# Export to XLSX

csv_file <- paste0("download/Beef_enterprise.csv")
xlsx_file <- paste0("download/Beef_enterprise.xlsx")

write.csv(Table_7, csv_file, row.names = FALSE)
writexl::write_xlsx(Table_7, xlsx_file)

# Sheep

row1<-c("Sheep livestock farm beef emission intensities, 2023-24", rep("",2))
row2<-rep("", 3)
row3<-c("Unit", "kgCO2e/kg dwt","")
row4<-rep("", 3)
row5<-(c("Farm type", "Measure", "2023-24"))
text8 <- rbind(row1,row2,row3,row4, row5)
text8<-data.frame(text8)

Table_8<-table_3_download %>% 
  select(1,2,4) 

names(text8)<-names(Table_8)

Table_8 <- rbind(text8, Table_8)

names(Table_8)<-NULL

# Export to XLSX

csv_file <- paste0("download/Sheep_enterprise.csv")
xlsx_file <- paste0("download/Sheep_enterprise.xlsx")

write.csv(Table_8, csv_file, row.names = FALSE)
writexl::write_xlsx(Table_8, xlsx_file)


# Milk

row1<-c("Dairy farm milk emission intensities, 2022-23 and 2023-24", rep("",3))
row2<-rep("", 4)
row3<-c("Unit", "kgCO2e/kg fat and protein corrected (FPC) milk",rep("",2))
row4<-rep("", 4)
row5<-(c("Farm type", "Measure", "2022-23", "2023-24"))
text9 <- rbind(row1,row2,row3,row4, row5)
text9<-data.frame(text9)

Table_9<-table_4_download

names(text9)<-names(Table_9)

Table_9 <- rbind(text9, Table_9)

names(Table_9)<-NULL

# Export to XLSX

csv_file <- paste0("download/Milk_enterprise.csv")
xlsx_file <- paste0("download/Milk_enterprise.xlsx")

write.csv(Table_9, csv_file, row.names = FALSE)
writexl::write_xlsx(Table_9, xlsx_file)



# Cereals

row1<-c("Arable farm cereal emission intensities, 2022-23 and 2023-24", rep("",3))
row2<-rep("", 4)
row3<-c("Unit", "kgCO2e/tonne crop",rep("",2))
row4<-rep("", 4)
row5<-(c("Farm type", "Measure", "2022-23", "2023-24"))
text10 <- rbind(row1,row2,row3,row4, row5)
text10<-data.frame(text10)

Table_10<-table_5_download

names(text10)<-names(Table_10)

Table_10 <- rbind(text10, Table_10)

names(Table_10)<-NULL

# Export to XLSX

csv_file <- paste0("download/Cereals_enterprise.csv")
xlsx_file <- paste0("download/Cereals_enterprise.xlsx")

write.csv(Table_10, csv_file, row.names = FALSE)
writexl::write_xlsx(Table_10, xlsx_file)


csv_file <- paste0("download/Table_6.csv")
xlsx_file <- paste0("download/Table_6.xlsx")

write.csv(table_6, csv_file, row.names = FALSE)
writexl::write_xlsx(table_6, xlsx_file)


csv_file <- paste0("download/Table_7.csv")
xlsx_file <- paste0("download/Table_7.xlsx")

write.csv(table_7, csv_file, row.names = FALSE)
writexl::write_xlsx(table_7, xlsx_file)

