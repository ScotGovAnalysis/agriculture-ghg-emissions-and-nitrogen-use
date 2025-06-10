library(tidyverse)
library(ReGenesees)
library(readxl) 
library(data.table)
library(kableExtra)

directory<-"C:/Users/u455049/Documents/R/repos/agriculture-ghg-emissions-and-nitrogen-use-2023-24"


# To update data, replace file path and run the below script.

# Load  data from the Excel file
tables <- "C:/Users/U455049/OneDrive - SCOTS Connect/Agricultural Statistics Unit - Economic Statistics/FBS/GHG 2023-24/sectoral analysis/GHG inventory data 2023.xlsx"


source(paste0(directory,"/scripts/functions.R"))

# Load data

load(paste0(directory,"/data/enterprise_tables.rda"))


table_1_html<-readxl::read_excel(paste0(directory,"/data/Table_1_front.xlsx"))


table_2_download<-enterprise_tables[[1]] %>% 
  tibble() %>% 
  arrange(`Farm type`) %>% 
  mutate(Measure = case_when(
    Measure == "Average (median) CO2e/kg dwt" ~ "Average (median)",
    TRUE ~  Measure
  )) %>%
  filter(Measure=="Average (median)")%>% 
  mutate(`Farm type`=ifelse(`Farm type`=="Cattle and Sheep (LFA)", "LFA cattle and sheep",
                            ifelse(`Farm type`=="Specialist Cattle (LFA)", "LFA cattle",
                                   ifelse(`Farm type`=="Lowland Cattle and Sheep", "Lowland cattle and sheep",`Farm type`))))

table_2_html<-table_2_download %>% 
  select(1,4) %>% 
  rename("2023-24 kg CO\u2082e/kg dwt" = `2023-24`) %>% 
  mutate(across(where(is.numeric), ~ round_half_up(.x, 1)))



table_3_download<-enterprise_tables[[2]] %>% 
  tibble() %>% 
  arrange(`Farm type`) %>% 
  mutate(Measure = case_when(
    Measure == "Average (median) CO2e/kg dwt" ~ "Average (median)",
    TRUE ~  Measure
  )) %>%
  filter(Measure=="Average (median)")%>% 
  mutate(`Farm type`=ifelse(`Farm type`=="Cattle and Sheep (LFA)", "LFA cattle and sheep",
                            ifelse(`Farm type`=="Specialist Sheep (LFA)", "LFA sheep",
                                   ifelse(`Farm type`=="Lowland Cattle and Sheep", "Lowland cattle and sheep",`Farm type`))))
  
table_3_html<-table_3_download %>% 
  select(1,4) %>% 
  rename("2023-24 kg CO\u2082e/kg dwt" = `2023-24`)  %>% 
  mutate(across(where(is.numeric), ~ round_half_up(.x, 1)))


table_4_download<-enterprise_tables[[3]] %>% 
  tibble() %>% 
  arrange(`Farm type`) %>% 
  mutate(Measure = case_when(
    Measure == "Average (median) CO2e/kg dwt" ~ "Average (median)",
    TRUE ~  Measure
  )) %>%
  filter(Measure=="Average (median)")

table_4_html<-table_4_download %>% 
  mutate("% change" = round(100*(`2023-24`/`2022-23`-1)), 0) %>% 
  mutate("% change" = paste0(`% change`,"%")) %>% 
  rename("2022-23 kg CO\u2082e/kg FPC milk" = `2022-23`,
         "2023-24 kg CO\u2082e/kg FPC milk" = `2023-24`) %>% 
  select(1,3:5)%>% 
  mutate(across(where(is.numeric), ~ round_half_up(.x, 1)))

table_5_download<-enterprise_tables[[4]] %>% 
  tibble() %>% 
  arrange(`Farm type`) %>% 
  mutate(Measure = case_when(
    Measure == "Average (median) CO2e/kg dwt" ~ "Average (median)",
    TRUE ~  Measure
  )) %>%
  filter(Measure=="Average (median)")%>% 
  mutate(`Farm type`=ifelse(`Farm type`=="General Cropping", "General cropping", `Farm type`))

table_5_html<-table_5_download %>% 
  mutate("% change" = round(100*(`2023-24`/`2022-23`-1)), 0) %>% 
  mutate("% change" = paste0(`% change`,"%")) %>% 
  rename("2022-23 kg CO\u2082e/tonne crop" = `2022-23`,
         "2023-24 kg CO\u2082e/tonne crop" = `2023-24`) %>% 
  select(1,3:5)%>% 
  mutate(across(where(is.numeric), ~ round_half_up(.x, 0)))



# read in table 1
table_6 <- read_excel(tables, sheet = "table 1")

# read in table 2
table_7 <- read_excel(tables, sheet = "table 2 ")


table_6<- table_6 %>% mutate(across(where(is.numeric), ~round(.x,2)*100)) %>% 
  mutate(across(where(is.numeric), ~paste0(.x, "%"))) %>% 
  select(1:8) %>% 
  mutate(`IPCC code – emission source category`= str_replace_all(`IPCC code – emission source category`, "_", " "))

# table 2 ----
#for display - "add merge suffix"

table_7$`Category in Scottish agriculture GHG emissions and nitrogen use` <-  replace(table_7$`Category in Scottish agriculture GHG emissions and nitrogen use`,
                                                                                            duplicated(table_7$`Category in Scottish agriculture GHG emissions and nitrogen use`), "")

table_7<-table_7 %>% 
  mutate(`IPCC code – emission source category`= str_replace_all(`IPCC code – emission source category`, "_", " "))


