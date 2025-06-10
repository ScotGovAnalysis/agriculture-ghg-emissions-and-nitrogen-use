# #R script for producing GHG analysis for experimental stats publication (Sefari project)
library(haven)
library(tidyverse)
library(spatstat)
library(writexl)
library(data.table)
library(gridExtra)
library(grid)
library(plotly)
source("~/R/agriculture-ghg-emissions-and-nitrogen-use/scripts/2023_24/functions.R")


# load(file="AllYears_carbon.rda")
load(file="~/R/agriculture-ghg-emissions-and-nitrogen-use/AllYears_nue.rda")

#Use cropyears to determine sampyears
cropyear_range <- c(2019:2023)
sampyear_range <- cropyear_range + 1
#Figure out the financial years associated with each sampyear. Eg., sampyear "2020" is financial year "2019/20"
financial_years_start <- sampyear_range - 1
financial_years_end <- sampyear_range - 2000
financial_years <- paste0(financial_years_start,"-",financial_years_end)

# can be run from rdas ----------------------------------------------------


##Repeat summarisation steps for Nitrogen


Nitrogen_summary <- AllYears_nue %>% 
  group_by(sampyear, type) %>%
  N_summary()
Nitrogen_summary_all <- AllYears_nue %>% 
  group_by(sampyear) %>%
  N_summary() %>% 
  mutate(type=9)
Nitrogen_summary_LFA <- AllYears_nue %>%
  filter(type %in% 4:6) %>% 
  group_by(sampyear) %>%
  N_summary() %>% 
  mutate(type=10)
Nitrogen_summary <- Nitrogen_summary %>% 
  bind_rows(Nitrogen_summary_all, Nitrogen_summary_LFA)
Nitrogen_summary <- Nitrogen_summary[order(Nitrogen_summary$sampyear), ]

#Apply wordy formats

Nitrogen_summary <- apply_type_formats(Nitrogen_summary) %>% 
  select(sampyear, farmtype, everything())



#Column names for the output tables - Type, measure and then one for each financial year.
Output_colnames = c("Farm type", "Measure", c(financial_years))
Output_types = c(9,1,2,3,4,5,6,7,8)
#This is a function to create the output tables. 
#The input table is either Carbon_summary or Nitrogen_summary.
#The "variable" argument needs to be one of the measures in the Carbon/Nitrogen summary. 
#Output_table is the name of the resulting table, created by the function
Create_output_table <- function(Input_table, variable, Output_table){
  Table_name <- Input_table %>% 
    #filter(type %in% Output_types) %>% 
    select("Average (median)"= paste0(variable,"_med"),
           "Lower quartile" = paste0(variable,"_Q1"), 
           "Upper quartile" = paste0(variable,"_Q3"),
           everything()) %>% 
    gather("Average (median)", "Lower quartile", "Upper quartile", key="Measure",value="Value") %>% 
    select("Farm type"=farmtype,type,sampyear,Measure,Value) %>% 
    spread(key=sampyear, Value)
  Table_name$type <- factor(Table_name$type, levels = Output_types)
  Table_name <- Table_name[order(Table_name$type),] %>% 
    select(-type)
  colnames(Table_name) <- Output_colnames
  return(Table_name)
}


#Use the above function to create four output tables, one for each of  the four variables.

Table_3 <- Create_output_table(Nitrogen_summary, "N_surplus", "Table_3")
Table_4 <- Create_output_table(Nitrogen_summary, "nue", "Table_4")

#plots
source("~/R/agriculture-ghg-emissions-and-nitrogen-use/scripts/2023_24/nitrogen_plots.R")

#save xlsx
N_data = list(N_bal =Table_3,
           NUE = Table_4,
           Detailed = Nitrogen_summary)

write_xlsx(N_data, "C:/Users/U456727/OneDrive - SCOTS Connect/Economic Statistics/FBS/GHG 2023-24/Nitrogen 2023-24/N_data_2023-24.xlsx")
