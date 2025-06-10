# #R script for producing GHG analysis for experimental stats publication (Sefari project)
library(haven)
library(tidyverse)
library(spatstat)
library(writexl)
library(data.table)
library(gridExtra)
library(grid)
library(plotly)
# ##Specify the locations of the data to be read in (SAS drive) and where outputs should go (currently the Z drive.)
# #Use an environment variable to specify the FBS and Z drive data paths.
# #See https://csgillespie.github.io/efficientR/set-up.html#renviron
# #The path here is to the FAS folder on the SAS drive.
# FBS_directory_path <- Sys.getenv("FBS_directory_path")
# Z_drive_path <- Sys.getenv("Z_drive_path")
# Output_directory <- paste0(Z_drive_path,'prod',max(cropyear_range)+1,'/Sefari_outputs/')
# 
#Variables for farmtype names and numbering
fbs_type_numbers <- c(1:10)
## The "[1]" after "All farm types" is a deliberate footnote.
# fbs_type_words <- c("Cereal", "General Cropping", "Dairy", "Specialist Sheep (LFA)", "Specialist Cattle (LFA)", "Cattle and Sheep (LFA)", "Lowland Cattle and Sheep", "Mixed", "All Types [1]", "Less favoured area (LFA) Livestock")
fbs_type_words <- c("Cereal", "General Cropping", "Dairy", "Specialist Sheep (LFA)", "Specialist Cattle (LFA)", "Cattle and Sheep (LFA)", "Lowland Cattle and Sheep", "Mixed", "All Types", "Less favoured area (LFA) Livestock")
fbs_type_tab <- data.frame(fbs_type_numbers, fbs_type_words)
apply_type_formats <- function(table_name) {
  setkey(setDT(table_name),type)
  table_name[setDT(fbs_type_tab),farmtype:=i.fbs_type_words]
  return(table_name)
}



load(file="AllYears_carbon.rda")
load(file="AllYears_nue.rda")

# can be run from rdas ----------------------------------------------------


#Create carbon output table
#Function to perform the summarising needed
C_summarise <- function(df){
  df <- summarise(df, CO2e_per_ha_mean = weighted.mean(total_ha_co2_calc,fbswt),
                  CO2e_per_ha_Q1 = weighted.quantile(total_ha_co2_calc, fbswt, 0.25),
                  CO2e_per_ha_Q3 = weighted.quantile(total_ha_co2_calc, fbswt, 0.75),
                  CO2e_per_ha_min = min(total_ha_co2_calc),
                  CO2e_per_ha_med = weighted.median(total_ha_co2_calc, fbswt),
                  CO2e_per_ha_max = max(total_ha_co2_calc),
                  
                  CO2e_per_kg_mean = weighted.mean(total_wf_co2, fbswt),
                  CO2e_per_kg_Q1 = weighted.quantile(total_wf_co2, fbswt, 0.25),
                  CO2e_per_kg_Q3 = weighted.quantile(total_wf_co2, fbswt, 0.75),
                  CO2e_per_kg_min = min(total_wf_co2),
                  CO2e_per_kg_med = weighted.median(total_wf_co2, fbswt),
                  CO2e_per_kg_max = max(total_wf_co2),
                  
                  FBI_mean = weighted.mean(fa_fbi, fbswt),
                  farm_output_kg_mean = weighted.mean(farm_output_kg, fbswt),
                  farm_output_kg_med = weighted.median(farm_output_kg, fbswt),
                  farm_output_kg_Q1 = weighted.quantile(farm_output_kg, fbswt, 0.25),
                  farm_output_kg_Q3 = weighted.quantile(farm_output_kg, fbswt, 0.75),
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
  return(df)
}



##Apply the summarising function to individual types
Carbon_summary <- AllYears_carbon %>% 
  group_by(sampyear, type) %>% 
  C_summarise()
##Apply separately to "All farm types" (type=9)
Carbon_summary_all <- AllYears_carbon %>% 
  group_by(sampyear) %>%
  C_summarise() %>% 
  mutate(type=9)
##And once more for "All LFA farms" (type=10)
Carbon_summary_LFA <- AllYears_carbon %>%
  filter(type %in% 4:6) %>% 
  group_by(sampyear) %>%
  C_summarise() %>% 
  mutate(type=10)
#Append the "All farm types" and "All LFA farms" mini-tables to the main table.
#Also convert kg to tonnes for per hectare calculations
Carbon_summary <- Carbon_summary %>% 
  bind_rows(Carbon_summary_all, Carbon_summary_LFA) %>% 
  mutate_at(vars(starts_with("CO2e_per_ha")), function(x) x*0.001)
##Order by sampyear
Carbon_summary <- Carbon_summary[order(Carbon_summary$sampyear),]

##Repeat summarisation steps for Nitrogen
N_summary <- function(df){
  df <- summarise(df, N_surplus_mean = weighted.mean(farm_n_surplus, fbswt),
                  N_surplus_Q1 = weighted.quantile(farm_n_surplus, fbswt, 0.25),
                  N_surplus_Q3 = weighted.quantile(farm_n_surplus, fbswt, 0.75),
                  N_surplus_min = min(farm_n_surplus),
                  N_surplus_med = weighted.median(farm_n_surplus, fbswt),
                  N_surplus_max = max(farm_n_surplus, fbswt),
                  
                  N_input_mean = weighted.mean(ninput_total, fbswt),
                  N_input_Q1 = weighted.quantile(ninput_total, fbswt, 0.25),
                  N_input_Q3 = weighted.quantile(ninput_total, fbswt, 0.75),
                  N_input_min = min(ninput_total),
                  N_input_med = weighted.median(ninput_total, fbswt),
                  N_input_max = max(ninput_total, fbswt),
                  
                  N_output_mean = weighted.mean(noutput_total, fbswt),
                  N_output_Q1 = weighted.quantile(noutput_total, fbswt, 0.25),
                  N_output_Q3 = weighted.quantile(noutput_total, fbswt, 0.75),
                  N_output_min = min(noutput_total),
                  N_output_med = weighted.median(noutput_total, fbswt),
                  N_output_max = max(noutput_total, fbswt),
                  
                  nue_mean = weighted.mean(nue, fbswt),
                  nue_Q1 = weighted.quantile(nue, fbswt, 0.25),
                  nue_Q3 = weighted.quantile(nue, fbswt, 0.75),
                  nue_min = min(nue),
                  nue_med = weighted.median(nue, fbswt),
                  nue_max = max(nue),
                  farm_output_kg_mean = weighted.mean(farm_output_kg, fbswt),
                  farm_output_kg_med = weighted.median(farm_output_kg, fbswt),
                  farm_output_kg_Q1 = weighted.quantile(farm_output_kg, fbswt, 0.25),
                  farm_output_kg_Q3 = weighted.quantile(farm_output_kg, fbswt, 0.75),
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
}



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
Carbon_summary <- apply_type_formats(Carbon_summary) %>% 
  select(sampyear, farmtype, everything())
Nitrogen_summary <- apply_type_formats(Nitrogen_summary) %>% 
  select(sampyear, farmtype, everything())


#Create a combined summary table and csv for the open data platform

Combined_summary <- Carbon_summary %>% 
  left_join(Nitrogen_summary, by=c("sampyear", "farmtype", "type")) %>% 
  filter(type %in% Output_types) %>% 
  mutate(DateCode=paste0(sampyear-1,"/",sampyear)) %>% 
  select(Farmtype=farmtype,
         DateCode,
         "Average farm absolute GHG emissions per hectare - median" = CO2e_per_ha_med,
         "Average farm absolute GHG emissions per hectare - lower quartile" = CO2e_per_ha_Q1,
         "Average farm absolute GHG emissions per hectare - upper quartile" = CO2e_per_ha_Q3,
         "Average farm emission intensity from GHG - median" = CO2e_per_kg_med,
         "Average farm emission intensity from GHG - lower quartile" = CO2e_per_kg_Q1,
         "Average farm emission intensity from GHG - upper quartile" = CO2e_per_kg_Q3,
         "Average farm nitrogen balance - median" = N_surplus_med,
         "Average farm nitrogen balance - lower quartile" = N_surplus_Q1,
         "Average farm nitrogen balance - upper quartile" = N_surplus_Q3,
         "Average farm nitrogen use efficiency - median" = nue_med,
         "Average farm nitrogen use efficiency - lower quartile" = nue_Q1,
         "Average farm nitrogen use efficiency - upper quartile" = nue_Q3
         )

## Create narrow dataset, for publication on opendata platform
Combined_summary_narrow <- Combined_summary %>% 
  gather(`Average farm absolute GHG emissions per hectare - median`:`Average farm nitrogen use efficiency - upper quartile`, key = "Measure", value = "Value") %>% 
  mutate(FeatureCode = "S92000003",
         Measurement = "Count",
         Units = "Tonnes CO2 equivalent per hectare") %>% 
  select(FeatureCode, DateCode, "Farm type" = Farmtype, Measure, Measurement, Units, Value)
### Add units, depending on value of Measure column.
### Pretty messy way of doing this. A lookup table may have been better...
Combined_summary_narrow <- Combined_summary_narrow %>% 
  mutate(Units = ifelse(substr(Measure, 1, 21) =="Average farm absolute", "Tonnes CO2 equivalent per hectare",
                        ifelse(substr(Measure, 1, 21) =="Average farm emission", "Tonnes CO2 equivalent per kilogram farm output",
                               ifelse(substr(Measure, 1, 29) =="Average farm nitrogen balance", "Kilogrammes per hectare",
                                      ifelse(substr(Measure, 1, 25) =="Average farm nitrogen use", "Percent",
                                             NA)))))
### Change Measurement value to "Ratio" if Measure is NUE.
Combined_summary_narrow$Measurement[substr(Combined_summary_narrow$Measure, 1, 12)=="Nitrogen use"] = "Ratio"



#Column names for the output tables - Type, measure and then one for each financial year.
Output_colnames = c("Farm type", "Measure", c(financial_years))

#This is a function to create the output tables. 
#The input table is either Carbon_summary or Nitrogen_summary.
#The "variable" argument needs to be one of the measures in the Carbon/Nitrogen summary. 
#Output_table is the name of the resulting table, created by the function
Create_output_table <- function(Input_table, variable, Output_table){
  Table_name <- Input_table %>% 
    filter(type %in% Output_types) %>% 
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
Table_1 <- Create_output_table(Carbon_summary, "CO2e_per_ha", "Table_1")
Table_2 <- Create_output_table(Carbon_summary, "CO2e_per_kg", "Table_2")
Table_3 <- Create_output_table(Nitrogen_summary, "N_surplus", "Table_3")
Table_4 <- Create_output_table(Nitrogen_summary, "nue", "Table_4")


