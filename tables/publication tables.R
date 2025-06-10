library(tidyverse)
library(ReGenesees)

directory<-"C:/Users/u455049/Documents/R/repos/agriculture-ghg-emissions-and-nitrogen-use-2023-24"

source(paste0(directory,"/scripts/functions.R"))

# Load data
load(paste0(directory,"/data/ghg_data.RData"))
load(paste0(directory,"/data/cloud_carbon.rda"))
load(paste0(directory,"/data/old_carbon.rda"))
load(paste0(directory,"/data/AllYears_nue.rda"))
load(paste0(directory,"/data/enterprise_tables.rda"))


# 

# Table 1 - agriculture vs total ------------------------------------------

# Filter the data for 'Total' and 'Agriculture'
filtered_data <- national_total %>%
  filter(Industry %in% c("Total", "Agriculture"))

# Rename 'Total' to 'Total emissions'
filtered_data$Industry <- ifelse(filtered_data$Industry == "Total", "Total emissions", filtered_data$Industry)

# Extend the years sequence to include 2034
years <- seq(min(filtered_data$Year), 2030)
industries <- unique(filtered_data$Industry)
complete_data <- expand.grid(Year = years, Industry = industries)

# Merge with filtered data to ensure all years are represented, filling missing values with NA
complete_data <- complete_data %>%
  left_join(filtered_data, by = c("Year", "Industry"))


# remove rows with NAs

complete_data <- complete_data %>% 
  na.omit() %>% 
  pivot_wider(names_from=Year, values_from=Value) %>% 
  rename(Category=Industry) %>% 
  mutate(Category = if_else(Category == "Total emissions", "Total", Category))

# pivot wider


table_1_df<-tibble(complete_data)




# Table 2 - agriculture by subsector --------------------------------------
subsector_levels <- c("Arable",
                    "Dairy",
                    "Dairy beef",
                    "Sheep",
                    "Suckler beef",
                    "Other",
                    "Total")


subsector<-subsector_total %>% 
  pivot_wider(names_from=Year, values_from=Value) %>% 
  mutate(Subsector = factor(Subsector, levels = subsector_levels)) %>%
  arrange(Subsector)

table_2_df<-tibble(subsector)



# Table 3 - subsector by source -------------------------------------------

source <- subsector_source %>% 
  mutate('Total agriculture' = rowSums(across(2:7))) %>% 
  bind_rows(
    summarise(
      .,
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
      across(where(~ !is.numeric(.x)), ~ "Total emissions")
    )
  )

table_3_df<-tibble(source) %>% 
  mutate(across(where(is.numeric), ~ round_half_up(.x, 3)))


# Farm level carbon emissions ---------------------------------------------


# get carbon data together

cloud_carbon <- map(all_carbon,  ~ .x %>%
                      mutate(total_ha_co2 = `Gross emissions from farming`/FA_AAUA) %>% 
                      filter(Enterprise == "Whole Farm")) %>% 
  bind_rows()


cloud_carbon_red<-cloud_carbon %>%
  rename(fa_id=FA_ID,
         sampyear=YS_YEAR) %>% 
  select(fa_id, fbswt, type, total_ha_co2, sampyear) %>% 
  mutate(total_ha_co2=total_ha_co2*0.001)%>% 
  mutate(method="improved")



old_carbon_red<-old_carbon %>%
  select(fa_id, fbswt, type, total_ha_co2, sampyear) %>% 
  mutate(total_ha_co2=total_ha_co2*0.001) %>% 
  mutate(method="previous")

all_carbon<-bind_rows(old_carbon_red, cloud_carbon_red)

all_carbon$sampyear <- paste0(all_carbon$sampyear - 1, "-", substr(all_carbon$sampyear, 3, 4))


measure_levels <- c("Average (median)",
                    "95% CI (lower limit)",
                    "95% CI (upper limit)",
                    "Lower quartile",
                    "Upper quartile")
type_levels<-c(9,1:8)


des<-e.svydesign(data=all_carbon, ids=~fa_id, 		
                 strata=NULL, weights=~fbswt)		

# gross emissions  errors

# table1 <- svystatTM(
#   design = des,
#   y = ~(total_ha_co2),
#   by = ~type:sampyear,
#   estimator="Mean", conf.int = T, conf.lev = 0.95, vartype=c("se", "cvpct")
# )


table1 <- svystatQ(
  design = des,
  y = ~(total_ha_co2),
  by = ~type:sampyear:method, conf.lev = 0.95, vartype=c("se", "cvpct")
)

table1b <- svystatQ(
  design = des,
  y = ~(total_ha_co2),
  by = ~sampyear:method, conf.lev = 0.95, vartype=c("se", "cvpct")
)

table1b<-table1b %>% 
  mutate(type=as.numeric(9))



table1<-bind_rows(table1, table1b) %>% 
  rename('95% CI (lower limit)'=`CI.l(95%).total_ha_co2.Q[0.500]`,
         '95% CI (upper limit)'=`CI.u(95%).total_ha_co2.Q[0.500]`,
         'Lower quartile' = `total_ha_co2.Q[0.250]`,
         'Upper quartile' = `total_ha_co2.Q[0.750]`,
         'Average (median)'=`total_ha_co2.Q[0.500]`) %>% 
  mutate(sampyear=factor(sampyear)) %>% 
  select(type, sampyear, method, 'Average (median)', '95% CI (lower limit)', '95% CI (upper limit)', 'Lower quartile', 'Upper quartile' ) %>% 
  mutate(yearmethod=paste(sampyear, method))




table_4_df<-table1 %>% 
  tibble() %>% 
  filter(yearmethod!="2021-22 previous") %>% 
  select(-c(method, yearmethod)) %>%
  gather(all_of(measure_levels), key="Measure",value="Value") %>% 
  select(type,sampyear,Measure,Value) %>% 
  spread(key=sampyear, Value) %>% 
  mutate(Measure = factor(Measure, levels = measure_levels)) %>%
  apply_type_formats() %>% 
  mutate(type = factor(type, levels = type_levels)) %>%
  arrange(type, Measure) %>% 
  select(-type) %>% 
  select(7,1:6) %>% 
  rename("Farm type" = farmtype)






AllYears_nue$year_label <- paste0(AllYears_nue$sampyear - 1, "-", substr(AllYears_nue$sampyear, 3, 4))


des2<-e.svydesign(data=AllYears_nue, ids=~fa_id, 		
                  strata=NULL, weights=~fbswt)		


table2 <- svystatQ(
  design = des2,
  y = ~(farm_n_surplus),
  by = ~type:year_label, conf.lev = 0.95, vartype=c("se", "cvpct")
)

table2b <- svystatQ(
  design = des2,
  y = ~(farm_n_surplus),
  by = ~year_label, conf.lev = 0.95, vartype=c("se", "cvpct")
)




table2b<-table2b %>% 
  mutate(type=as.numeric(9))



table2<-bind_rows(table2, table2b) %>% 
  rename('95% CI (lower limit)'=`CI.l(95%).farm_n_surplus.Q[0.500]`,
         '95% CI (upper limit)'=`CI.u(95%).farm_n_surplus.Q[0.500]`,
         'Lower quartile' = `farm_n_surplus.Q[0.250]`,
         'Upper quartile' = `farm_n_surplus.Q[0.750]`,
         'Average (median)'=`farm_n_surplus.Q[0.500]`) %>% 
  rename(sampyear=year_label) %>% 
  mutate(sampyear=factor(sampyear)) %>% 
  select(type, sampyear,'Average (median)', '95% CI (lower limit)', '95% CI (upper limit)', 'Lower quartile', 'Upper quartile' )



table_5_df<-table2 %>% 
  tibble() %>% 
  gather(all_of(measure_levels), key="Measure",value="Value") %>% 
  select(type,sampyear,Measure,Value) %>% 
  spread(key=sampyear, Value) %>% 
  mutate(Measure = factor(Measure, levels = measure_levels)) %>%
  apply_type_formats() %>% 
  mutate(type = factor(type, levels = type_levels)) %>%
  arrange(type, Measure) %>% 
  select(-type) %>% 
  select(7,1:6) %>% 
  rename("Farm type" = farmtype)




table3 <- svystatQ(
  design = des2,
  y = ~(nue),
  by = ~type:year_label, conf.lev = 0.95, vartype=c("se", "cvpct")
)

table3b <- svystatQ(
  design = des2,
  y = ~(nue),
  by = ~year_label, conf.lev = 0.95, vartype=c("se", "cvpct")
)




table3b<-table3b %>% 
  mutate(type=as.numeric(9))



table3<-bind_rows(table3, table3b) %>% 
  rename('95% CI (lower limit)'=`CI.l(95%).nue.Q[0.500]`,
         '95% CI (upper limit)'=`CI.u(95%).nue.Q[0.500]`,
         'Lower quartile' = `nue.Q[0.250]`,
         'Upper quartile' = `nue.Q[0.750]`,
         'Average (median)'=`nue.Q[0.500]`) %>% 
  rename(sampyear=year_label) %>% 
  mutate(sampyear=factor(sampyear)) %>% 
  select(type, sampyear,'Average (median)', '95% CI (lower limit)', '95% CI (upper limit)', 'Lower quartile', 'Upper quartile' )



table_6_df<-table3 %>% 
  tibble() %>% 
  gather(all_of(measure_levels), key="Measure",value="Value") %>% 
  select(type,sampyear,Measure,Value) %>% 
  spread(key=sampyear, Value) %>% 
  mutate(Measure = factor(Measure, levels = measure_levels)) %>%
  apply_type_formats() %>% 
  mutate(type = factor(type, levels = type_levels)) %>%
  arrange(type, Measure) %>% 
  select(-type) %>% 
  select(7,1:6) %>% 
  rename("Farm type" = farmtype)


# Enterprise tables -------------------------------------------------------


table_7_df<-enterprise_tables[[1]] %>% 
  tibble() %>% 
  arrange(`Farm type`) %>% 
  mutate(Measure = case_when(
    Measure == "Average (median) CO2e/kg dwt" ~ "Average (median)",
    TRUE ~  Measure
  ))%>% 
  mutate(`Farm type`=ifelse(`Farm type`=="Cattle and Sheep (LFA)", "LFA cattle and sheep",
                            ifelse(`Farm type`=="Specialist Cattle (LFA)", "LFA cattle",
                                   ifelse(`Farm type`=="Lowland Cattle and Sheep", "Lowland cattle and sheep",`Farm type`))))


table_8_df<-enterprise_tables[[2]] %>% 
  tibble() %>% 
  arrange(`Farm type`) %>% 
  mutate(Measure = case_when(
    Measure == "Average (median) CO2e/kg dwt" ~ "Average (median)",
    TRUE ~  Measure
  )) %>% 
  mutate(`Farm type`=ifelse(`Farm type`=="Cattle and Sheep (LFA)", "LFA cattle and sheep",
                            ifelse(`Farm type`=="Specialist Sheep (LFA)", "LFA sheep",
                                   ifelse(`Farm type`=="Lowland Cattle and Sheep", "Lowland cattle and sheep",`Farm type`))))


table_9_df<-enterprise_tables[[3]] %>% 
  tibble() %>% 
  arrange(`Farm type`) %>% 
  mutate(Measure = case_when(
    Measure == "Average (median) CO2e/kg dwt" ~ "Average (median)",
    TRUE ~  Measure
  ))


table_10_df<-enterprise_tables[[4]] %>% 
  tibble() %>% 
  arrange(`Farm type`) %>% 
  mutate(Measure = case_when(
    Measure == "Average (median) CO2e/kg dwt" ~ "Average (median)",
    TRUE ~  Measure
  )) %>% 
  mutate(`Farm type`=ifelse(`Farm type`=="General Cropping", "General cropping", `Farm type`))


