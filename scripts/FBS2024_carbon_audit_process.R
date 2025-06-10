
library(readxl)
library(RtoSQLServer)
library(haven)
library(tidyverse)
library(spatstat)
library(data.table)
library(plotly)
source("~/R/agriculture-ghg-emissions-and-nitrogen-use/2023-24/functions.R")
#load 2019-20 and 2020-21 data
load("~/R/agriculture-ghg-emissions-and-nitrogen-use/2023-24/old_carbon.Rda")

server <- "S0196A\\ADM"
database <-  "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "farmbusinesssurvey2024"
# read in datasets from ADM -----

# 2022 = crop year 2022; data year = 2023 (FAID) etc
#crop year 2022 and 2023 are in one dataset

#datayear
current_year <- 2024  # crop year 2023
prev_year <- 2023 # crop year 2022
prev_year_1 <- 2022 # crop year 2021

carbon_current <- RtoSQLServer::read_table_from_db(database = database,
                                 server = server,
                                 schema =  schema,
                                 table_name = "Carbonaudit2022and2023EmissionsReport"
                                )


carbon_prev_1 <- RtoSQLServer::read_table_from_db(database = database,
                                                 server = server,
                                                 schema =  schema,
                                                 table_name = "Carbonaudit2021EmissionsReport"
)

# farm_account <- RtoSQLServer::read_table_from_db(database = database,
#                                               server = server,
#                                               schema =  schema,
#                                               table_name = "Farmaccount"
# )

# read in weights and farm accounts file from SAS ----

SAS_directory <- "//s0177a/sasdata1/ags/fas/"
fbswt <- read_sas(paste0(SAS_directory, "new_weights.sas7bdat"))

#FOR CY2022 AND 2023
farm_account_current <- read_sas(paste0(SAS_directory, "so_y2024_fa.sas7bdat"))

#for CY2021 data
farm_account_prev <- read_sas(paste0(SAS_directory, "so_y2023_fa.sas7bdat"))

# merge farm accounts and fbswt
farm_account_current <- farm_account_current %>% rename(fa_id = FA_ID) %>% 
  left_join(., fbswt)

farm_account_prev <- farm_account_prev %>% rename(fa_id = FA_ID) %>% 
  left_join(., fbswt)

# merge with carbon data to remove farms not in final dataset

carbon_prev <- farm_account_current %>% filter(substr(fa_id, 6,9 ) == prev_year) %>% 
  rename(FA_ID = fa_id) %>% 
  left_join(., carbon_current)
                                     

# merge with new weights
carbon_current <- farm_account_current %>% filter(substr(fa_id, 6,9 ) == current_year) %>% 
  rename(FA_ID = fa_id) %>% 
  left_join(., carbon_current)


# merge with new weights
carbon_prev_1 <- farm_account_prev %>% filter(substr(fa_id, 6,9 ) == prev_year_1) %>% 
  rename(FA_ID = fa_id) %>% 
  left_join(., carbon_prev_1)


#combine

# Create list dynamically with named keys
all_carbon <- list(
  setNames(list(carbon_prev_1), prev_year_1),
  setNames(list(carbon_prev), prev_year),
  setNames(list(carbon_current), current_year)
)

# Flatten the list
all_carbon <- do.call(c, all_carbon)

# calc gross emissions per hectare - divide gross emissions from farming by FA_AAUA
#check with lucy - diff between total_co2_per_ha_calc....

gross_emissions <- map(all_carbon,  ~ .x %>%
                         mutate(total_ha_co2 = `Gross emissions from farming`/FA_AAUA) %>% 
                         select (FA_ID,
                                 type,
                                 total_ha_co2,
                                 fbswt,
                                 Enterprise) %>% filter(Enterprise == "Whole Farm"))


# get summary stats for each farm type at whole farm level
gross_emissions_ft <- map(gross_emissions,  ~.x %>% 
                             group_by(type) %>% C_summarise(.))

# get summary stats for all farms at whole farm level
gross_emissions_all <- map(gross_emissions,  ~.x %>% 
                             C_summarise(.) %>% mutate(type = 9))
 
# add year col
gross_emissions_ft<- map2(
  gross_emissions_ft,
  names(gross_emissions_ft),
  ~ .x %>% mutate(year = .y)
)
gross_emissions_all<- map2(
  gross_emissions_all,
  names(gross_emissions_all),
  ~ .x %>% mutate(year = .y)
)

#combine
gross_emissions_ft <-  bind_rows(gross_emissions_ft, .id = "year")
gross_emissions_all <-  bind_rows(gross_emissions_all, .id = "year")
gross_emissions_all <- bind_rows(gross_emissions_ft, gross_emissions_all)


# add 2019-20 and  2020-21 data

# farmtypes
gross_20ft <- old_carbon %>% filter(substr(fa_id, 6,9 ) == 2020) %>% group_by(type) %>% 
  C_summarise(.) %>% mutate(year = "2020")


gross_20 <- old_carbon %>% filter(substr(fa_id, 6,9 ) == 2020) %>% 
                                    C_summarise(.) %>% mutate(type = 9, year = "2020")


gross_21ft <- old_carbon %>% filter(substr(fa_id, 6,9 ) == 2021) %>% group_by(type) %>% 
  C_summarise(.)%>% mutate(year = "2021")

gross_21 <- old_carbon %>% filter(substr(fa_id, 6,9 ) == 2021) %>% 
  C_summarise(.)%>% mutate(type =9, year = "2021")

# combine with Agrecalc cloud years
gross_emissions_all <-  bind_rows(gross_20, gross_21, gross_20ft, gross_21ft, gross_emissions_all) %>% 
  arrange(type, year) %>% 
  mutate_at(vars(starts_with("CO2e_per_ha")), function(x) x*0.001)

#recode farmtype
gross_emissions_all <- apply_type_formats(gross_emissions_all) %>% rename(sampyear=year)
financial_years <- c("2019-20",
                     "2020-21",
                     paste0(prev_year_1-1,"-", substr(prev_year_1, 3,4)),
                     paste0(prev_year_1,"-", substr(prev_year, 3,4)),
                     paste0(prev_year,"-", substr(current_year, 3,4))
                            )
Output_colnames = c("Farm type", "Measure", c(financial_years))
Output_types = c(9,1,2,3,4,5,6,7,8)

#create output table
Table_1 <- Create_output_table(gross_emissions_all, "CO2e_per_ha", "Table_1")  

#export table_1
write_xlsx(Table_1, path = "C:/Users/U456727/OneDrive - SCOTS Connect/Economic Statistics/FBS/GHG 2023-24/Gross emissions 2023-24/gross_emissions2023-24.xlsx")

#export cloud data as rda for enterprise intensity results

save(all_carbon, file = "2023-24/cloud_carbon.rda")
