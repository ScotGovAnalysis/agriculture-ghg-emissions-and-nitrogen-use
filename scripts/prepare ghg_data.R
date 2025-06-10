# create ghg data rda - taken from ag stats hub data.R script
#load libraries ----
library(readxl) 
library(tidyverse)
library(data.table)
library(kableExtra)
# To update data, replace file path and run the below script.

# Load  data from the Excel file
file_path <- "C:/Users/U455049/OneDrive - SCOTS Connect/Agricultural Statistics Unit - Economic Statistics/FBS/GHG 2023-24/sectoral analysis/GHG inventory data 2023.xlsx"

agri_gas <- read_excel(file_path, sheet = "agri_gas")
national_total <- read_excel(file_path, sheet = "national_total")
subsector_total <- read_excel(file_path, sheet = "subsector_total")
subsector_source <- read_excel(file_path, sheet = "subsector_source")

agri_gas <- agri_gas %>% 
  rename(Gas = ...1)

# Reshape the data to long format
agri_gas <- agri_gas %>% pivot_longer(cols = -Gas, names_to = "Year", values_to = "Value")
national_total <- national_total %>% pivot_longer(cols = -Industry, names_to = "Year", values_to = "Value")
subsector_total <- subsector_total %>% pivot_longer(cols = -Subsector, names_to = "Year", values_to = "Value")

# Convert Year to numeric
agri_gas$Year <- as.numeric(agri_gas$Year)
national_total$Year <- as.numeric(national_total$Year)
subsector_total$Year <- as.numeric(subsector_total$Year)

# Merge the specified sources into 'Other emission source'
subsector_source <- subsector_source %>%
  filter(Source != "Urea application") %>% 
  filter(Source != "Non-energy products from fuels and solvent use")  %>%
  group_by(Source) %>%
  summarise(across(everything(), sum)) %>%
  ungroup()

save(subsector_total, agri_gas, national_total, subsector_source, file = "data/ghg_data.RData")

