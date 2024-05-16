#File sorts and loads data 
#Data include dataframes used in plots and data tables
#load libraries ----
library(readxl) 
library(tidyverse)

# set parameters ----

sectors <- c("Arable", "Dairy", "Dairy beef", "Other", "Sheep", "Suckler beef"
#, "Total"
)

#load data----
#read in sector totals
sec_tot <- read_excel("Data/national_data.xlsx", sheet = "subsector_total")
#names(sec_tot) <- gsub("X", "", names(sec_tot))

# read in subsector gas compostion for latest year bar chart
sec_comp_latest <- read_excel("Data/national_data.xlsx", sheet = "subsector_comp_latest")

#sort data----

# sect totals
#pivot longer
sec_tot <- sec_tot %>% pivot_longer(cols = where(is.numeric), names_to = "Year", values_to = "ghg_emiss")
sec_tot <- sec_tot %>% filter(Sector != "Total") 
sec_tot$Year <- as.numeric(sec_tot$Year)


#sector in cols - pivot wider
sec_tot <- sec_tot %>% pivot_wider(names_from = Sector, values_from = c(ghg_emiss))


# sect composition 
