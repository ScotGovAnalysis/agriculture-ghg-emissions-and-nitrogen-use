#File sorts and loads data 
#Data include dataframes used in plots and data tables
#load libraries ----
library(readxl) 
library(tidyverse)
library(data.table)

# set parameters ----
CurrentYear = 2021
sectors <- c("Arable", "Dairy", "Dairy beef", "Other", "Sheep", "Suckler beef"
#, "Total"
)

national_data <- "//s0177a/datashare/seerad/fas/emissions_pub/national_data.xlsx"

#load data----
#read in national totals
nat_tot <- read_excel(national_data, sheet = "national_total")


#read in sector totals
sec_tot <- read_excel(national_data, sheet = "subsector_total")
#names(sec_tot) <- gsub("X", "", names(sec_tot))

# read in subsector gas compostion for latest year bar chart
sec_comp_latest <- read_excel(national_data, sheet = "subsector_comp_latest")

# read in subsector gas compostion for latest year bar chart
sec_source <- read_excel(national_data, sheet = "subsector_source")



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
latest_year_gross_data <- nat_tot_98 %>% filter(Year %in% CurrentYear)
nineties_gross_data <- nat_tot %>% filter(Year == 1990)
  
# fig 1b ----
# current year sort for bar chart
nat_tot_current <- nat_tot %>% filter(Year == CurrentYear) %>% select(-Year, - `Total (RHS)`) 
#%>% rename(`Total Emissions`= `Total (RHS)`)
nat_tot_current <- nat_tot_current %>% pivot_longer(cols = everything(), names_to = "Industry", values_to = "ghg_emiss") 
# sort by ascending order
nat_tot_current <- nat_tot_current %>% dplyr::arrange(ghg_emiss)
#for plotly- it ignores order of dataset - need to get order and then set Industry as factor
nat_order <- nat_tot_current$Industry
nat_tot_current$Industry <- factor(nat_tot_current$Industry, levels = nat_order)

# fig 2----
# sect totals - fig 2
# pivot longer
sec_tot <- sec_tot %>% pivot_longer(cols = where(is.numeric), names_to = "Year", values_to = "ghg_emiss")
sec_tot <- sec_tot %>% filter(Sector != "Total") 
sec_tot$Year <- as.numeric(sec_tot$Year)


# sector in cols - pivot wider
sec_tot <- sec_tot %>% pivot_wider(names_from = Sector, values_from = c(ghg_emiss))

sec_tot_98 <- sec_tot %>% filter(Year > 1995)
latest_year_data <- sec_tot_98 %>% filter(Year %in% CurrentYear)
nineties_data <- sec_tot %>% filter(Year == 1990)


# fig 3----

# order sector latest data for bar chart - fig 3
# create total first
sec_comp_latest$total <- rowSums(sec_comp_latest[-1])
sec_comp_latest <- sec_comp_latest %>% dplyr::arrange(total)
#for plotly- it ignores order of dataset - need to get order and then set Industry as factor
sec_order <- sec_comp_latest$Sector
sec_comp_latest$Sector<- factor(sec_comp_latest$Sector, levels = sec_order)


# fig 4 ----
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
sec_source <- sec_source %>% select(Sector, everything())