# load data and functions
library(openxlsx)
load("data/cloud_carbon.rda")
source("scripts/functions.R")



# clean enterprise names

all_carbon <- map(all_carbon, ~.x %>% mutate(Enterprise = tolower(Enterprise)))

livestock_enterprise <- c("dairy",
                          "sheep",
                          "dairy")

#enterprise checks
#enterprise_23 <- unique(all_carbon$`2023`$Enterprise)
# enterprise_22 <- unique(all_carbon$`2022`$Enterprise)
# enterprise_24 <- unique(all_carbon$`2024`$Enterprise)
# 
# misc <- setdiff(enterprise_23, enterprise_22)
# misc2 <- setdiff(misc, enterprise_24) 
# 
# print(enterprise_23)

cereals_enterprise <- c("spring feed barley",
                       #"oilseed rape (all)",
                       "minor cereals",
                       "spring oats",
                       "feed wheat (all)")
                       #"wholecrop cereals") # query with agrecalc but exclude

# guestimating what to include in a total crop category
total_crops_enterprise <-c("spring feed barley",         
                           "feed wheat (all)",             
                          #"pasture grazing" , 
                          "hay & graze",
                          "silage & graze",
                         #"rough grazing",
                         "oilseed rape (all)", 
                         "spring oats",
                         "field beans (all)",
                         "maincrop processing potatoes",
                         "kale /stubble turnips/ swedes",
                         "minor cereals",
                         "wholecrop cereals",
                         "maincrop ware potatoes",
                         "seed potatoes", 
                         "forage maize",
                         "fodder beet")

#list of crop enterprises exclusive of cereals
no_cereals_enterprise <- setdiff(total_crops_enterprise ,cereals_enterprise)

# calc emission intensity for livestock farms - enterprise level -----

liv_intensity <- map(
  all_carbon,
  ~ .x %>%
    filter(str_detect(Enterprise, str_c(livestock_enterprise, collapse = "|"))) %>% # Filter rows with livestock enterprises
    filter(type %in% c(4:7)) %>%  # Filter rows for farm types with livestock
    group_by(type, Enterprise) %>%                          # Group by type and Enterprise
    select(
      FA_ID,
      type,
      `KgCO2e/kg dwt`,   
      fbswt,
      Enterprise
    )
)


livestock_intensity <- map(liv_intensity, ~u_liv_i_summarise(.))


#make long dataset and add year identifier
livestock_intensity <- year_flat(livestock_intensity) %>%
  apply_type_formats()%>%
  rename(sampyear = year)

#dairy enterprise
dairy <- livestock_intensity %>% filter(str_detect(Enterprise, "dairy"))

#sheep enterprise
sheep <- livestock_intensity %>% filter(str_detect(Enterprise, "sheep"))

#dairy enterprise 
dairy_dairy <- livestock_intensity %>% filter(str_detect(Enterprise, "dairy"))

# dairy enterprise -----

dairy_intensity <- map(
  all_carbon,
  ~ .x %>%
    filter(str_detect(Enterprise, "dairy")) %>% 
    filter(type == 3) %>% # filter for only dairy farms
    group_by(type, Enterprise) %>%                          # Group by type and Enterprise
    select(
      FA_ID,
      type,
      `KgCO2e/kg FPC milk`,   
      fbswt,
      Enterprise
    )
)


dairy_intensity <- map(dairy_intensity, ~u_dairy_i_summarise(.))


#make long dataset
dairy <- year_flat(dairy_intensity) %>% 
  apply_type_formats()%>% 
  rename(sampyear = year)



# cereal enterprise - need to sum quick glance rows to get total for specific crop-----

all_carbon <- map(all_carbon, ~.x %>%
                    mutate(`kgco2e/tonne crop` =rowSums(select(., contains("Quick Glance")), na.rm = TRUE))
)


# calc weighted cereals average - for all cereals enterprises by farmtype----

cereals <- map(
  all_carbon,
  ~ .x %>%
    filter(Enterprise %in% cereals_enterprise)%>%
    group_by(type) %>% 
    filter(type %in% c(1,2)) %>%  # filter for only cereals and GC farms
    select(
      FA_ID,
      type,
      `kgco2e/tonne crop`,   # Explicitly keep necessary columns
      fbswt,
      Enterprise
    )
)             

# remove farm 118332024 (not  in gross emissions )

cereals$`2024` <- cereals$`2024` %>% filter(FA_ID != 118332024)

cereals <-  map(cereals, ~u_cereals_i_summarise(.))

#make long dataset 
cereals <- year_flat(cereals)  %>% apply_type_formats() %>% 
  rename(sampyear= year)
# 
# #change to tonnes?
# cereals_all <-  cereals_all %>% 
#   arrange(type, year) %>% 
#   mutate_at(vars(starts_with("CO2e_per_ha")), function(x) x*0.001)



# total crops emission intensity

# calc weighted cereals average - for all cereals enterprises by farmtype----

total_crops <- map(
  all_carbon,
  ~ .x %>%
    filter(Enterprise %in% total_crops_enterprise)%>%
    group_by(type) %>% 
    filter(type %in% c(1,2)) %>%  # filter for only cereals and GC farms
    select(
      FA_ID,
      type,
      `kgco2e/tonne crop`,   # Explicitly keep necessary columns
      fbswt,
      Enterprise
    )
)             


total_crops <-  map(total_crops, ~u_cereals_i_summarise(.))

#make long dataset 
total_crops <- year_flat(total_crops)  %>% apply_type_formats() %>% 
  rename(sampyear= year)




# no cereals

no_cereals <- map(
  all_carbon,
  ~ .x %>%
    filter(Enterprise %in% no_cereals_enterprise)%>%
    group_by(type) %>% 
    filter(type %in% c(1,2)) %>%  # filter for only cereals and GC farms
    select(
      FA_ID,
      type,
      `kgco2e/tonne crop`,   # Explicitly keep necessary columns
      fbswt,
      Enterprise
    )
)             


no_cereals <-  map(no_cereals, ~u_cereals_i_summarise(.))

no_cereals<- year_flat(no_cereals)  %>% apply_type_formats() %>% 
  rename(sampyear= year)



#recode farmtype
financial_years <- c(
                     "2022-23",
                     "2023-24"
)
Output_colnames = c("Farm type", "Measure", c(financial_years))
Output_types = c(9,1,2,3,4,5,6,7,8)

#create output tables - check missing values
dairy <- Create_output_table(dairy, "CO2e_per_kgdwt", "dairy")  
Sheep <- Create_output_table(sheep, "CO2e_per_kgdwt", "Sheep")  
Dairy_dairy <- Create_output_table(dairy_dairy, "CO2e_per_kgdwt", "Dairy_dairy")  
Dairy <- Create_output_table(dairy, "CO2e_per_kgFPCmilk", "Dairy")  

#cereals
Cereals <- Create_output_table(cereals, "CO2e_per_tonne_crop", "Cereals")  
Crops <- Create_output_table(total_crops, "CO2e_per_tonne_crop", "Crops") 
No_cereals <- Create_output_table(no_cereals, "CO2e_per_tonne_crop", "No_cereals") 
# format for publication
dairy_mean <- dairy %>% select(farmtype, CO2e_per_kgdwt_mean, sampyear) %>% 
  pivot_wider(names_from = sampyear, values_from = (CO2e_per_kgdwt_mean)) %>% mutate (Measure = "Average (median) CO2e/kg dwt")
dairy_n <- dairy %>% select(farmtype, sampyear, simple_count) %>% 
  pivot_wider(names_from = sampyear, values_from = (simple_count)) %>% 
  mutate (Measure = "Sample size")
dairy <- rbind(dairy_mean,
              dairy_n) %>% select(`Farm type` = farmtype, Measure, everything())


#
# #export xlsx
enterprise <- list("dairy_raw" = dairy,
                   "dairy_summary" = dairy,
                   "sheep_raw" =sheep,
                   "sheep_summary" = Sheep,
                   "dairy_dairy_raw" = dairy_dairy,
                   "dairy_dairy_summary" = Dairy_dairy,
                   "dairy_raw" = dairy,
                   "dairy_summary" = Dairy,
                   "cereals_raw" = cereals,
                   "cereals_summary" = Cereals,
                   "total_crops_raw" = total_crops,
                   "total_crops_summary" = Crops,
                   "no_cereals_raw" = no_cereals,
                   "no_cereals_summary" = No_cereals
                   )
#
# Create a new workbook
wb <- createWorkbook()

# Iterate through the list and add each data frame to a worksheet
for (name in names(enterprise)) {
  addWorksheet(wb, name)                      # Add a worksheet with the name of the list item
  writeData(wb, name, enterprise[[name]])    # Write the data frame to the worksheet

  # Set all column widths to 20
  setColWidths(wb, name, cols = 1:ncol(enterprise[[name]]), widths = 20)
}

# Save the workbook to a file
saveWorkbook(wb, "data/unweighted_enterprise_2023-24.xlsx", overwrite = TRUE)




# create tables for publication workbook ----------------------------------

# format for publication
beef_median <- beef %>% select(farmtype, CO2e_per_kgdwt_med, sampyear) %>% 
  pivot_wider(names_from = sampyear, values_from = (CO2e_per_kgdwt_med)) %>% mutate (Measure = "Average (median) CO2e/kg dwt")
beef_n <- beef %>% select(farmtype, sampyear, simple_count) %>% 
  pivot_wider(names_from = sampyear, values_from = (simple_count)) %>% 
  mutate (Measure = "Sample size")

beef_table<-rbind(beef_median,
                  beef_n) %>% select(`Farm type` = farmtype, Measure, everything()) %>% 
  select(-'2022') %>% 
  rename('2022-23'=`2023`, '2023-24'=`2024`) %>%
  filter(`Farm type`!="Specialist Sheep (LFA)")

sheep_median <- sheep %>% select(farmtype, CO2e_per_kgdwt_med, sampyear) %>% 
  pivot_wider(names_from = sampyear, values_from = (CO2e_per_kgdwt_med)) %>% mutate (Measure = "Average (median) CO2e/kg dwt")
sheep_n <- sheep %>% select(farmtype, sampyear, simple_count) %>% 
  pivot_wider(names_from = sampyear, values_from = (simple_count)) %>% 
  mutate (Measure = "Sample size")

sheep_table<-rbind(sheep_median,
                  sheep_n) %>% select(`Farm type` = farmtype, Measure, everything())%>% 
  select(-'2022') %>% 
  rename('2022-23'='2023', '2023-24'='2024') %>%
  filter(`Farm type`!="Specialist Cattle (LFA)")



dairy_median <- dairy %>% select(farmtype, CO2e_per_kgFPCmilk_med, sampyear) %>% 
  pivot_wider(names_from = sampyear, values_from = (CO2e_per_kgFPCmilk_med)) %>% mutate (Measure = "Average (median) CO2e/kg dwt")
dairy_n <- dairy %>% select(farmtype, sampyear, simple_count) %>% 
  pivot_wider(names_from = sampyear, values_from = (simple_count)) %>% 
  mutate (Measure = "Sample size")

dairy_table<-rbind(dairy_median,
                  dairy_n) %>% select(`Farm type` = farmtype, Measure, everything())%>% 
  select(-'2022') %>% 
  rename('2022-23'='2023', '2023-24'='2024')




cereals_median <- cereals %>% select(farmtype, CO2e_per_tonne_crop_med, sampyear) %>% 
  pivot_wider(names_from = sampyear, values_from = (CO2e_per_tonne_crop_med)) %>% mutate (Measure = "Average (median) CO2e/kg dwt")
cereals_n <- cereals %>% select(farmtype, sampyear, simple_count) %>% 
  pivot_wider(names_from = sampyear, values_from = (simple_count)) %>% 
  mutate (Measure = "Sample size")

cereals_table<-rbind(cereals_median,
                   cereals_n) %>% select(`Farm type` = farmtype, Measure, everything())%>% 
  select(-'2022') %>% 
  rename('2022-23'='2023', '2023-24'='2024')


# save tables


enterprise_tables<-list(beef_table, sheep_table, dairy_table, cereals_table)


save(enterprise_tables, file="data/enterprise_tables.rda")
#plot

#cattle and sheep dairy
# 
# plotlybox <- plot_ly(filter(combined, farmtype=="Cattle and Sheep (LFA)", Enterprise %in% c("dairy - dairy Group", "dairy - dairy Group")),
#                      x = ~year, type = "box", 
#                      q1 = ~CO2e_per_kgdwt_Q1, 
#                      median = ~CO2e_per_kgdwt_med, 
#                      q3 = ~CO2e_per_kgdwt_Q3,
#                      lowerfence = ~CO2e_per_kgdwt_Q1, 
#                      upperfence =~ CO2e_per_kgdwt_Q3)
# plotlybox
# 
# #cattle and sheep sheep
# plotlybox <- plot_ly(filter(combined, farmtype=="Cattle and Sheep (LFA)", Enterprise %in% c("Sheep - Sheep Group", "Sheep - SHEEP Group")),
#                      x = ~year, type = "box", 
#                      q1 = ~CO2e_per_kgdwt_Q1, 
#                      median = ~CO2e_per_kgdwt_med, 
#                      q3 = ~CO2e_per_kgdwt_Q3,
#                      lowerfence = ~CO2e_per_kgdwt_Q1, 
#                      upperfence =~ CO2e_per_kgdwt_Q3)
# plotlybox




# QA checks  
# #check - looks like cereals in 2022 are off?
# 
# cereal_check <- map(all_carbon, ~.x %>% filter(`kgco2e/tonne crop`>1000)%>% select(Enterprise, `kgco2e/tonne crop`, type))
# 
# cereals_check_2 <- map(
#   all_carbon,
#   ~ .x %>%
#     arrange(desc(pmax(!!!select(.x, contains("Quick Glance")))))
# )
# 
# cereals_check_2 <- map(all_carbon, ~.x %>% select(FA_ID, type, Enterprise, contains ("Quick Glance"), Fertiliser, `kgco2e/tonne crop`) %>% 
#                          arrange(desc(`Quick Glance Fertiliser & manure`)))
# 
# 
# #fa_id check 
# 
# cereal_check <- map(cereals_check_2, ~.x %>% select(FA_ID, type, Enterprise, contains ("Quick Glance"), `kgco2e/tonne crop`) %>%     filter(str_detect(Enterprise, str_c(cereals_enterprise, collapse = "|")))) 
# 
# # add year col
# cereal_check <- map2(
#   cereal_check,
#   names(cereal_check),
#   ~ .x %>% mutate(year = .y)
# )
# 
# cereal_check_2022 <- cereal_check$`2022` %>% mutate(fa_id = substr(FA_ID, 1,5)) 
# cereal_check_2023 <- cereal_check$`2023` %>% mutate(fa_id = substr(FA_ID, 1,5))  
# cereal_check_2024 <- cereal_check$`2024` %>% mutate(fa_id = substr(FA_ID, 1,5))  
# 
# 
# 
# barley_2022 <- cereal_check_2022 %>% filter(Enterprise == "spring feed barley") %>% select(fa_id, type, Enterprise, contains("Quick Glance") ) 
# 
# barley_2023 <- cereal_check_2023 %>% filter(Enterprise == "spring feed barley") %>% select(fa_id, type, Enterprise, contains("Quick Glance")  ) 
# 
# barley_2024 <- cereal_check_2024 %>% filter(Enterprise == "spring feed barley") %>% select(fa_id, type, Enterprise, contains("Quick Glance")  ) 
# 
# barley_check <- barley_2022 %>% inner_join(barley_2023, by = "fa_id", suffix = c("_2022", "_2023"))
# barley_check <- barley_check %>% inner_join(barley_2024, by ="fa_id", suffix = c("_2024", "_2024") ) %>%  select(sort(names(.)))
# 
# 
# cereal_2022 <- cereal_check_2022 %>% select(fa_id, type, Enterprise, contains("Quick Glance") ) 
# cereal_2023 <- cereal_check_2023 %>% select(fa_id, type, Enterprise, contains("Quick Glance") ) 
# cereal_2024 <- cereal_check_2024 %>% select(fa_id, type, Enterprise, contains("Quick Glance") ) 

  
# cereal_check <- cereal_2022 %>% inner_join(cereal_2023, by = "fa_id", suffix = c("_2022", "_2023"))
# cereal_check <- cereal_check %>% inner_join(cereal_check_2024, by ="fa_id", suffix = c("_2024", "_2024") ) %>%  select(sort(names(.)))
#  write.csv(barley_check, "barley_entrprise_QA.csv")

# old cy2022 data 
missing_enterprise_check <- map(all_carbon, ~.x %>% filter(is.na(Enterprise)))

