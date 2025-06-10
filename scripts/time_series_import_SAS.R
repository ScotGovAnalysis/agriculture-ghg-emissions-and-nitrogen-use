# vars -----
library(tidyverse)
library(data.table)
#missing farmtype - need to bring in additional datasets 
fbs_full_yr <- c(2024,
                 2023,
                 2022,
                 2021,
                 2020,
                 2019,
                 2018,
                 2017,
                 2016,
                 2015,
                 2014,
                 2013,
                 2012)

fbs_yr <- c(2024,
            2023,
            2022,
            2021,
            2020,
            2019,
            2018,
            2017,
            2016)
#2015,
#2014,
#2013)

#import datasets#####
ags_temp <- "//s0177a/sasdata1/ags/census/agstemp/"
fas <- "//s0177a/sasdata1/ags/fas/"
fbs_2024_24 <- haven::read_sas(paste0(ags_temp, "FBS2024_FBI_24.sas7bdat"))
#read in weights file
fbs_wt <- haven::read_sas(paste0(fas, "new_weights.sas7bdat"))

# #names of SAS datasets to import
fbs_name <- lapply(fbs_yr, function(x) paste0("FBS", x, "_FBI_", as.numeric(substr(x, 3,4))-1))

fbs_read <- sapply(fbs_name, function(x) haven::read_sas(paste0(ags_temp, x, ".sas7bdat")))                   
# 
# #years wit hdifferent naming conventions
fbs_yr_2 <- c(2015,2014, 2013)
# 
fbs_name_2 <- lapply(fbs_yr_2, function(x) paste0("TS_SO_FAS", x, "_FBI_", as.numeric(substr(x, 3,4))-1))
fbs_read_2<- lapply(fbs_name_2, function(x) haven::read_sas(paste0(ags_temp, x, ".sas7bdat"))) 


# #join each with new_weights#####
fbs_wt <- fbs_wt %>% rename(FA_ID = fa_id)
# 
# #join lists of fbs datasets
fbs_data_lite<-c(fbs_read, fbs_read_2)

#add 2024 data and add names
fbs_data_lite <- append(list(fbs_2024_24), fbs_data_lite) 
names(fbs_data_lite) <- fbs_full_yr

# #check which don't have weights already attached
# #join fbswt to relevant datasets
fbs_data_lite[c(8:13)] <- lapply(fbs_data_lite[c(8:13)], function(x) left_join(x, fbs_wt, by = "FA_ID"))
# 


#create list of dataset names via naming conventions
fbs_long <- lapply(fbs_full_yr, function(x) paste0("SO_Y", ifelse(x==2024, x, x+1), "_FA"))

#import list of  datasets
fbs_long<- lapply(fbs_long, function(x) haven::
                    read_sas(paste0(fas, x, ".sas7bdat")))  

#keep relevant cols
fbs_long<- lapply(fbs_long, function(x) x %>% select(FA_ID, FA_FBI, type))

#set as dataframe
fbs_long <- lapply(fbs_long, function(x) as.data.frame(x))
fbs_data_lite <- lapply(fbs_data_lite, function(x) as.data.frame(x))


#join datasets together, to get farm type onto agstemp data - fas datasets have FA_ID for both years. Just want one.
time_series <- map2(.x = fbs_data_lite, 
            .y = fbs_long, 
            ~ left_join(.x, .y))





# 
# 
# # calc weighted mean
# #QA
# fbi2024 <- weighted.mean(fbs_2024_24$tox1, fbs_2024_24$fbswt)
# # filter out incomplete cases
# fbs_data <- map(fbs_data, function(x) {
#   x <- x[complete.cases(x), ]
#   return(x)
# })
# all_fbi <- map(fbs_data, function(x) weighted.mean(x$tox1, x$fbswt, na.rm = TRUE))
# 
# ##
# # **                                                                                 **
# #   **   Farmtype :  1-9                                                               **
# #   **               1: Cereal                                                         **
# #   **               2: General Cropping                                               **
# #   **               3: Dairy                                                          **
# #   **               4: LFA Specialist Sheep                                           **
# #   **               5: LFA Specialist Beef                                            **
# #   **               6: LFA Sheep & Cattle                                             **
# #   **               7: Lowland Cattle & Sheep                                         **
# #   **               8: Mixed                                                          **
# #   **               9: All 
# 
# 
# 
# 
# fbs_data <- lapply(fbs_data, function(x) as.data.frame(x))
