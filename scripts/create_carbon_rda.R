# create carbon data - old
cropyear_range <- c(2019:2021)
#2019-20 and 2020-21

FBS_directory_path <- Sys.getenv("FBS_directory_path")
Z_drive_path <- Sys.getenv("Z_drive_path")

#Use cropyears to determine sampyears
sampyear_range <- cropyear_range + 1
#Figure out the financial years associated with each sampyear. Eg., sampyear "2020" is financial year "2019/20"
financial_years_start <- sampyear_range - 1
financial_years_end <- sampyear_range - 2000
financial_years <- paste0(financial_years_start,"-",financial_years_end)

#Loop to read in data
for (sampyear in sampyear_range){
  # Calculate datayear, given sampyear. For the most recently-available data, datayear=sampyear (provisional data). 
  # For older data, it will be datayear=sampyear+1 (final)
  if(sampyear==max(sampyear_range)){
    datayear=sampyear
  }  else {
    datayear=sampyear + 1
  }
  #Initialise dataframes (first year in sampyear range only)
  if(sampyear==min(sampyear_range)){
    
    AllYears_fa <- NULL
    AllYears_carbon <- NULL
    AllYears_nue <- NULL
  }
  ## Filenames for Farm account, carbon audit and NUE datasets
  # NB: sampyear rather than datayear in NUE filename
  FBS_fa_data_file <- paste0(FBS_directory_path,"so_y", datayear, "_fa",".sas7bdat")
  FBS_carbon_file <- paste0(FBS_directory_path,  "so_y", datayear, "_carbon",".sas7bdat")
  FBS_nue_file <- paste0(FBS_directory_path, "so_y", sampyear, "_nue",".sas7bdat")
  
  #Single year's FA data
  if (file.exists(FBS_fa_data_file)) {
    FBS_fa_data <- tryCatch(
      {
        read_sas(FBS_fa_data_file)
      },
      error = function(e) {
        warning("Error reading file, continuing without it.")
        return(read_sas(FBS_fa_data_file))
      }
    )
  } else {
    warning("File not found, continuing without it.")
    FBS_fa_data <- NULL
  }
  ##Basic data cleaning - convert all column names to lower case and strip sas formatting
  names(FBS_fa_data) <- tolower(names(FBS_fa_data))
  for (x in colnames(FBS_fa_data)){
    attr(FBS_fa_data[[deparse(as.name(x))]],"format.sas")=NULL
  }
  #Process FA data.
  FBS_fa_data_tidy <- FBS_fa_data %>% 
    filter(fa_id%%10000==sampyear) %>% 
    select(fa_id, type, fa_fbi, fa_aaua) %>% 
    mutate(sampyear=fa_id%%10000)
 
  
   #Single year's carbon data
  
  if (file.exists(FBS_carbon_file)) {
    FBS_carbon_data <- tryCatch(
      {
        read_sas(FBS_carbon_file)
      },
      error = function(e) {
        warning("Error reading file, continuing without it.")
        return(read_sas(FBS_carbon_file))
      }
    )
  } else {
    warning("File not found, continuing without it.")
    FBS_carbon_data <- NULL
  }

  ##Basic data cleaning - convert all column names to lower case and strip sas formatting
  names(FBS_carbon_data) <- tolower(names(FBS_carbon_data))
  for (x in colnames(FBS_carbon_data)){
    attr(FBS_carbon_data[[deparse(as.name(x))]],"format.sas")=NULL
  }
  #Process carbon data
  FBS_carbon_data_tidy <- FBS_carbon_data %>%
    filter(fa_id%%10000==sampyear)
  
 
  #Process NUE data
  

  # Append each year's data to All Years dataset, ensuring the data frames are not NULL
  if (!is.null(FBS_fa_data_tidy) && nrow(FBS_fa_data_tidy) > 0) {
    AllYears_fa <- bind_rows(AllYears_fa, FBS_fa_data_tidy)
  }
  # 
  if (!is.null(FBS_carbon_data_tidy) && nrow(FBS_carbon_data_tidy) > 0) {
    AllYears_carbon <- bind_rows(AllYears_carbon, FBS_carbon_data_tidy)
  }

  # if (!is.null(FBS_nue_data_tidy) && nrow(FBS_nue_data_tidy) > 0) {
  #   AllYears_nue <- bind_rows(AllYears_nue, FBS_nue_data_tidy)
  # }
}



#Read in the FBS weights file
FBS_weights_file <- paste0(FBS_directory_path,"new_weights.sas7bdat")
FBS_weights <- tryCatch(
  {
    FBS_weights <- read_sas(FBS_weights_file)
  },
  error = function(e)
  {
    return(read_sas(FBS_weights_file))
  }
)

##Basic data cleaning - convert all column names to lower case and strip sas formatting
names(FBS_weights) <- tolower(names(FBS_weights))
for (x in colnames(FBS_weights)){
  attr(FBS_weights[[deparse(as.name(x))]],"format.sas")=NULL
}

#Join weights/farm account to carbon and nue datasets
old_carbon <- AllYears_carbon %>%
  inner_join(FBS_weights, by="fa_id") %>%
  inner_join(AllYears_fa, by="fa_id")

save(old_carbon , file ="data/old_carbon.Rda")

