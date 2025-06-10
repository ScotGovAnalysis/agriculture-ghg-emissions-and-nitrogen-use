library(spatstat)
library(data.table)
library(tidyverse)
library(haven)

fbs_type_numbers <- c(1:10)
## The "[1]" after "All farm types" is a deliberate footnote.
# fbs_type_words <- c("Cereal", "General Cropping", "Dairy", "Specialist Sheep (LFA)", "Specialist Cattle (LFA)", "Cattle and Sheep (LFA)", "Lowland Cattle and Sheep", "Mixed", "All Types [1]", "Less favoured area (LFA) Livestock")
fbs_type_words <- c("Cereal", "General cropping", "Dairy", "LFA sheep", "LFA cattle", "LFA cattle and sheep", "Lowland cattle and sheep", "Mixed", "All Types", "Less favoured area (LFA) Livestock")
fbs_type_tab <- data.frame(fbs_type_numbers, fbs_type_words)
apply_type_formats <- function(table_name) {
  setkey(setDT(table_name),type)
  table_name[setDT(fbs_type_tab),farmtype:=i.fbs_type_words]
  return(table_name)
}


# can be run from rdas ----------------------------------------------------


#Create carbon output table
#Function to perform the summarising needed
C_summarise <- function(df){
  df <- summarise(df, CO2e_per_ha_mean = weighted.mean(total_ha_co2,fbswt),
                  CO2e_per_ha_Q1 = weighted.quantile(total_ha_co2, fbswt, 0.25),
                  CO2e_per_ha_Q3 = weighted.quantile(total_ha_co2, fbswt, 0.75),
                  CO2e_per_ha_min = min(total_ha_co2),
                  CO2e_per_ha_med = weighted.median(total_ha_co2, fbswt),
                  CO2e_per_ha_max = max(total_ha_co2),
                  
                  # CO2e_per_kg_mean = weighted.mean(total_wf_co2, fbswt),
                  # CO2e_per_kg_Q1 = weighted.quantile(total_wf_co2, fbswt, 0.25),
                  # CO2e_per_kg_Q3 = weighted.quantile(total_wf_co2, fbswt, 0.75),
                  # CO2e_per_kg_min = min(total_wf_co2),
                  # CO2e_per_kg_med = weighted.median(total_wf_co2, fbswt),
                  # CO2e_per_kg_max = max(total_wf_co2),
                  # 
                  # FBI_mean = weighted.mean(fa_fbi, fbswt),
                  # farm_output_kg_mean = weighted.mean(farm_output_kg, fbswt),
                  # farm_output_kg_med = weighted.median(farm_output_kg, fbswt),
                  # farm_output_kg_Q1 = weighted.quantile(farm_output_kg, fbswt, 0.25),
                  # farm_output_kg_Q3 = weighted.quantile(farm_output_kg, fbswt, 0.75),
                  # fbswt_sum = sum(fbswt),
                  simple_count = n())
                  
  return(df)
}

# unweighted
u_C_summarise <- function(df){
  df <- summarise(df, CO2e_per_ha_mean = mean(total_ha_co2),
                  CO2e_per_ha_Q1 = quantile(total_ha_co2, 0.25),
                  CO2e_per_ha_Q3 = quantile(total_ha_co2,  0.75),
                  CO2e_per_ha_min = min(total_ha_co2),
                  CO2e_per_ha_med = median(total_ha_co2),
                  CO2e_per_ha_max = max(total_ha_co2),
                  
                
                 # FBI_mean = mean(fa_fbi, fbswt),
                  # farm_output_kg_mean = mean(farm_output_kg),
                  # farm_output_kg_med = median(farm_output_kg),
                  # farm_output_kg_Q1 = quantile(farm_output_kg, 0.25),
                  # farm_output_kg_Q3 = quantile(farm_output_kg, 0.75),
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
  
  return(df)
}
# summarise emission intensity for livestock (kgco2e/kgdwt)
liv_i_summarise <- function(df){
  df <- summarise(df, CO2e_per_kgdwt_mean = weighted.mean(`KgCO2e/kg dwt`,fbswt),
                  CO2e_per_kgdwt_Q1 = weighted.quantile(`KgCO2e/kg dwt`, fbswt, 0.25),
                  CO2e_per_kgdwt_Q3 = weighted.quantile(`KgCO2e/kg dwt`, fbswt, 0.75),
                  CO2e_per_kgdwt_min = min(`KgCO2e/kg dwt`),
                  CO2e_per_kgdwt_med = weighted.median(`KgCO2e/kg dwt`, fbswt),
                  CO2e_per_kgdwt_max = max(`KgCO2e/kg dwt`),
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
  return(df)
}

# unweighted emission intensity for livestock
u_liv_i_summarise <- function(df){
  df <- summarise(df, CO2e_per_kgdwt_mean = mean(`KgCO2e/kg dwt`),
                  CO2e_per_kgdwt_Q1 = quantile(`KgCO2e/kg dwt`, 0.25),
                  CO2e_per_kgdwt_Q3 = quantile(`KgCO2e/kg dwt`,  0.75),
                  CO2e_per_kgdwt_min = min(`KgCO2e/kg dwt`),
                  CO2e_per_kgdwt_med = median(`KgCO2e/kg dwt`),
                  CO2e_per_kgdwt_max = max(`KgCO2e/kg dwt`),
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
  return(df)
}


# summarise emission intensity for cereals (kgco2e/kgcrop)
cereals_i_summarise <- function(df){
  df <- summarise(df, CO2e_per_tonne_crop_mean = weighted.mean(`kgco2e/tonne crop`,fbswt),
                  CO2e_per_tonne_crop_Q1 = weighted.quantile(`kgco2e/tonne crop`, fbswt, 0.25),
                  CO2e_per_tonne_crop_Q3 = weighted.quantile(`kgco2e/tonne crop`, fbswt, 0.75),
                  CO2e_per_tonne_crop_min = min(`kgco2e/tonne crop`),
                  CO2e_per_tonne_crop_med = weighted.median(`kgco2e/tonne crop`, fbswt),
                  CO2e_per_tonne_crop_max = max(`kgco2e/tonne crop`),
                 fbswt_sum = sum(fbswt),
                  simple_count = n())
  return(df)
}

#unweighted cereals intensity 

u_cereals_i_summarise <- function(df){
  df <- summarise(df, CO2e_per_tonne_crop_mean = mean(`kgco2e/tonne crop`),
                  CO2e_per_tonne_crop_Q1 = quantile(`kgco2e/tonne crop`, 0.25),
                  CO2e_per_tonne_crop_Q3 = quantile(`kgco2e/tonne crop`, 0.75),
                  CO2e_per_tonne_crop_min = min(`kgco2e/tonne crop`),
                  CO2e_per_tonne_crop_med = median(`kgco2e/tonne crop`),
                  CO2e_per_tonne_crop_max = max(`kgco2e/tonne crop`),
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
  return(df)
}


# summarise emission intensity for dairy (KgCO2e/kg FPC milk)
dairy_i_summarise <- function(df){
  df <- summarise(df, CO2e_per_kgdwt_mean = weighted.mean(`KgCO2e/kg FPC milk`,fbswt),
                  CO2e_per_kgFPCmilk_Q1 = weighted.quantile(`KgCO2e/kg FPC milk`, fbswt, 0.25),
                  CO2e_per_kgFPCmilk_Q3 = weighted.quantile(`KgCO2e/kg FPC milk`, fbswt, 0.75),
                  CO2e_per_kgFPCmilk_min = min(`KgCO2e/kg FPC milk`),
                  CO2e_per_kgFPCmilk_med = weighted.median(`KgCO2e/kg FPC milk`, fbswt),
                  CO2e_per_kgFPCmilk_max = max(`KgCO2e/kg FPC milk`),
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
  return(df)
}

# unweighted dairy emission intensity

u_dairy_i_summarise <- function(df){
  df <- summarise(df, CO2e_per_kgdwt_mean = mean(`KgCO2e/kg FPC milk`),
                  CO2e_per_kgFPCmilk_Q1 = quantile(`KgCO2e/kg FPC milk`, 0.25),
                  CO2e_per_kgFPCmilk_Q3 = quantile(`KgCO2e/kg FPC milk`, 0.75),
                  CO2e_per_kgFPCmilk_min = min(`KgCO2e/kg FPC milk`),
                  CO2e_per_kgFPCmilk_med = median(`KgCO2e/kg FPC milk`),
                  CO2e_per_kgFPCmilk_max = max(`KgCO2e/kg FPC milk`),
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
  return(df)
}



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
                  # farm_output_kg_mean = weighted.mean(farm_output_kg, fbswt),
                  # farm_output_kg_med = weighted.median(farm_output_kg, fbswt),
                  # farm_output_kg_Q1 = weighted.quantile(farm_output_kg, fbswt, 0.25),
                  # farm_output_kg_Q3 = weighted.quantile(farm_output_kg, fbswt, 0.75),
                  fbswt_sum = sum(fbswt),
                  simple_count = n())
}



# plots

#Separate script for creating charts
library(janitor)
# Defining a function which rounds in an expected way (rather than, eg,  rounding 53.2500 to 53.2).
round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}


##Function for converting the tables created in CN_analysis.R into a format better for feeding into ggplot.


Create_plot_df <- function(Input_table, variable){
  Output_table <- Input_table %>% 
    gather(all_of(financial_years), key = "Year", value = "Value") %>% 
    spread(key= "Measure", Value) %>% 
    mutate(Quantity = paste(variable))
  colnames(Output_table) <- c("Farm type", "Year", "Median", "Q1", "Q3", "Quantity")
  return(Output_table)
}




darkblue<-"#002d54"
teal<-"#2b9c93"


lightteal<-"#86ded7"
lightblue<-"#cfe8ff"
lighterblue<-"#e6f3ff"
purple<-"#6a2063"
lightpurple<-"#f2d3ef"
lighterpurple<-"#fbf0fa"

# Boxplot for static  visual summary - CN_analysis_md.Rmd


output_plotstatic<- function(i, title_label, quantity_label, y_label, colour_code=darkblue, fill_code=c(lighterblue,lighterblue,lighterblue,lightblue), limits=c(0, 23), NUE_flag=F){
  output_plot_box <- ggplot(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label)) +
    geom_boxplot(aes(x=Year, lower = Q1, upper = Q3,
                     middle = Median, ymin = Q1, ymax = Q3, group=Year),
                 stat = "identity", colour=colour_code, fill=fill_code, width=0.75)+
    theme(panel.grid.minor.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_rect(fill = 'white', colour="white"))+
    scale_y_continuous(expand = c(0, 0), limits = limits)+
    ggExtra::removeGrid(x=TRUE, y=TRUE)+
    ylab(bquote(.(y_label)))
  return(output_plot_box)
}



# Plotly plots for markdown - report.Rmd -----------------------------------------------

t1<-list(family = 'Arial',
         size = 16,
         color = 'rgb(82, 82, 82)')

t2<-list(family = 'Arial',
         size = 16,
         color = 'rgb(82, 82, 82)')

# Make plotly function for figures
#                 \n

plotlybox <- function(i, title_label, quantity_label, y_label, colour_code=darkblue, fill_code=lightblue, yrange=c(0,24), NUE_flag=F){
  
  
  plotlybox <- plot_ly(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label),
                       x = ~Year, type = "box", 
                       q1 = ~round_half_up(Q1, 1), 
                       median = ~round_half_up(Median, 1), 
                       q3 = ~round_half_up(Q3, 1),
                       lowerfence = ~round_half_up(Q1, 1), 
                       upperfence = ~round_half_up(Q3, 1),
                       line = list(color = colour_code),
                       fillcolor=fill_code
                       
                       
  ) %>% 
    
    add_annotations(
      x=2.2,
      y=0.98,
      yanchor="top",
      yref="paper",
      text="Methodology change \nin 2021-22                 ",
      inherit=F,
      font=t2,
      showlegend=FALSE,
      hoverinfo="none"
    ) %>% 
    
    layout(yaxis = list(title = list(text = y_label, font = t1),
                        range = yrange,
                        tickfont = t1),
           xaxis = list(title = list(text="Year", font = t1),
                        tickfont = t1), boxgroupgap=0, showlegend = FALSE,
           shapes=list(type='line', yref = "paper", x0= 1.5, x1= 1.5, y0=0, y1=max(yrange), line=list(dash='dot', width=3, color="##b0b0b0"))
    ) 
  
  
}






plotlyboxnitrogen <- function(i, title_label, quantity_label, y_label, colour_code=darkblue, fill_code=lightblue, yrange=c(0,24), NUE_flag=F){
  
  
  plotlybox <- plot_ly(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label),
                       x = ~Year, type = "box", 
                       q1 = ~round_half_up(Q1, 1), 
                       median = ~round_half_up(Median, 1), 
                       q3 = ~round_half_up(Q3, 1),
                       lowerfence = ~round_half_up(Q1, 1), 
                       upperfence = ~round_half_up(Q3, 1),
                       line = list(color = colour_code),
                       fillcolor=fill_code
                       
                       
  ) %>% 
    
    layout(yaxis = list(title = list(text = y_label, font = t1),
                        range = yrange,
                        tickfont = t1),
           xaxis = list(title = list(text="Year", font = t1),
                        tickfont = t1), boxgroupgap=0, showlegend = FALSE
           
    ) 
  
  
}



plotlybox_i <- function(i, title_label, quantity_label, y_label, colour_code=darkblue, fill_code=lightblue, yrange=c(0, ~max(Q3)), NUE_flag=F){
  
  
  plotlybox <- plot_ly(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label),
                       x = ~Year, type = "box", 
                       q1 = ~round_half_up(Q1, 1), 
                       median = ~round_half_up(Median, 1), 
                       q3 = ~round_half_up(Q3, 1),
                       lowerfence = ~round_half_up(Q1, 1), 
                       upperfence = ~round_half_up(Q3, 1),
                       line = list(color = colour_code),
                       fillcolor=fill_code
                       
                       
  ) %>% 
    
    layout(yaxis = list(title = list(text = y_label, font = t1),
                        range = yrange,
                        tickfont = t1),
           xaxis = list(title = list(text="Year", font = t1),
                        tickfont = t1)
           
    ) 
  
  
}

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
           "Sample size" = "simple_count",
           everything()) %>% 
    gather("Average (median)", "Lower quartile", "Upper quartile", "Sample size", key="Measure",value="Value") %>% 
    select("Farm type"=farmtype,type,sampyear,Measure,Value) %>% 
    spread(key=sampyear, Value)
  Table_name$type <- factor(Table_name$type, levels = Output_types)
  Table_name <- Table_name[order(Table_name$type),] %>% 
    select(-type)
  colnames(Table_name) <- Output_colnames
  return(Table_name)
}


#function for enterprise output tables
create_enterprise_summary <- function(data, variable) {
  ent_med <- data %>%
    select(farmtype, variable, sampyear) %>%
    pivot_wider(names_from = sampyear, values_from = paste0(variable, "_med")) %>%
    mutate(Measure = paste0("Average (median)", variable))
  
  ent_n <- data %>%
    select(farmtype, sampyear, simple_count) %>%
    pivot_wider(names_from = sampyear, values_from = simple_count) %>%
    mutate(Measure = "Sample size")
  
  ent <- bind_rows(ent_med, ent_n) %>%
    select(`Farm type` = farmtype, Measure, everything())
  
  return(ent)
}
#function to add year and combine to one dataframe

year_flat <- function(x) {
  map2(x, names(x), ~ .x %>%
         mutate(year = .y)) %>%
    bind_rows(.id = "year") # This combines all elements into one dataframe
}

# function to format output tables for publication
hc_format <- function(x){x %>% pivot_longer(cols = c(-`Farm type`, - Measure), names_to = "year",
                                            values_to = "value") %>% mutate(Measure = factor(Measure, levels = measure_order)) %>%
    group_by(year) %>% 
    arrange(Measure) }
