# gross emissions plots
library(tidyverse)
library(ReGenesees)
library(haven)
library(plotly)
source("scripts/functions.R")


darkblue<-"#002d54"
teal<-"#2b9c93"


lightteal<-"#86ded7"
lightblue<-"#cfe8ff"
lighterblue<-"#e6f3ff"
purple<-"#6a2063"
lightpurple<-"#f2d3ef"
lighterpurple<-"#fbf0fa"


# gross emissions ---------------------------------------------------------


# Process data

load("data/cloud_carbon.rda")
load("data/old_carbon.Rda")

# summary of carbon variables to explain emissions results

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



table1<-table1 %>% 
  mutate(sampyear=factor(sampyear)) %>% 
  rename(Lower=`CI.l(95%).total_ha_co2.Q[0.500]`,
         Upper=`CI.u(95%).total_ha_co2.Q[0.500]`,
         Median=`total_ha_co2.Q[0.500]`)

table1b<-table1b %>% 
  mutate(type=as.numeric(9),
         sampyear=factor(sampyear))%>% 
  rename(Lower=`CI.l(95%).total_ha_co2.Q[0.500]`,
         Upper=`CI.u(95%).total_ha_co2.Q[0.500]`,
         Median=`total_ha_co2.Q[0.500]`)



table1<-bind_rows(table1, table1b) 

# save for download tables

save(table1, file="data/tablebothmethods.rda")


# round for charts

table1<-table1%>% 
  mutate(across(where(is.numeric), ~ janitor::round_half_up(., 1)))



t1<-list(family = 'Arial',
         size = 16,
         color = 'rgb(82, 82, 82)')

t2<-list(family = 'Arial',
         size = 14,
         color = 'rgb(82, 82, 82)')




plotly_line_break <- function(data, type_value, y_label, yrange) {
  filtered_data <- subset(data, type == type_value)
  
  # Define consistent styling
  line_color <- "rgb(0,100,80)"
  
  fig <- plot_ly()
  
  # Loop through shading groups
  for (sh in unique(filtered_data$method)) {
    subdata <- subset(filtered_data, method == sh)
    
    fig <- fig %>%
      # Upper line (transparent to define upper bound of fill)
      add_trace(
        data = subdata,
        x = ~sampyear,
        y = ~Upper,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'transparent'),
        showlegend = FALSE,
        text = paste("Method:", sh, "<br>Year:", subdata$sampyear, "<br>Upper CI:", subdata$Upper),
        hoverinfo = 'text'
      ) %>%
      # Lower line with fill to upper
      add_trace(
        data = subdata,
        x = ~sampyear,
        y = ~Lower,
        type = 'scatter',
        mode = 'lines',
        fill = 'tonexty',
        fillcolor = "rgba(0,100,80,0.2)",
        line = list(color = 'transparent'),
        showlegend = FALSE,
        text = paste("Method:", sh, "<br>Year:", subdata$sampyear, "<br>Lower CI:", subdata$Lower),
        hoverinfo = 'text'
      ) %>%
      # Median line with fixed color
      add_trace(
        data = subdata,
        x = ~sampyear,
        y = ~Median,
        type = 'scatter',
        mode = 'lines',
        line = list(color = line_color),
        text = paste("Method:", sh, "<br>Year:", subdata$sampyear, "<br>Median:", subdata$Median),
        hoverinfo = 'text',
        showlegend = FALSE
      )
  }
  
  # Optional annotation
  fig <- fig %>%
    add_annotations(
      x = 2.68,
      y = 0.98,
      yanchor = "top",
      align ="left",
      yref = "paper",
      text = "Methodology improvement \n from 2021-22 onwards",
      inherit = FALSE,
      font = t1,
      showlegend = FALSE,
      hoverinfo = "none"
    ) %>%
    layout(
      yaxis = list(
        title = list(text = y_label, font = t1),
        range = yrange,
        tickfont = t1
      ),
      xaxis = list(
        title = list(text = "Year", font = t1, standoff=15),
        tickfont = t1,
        ticklabelposition = "outside",
        ticklen=10,
        tickcolor = "rgba(0,0,0,0)"# pushes tick labels slightly away from axis
        
      ),
      shapes = list(
        type = 'line',
        yref = "paper",
        x0 = 2, x1 = 2,
        y0 = 0, y1 = max(yrange),
        line = list(dash = 'dot', width = 3, color = "#b0b0b0")
      ),
      showlegend = FALSE
    )
  
  return(fig)
}

# create function for line chart

  
# Plotly plots for markdown - report.Rmd -----------------------------------------------
# # Absolute emissions
# # Separate figures for each farmtype
#

# All

fig5a <- plotly_line_break(table1, 9, "Absolute emissions (tCO<sub>2</sub>e/ha)", c(0,20))
fig5a
# Cereal

fig5b <- plotly_line_break(table1, 1, "Absolute emissions (tCO<sub>2</sub>e/ha)", c(0,20))

# General cropping

fig5c <- plotly_line_break(table1, 2, "Absolute emissions (tCO<sub>2</sub>e/ha)", c(0,20))

# Dairy

fig5d <- plotly_line_break(table1, 3, "Absolute emissions (tCO<sub>2</sub>e/ha)", c(0,20))

# Specialist sheep (LFA)

fig5e <- plotly_line_break(table1, 4, "Absolute emissions (tCO<sub>2</sub>e/ha)", c(0,20))

# Specialist cattle (LFA)

fig5f <- plotly_line_break(table1, 5, "Absolute emissions (tCO<sub>2</sub>e/ha)", c(0,20))

# Cattle and sheep (LFA)

fig5g <-plotly_line_break(table1, 6, "Absolute emissions (tCO<sub>2</sub>e/ha)", c(0,20))

# Lowland cattle and sheep

fig5h <- plotly_line_break(table1, 7, "Absolute emissions (tCO<sub>2</sub>e/ha)", c(0,20))

# Mixed

fig5i <- plotly_line_break(table1, 8, "Absolute emissions (tCO<sub>2</sub>e/ha)", c(0,20))


