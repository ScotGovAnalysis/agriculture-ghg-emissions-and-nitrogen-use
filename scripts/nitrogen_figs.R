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



load(file="data/AllYears_nue.rda")

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


table2<-table2 %>% 
  mutate(year_label=factor(year_label)) %>% 
  rename(Lower=`CI.l(95%).farm_n_surplus.Q[0.500]`,
         Upper=`CI.u(95%).farm_n_surplus.Q[0.500]`,
         Median=`farm_n_surplus.Q[0.500]`)

table2b<-table2b %>% 
  mutate(type=as.numeric(9),
         year_label=factor(year_label))%>% 
  rename(Lower=`CI.l(95%).farm_n_surplus.Q[0.500]`,
         Upper=`CI.u(95%).farm_n_surplus.Q[0.500]`,
         Median=`farm_n_surplus.Q[0.500]`)



table2<-bind_rows(table2, table2b) %>% 
  mutate(across(where(is.numeric), ~ janitor::round_half_up(., 1)))



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


table3<-table3 %>% 
  mutate(year_label=factor(year_label)) %>% 
  rename(Lower=`CI.l(95%).nue.Q[0.500]`,
         Upper=`CI.u(95%).nue.Q[0.500]`,
         Median=`nue.Q[0.500]`)

table3b<-table3b %>% 
  mutate(type=as.numeric(9),
         year_label=factor(year_label))%>% 
  rename(Lower=`CI.l(95%).nue.Q[0.500]`,
         Upper=`CI.u(95%).nue.Q[0.500]`,
         Median=`nue.Q[0.500]`)



table3<-bind_rows(table3, table3b) %>% 
  mutate(across(where(is.numeric), ~ janitor::round_half_up(., 1)))

plotly_line_nitrogen <- function(data, type_value, y_label, yrange) {
  filtered_data <- subset(data, type == type_value)
  
  fig <- plot_ly(
    data = filtered_data,
    x = ~year_label,
    y = ~Upper,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'transparent'),
    showlegend = FALSE,
    text = paste("<br>Year:", filtered_data$year_label, "<br>Upper CI:", filtered_data$Upper),
    hoverinfo = 'text'
  ) %>%
    add_trace(
      y = ~Lower,
      type = 'scatter',
      mode = 'lines',
      fill = 'tonexty',
      fillcolor = '#f2d3ef',
      line = list(color = 'transparent'),
      showlegend = FALSE,
      text = paste("<br>Year:", filtered_data$year_label, "<br>Lower CI", filtered_data$Lower),
      hoverinfo = 'text'
    ) %>%
    add_trace(
      x = ~year_label,
      y = ~Median,
      type = 'scatter',
      mode = 'lines',
      line = list(color='#6a2063'),
      text = paste("<br>Year:", filtered_data$year_label, "<br>Median:", filtered_data$Median),
      hoverinfo = 'text'
    ) %>%
    layout(yaxis = list(title = list(text = y_label, font = t1),
                        range = yrange,
                        tickfont = t1),
           xaxis = list(
             title = list(text = "Year", font = t1, standoff=15),
             tickfont = t1,
             ticklabelposition = "outside",
             ticklen=10,
             tickcolor = "rgba(0,0,0,0)"# pushes tick labels slightly away from axis
             
           ),
            showlegend = FALSE)
  return(fig)
}



# Plotly plots for markdown - report.Rmd -----------------------------------------------

# # Separate figures for each farmtype
#

# All

fig6a <- plotly_line_nitrogen(table2, 9, "Nitrogen balance (kg N surplus/ha)", c(-50,390))
fig6a
# Cereal

fig6b <- plotly_line_nitrogen(table2, 1, "Nitrogen balance (kg N surplus/ha)", c(-50,390))

# General cropping

fig6c <- plotly_line_nitrogen(table2, 2, "Nitrogen balance (kg N surplus/ha)", c(-50,390))

# Dairy

fig6d <- plotly_line_nitrogen(table2, 3, "Nitrogen balance (kg N surplus/ha)", c(-50,390))

# Specialist sheep (LFA)

fig6e <- plotly_line_nitrogen(table2, 4, "Nitrogen balance (kg N surplus/ha)", c(-50,390))

# Specialist cattle (LFA)

fig6f <- plotly_line_nitrogen(table2, 5, "Nitrogen balance (kg N surplus/ha)", c(-50,390))

# Cattle and sheep (LFA)

fig6g <-plotly_line_nitrogen(table2, 6, "Nitrogen balance (kg N surplus/ha)", c(-50,390))

# Lowland cattle and sheep

fig6h <- plotly_line_nitrogen(table2, 7, "Nitrogen balance (kg N surplus/ha)", c(-50,390))

# Mixed

fig6i <- plotly_line_nitrogen(table2, 8, "Nitrogen balance (kg N surplus/ha)", c(-50,390))


# NUE


# All

fig7a <- plotly_line_nitrogen(table3, 9, "Nitrogen use efficiency (%)", c(0,130))

# Cereal

fig7b <- plotly_line_nitrogen(table3, 1, "Nitrogen use efficiency (%)", c(0,130))

# General cropping

fig7c <- plotly_line_nitrogen(table3, 2, "Nitrogen use efficiency (%)", c(0,130))

# Dairy

fig7d <- plotly_line_nitrogen(table3, 3, "Nitrogen use efficiency (%)", c(0,130))

# Specialist sheep (LFA)

fig7e <- plotly_line_nitrogen(table3, 4, "Nitrogen use efficiency (%)", c(0,130))

# Specialist cattle (LFA)

fig7f <- plotly_line_nitrogen(table3, 5, "Nitrogen use efficiency (%)", c(0,130))

# Cattle and sheep (LFA)

fig7g <-plotly_line_nitrogen(table3, 6, "Nitrogen use efficiency (%)", c(0,130))

# Lowland cattle and sheep

fig7h <- plotly_line_nitrogen(table3, 7, "Nitrogen use efficiency (%)", c(0,130))

# Mixed

fig7i <- plotly_line_nitrogen(table3, 8, "Nitrogen use efficiency (%)", c(0,130))
