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


#Use the function to create dataframes for each Table
Table_1_plot <- Create_plot_df(Table_1, "t CO2-e per ha")
Table_2_plot <- Create_plot_df(Table_2, "kg CO2-e per kg output")
Table_3_plot <- Create_plot_df(Table_3, "Nitrogen surplus (kg)")
Table_4_plot <- Create_plot_df(Table_4, "Nitrogen use efficiency (%)")

##Combine the four plot tables into one
Master_plots <- Table_1_plot %>% 
  rbind(Table_2_plot, Table_3_plot, Table_4_plot)
#Create a factor with the four different quantities, in the order that they should be plotted.
#Without this they are arranged alphabetically.
Master_plots$facet_order = factor(Master_plots$Quantity, 
                                  levels = c("t CO2-e per ha", "kg CO2-e per kg output",
                                             "Nitrogen surplus (kg)", "Nitrogen use efficiency (%)"))



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

#, 

Master_plots <- Master_plots %>% 
  mutate(unit=ifelse(Quantity=="t CO2-e per ha"," tCO<sub>2</sub>e/ha", 
                     ifelse(Quantity=="kg CO2-e per kg output", " kg CO<sub>2</sub>e/kg output",
                            ifelse(Quantity=="Nitrogen surplus (kg)", " kg N surplus/ha", "%"))))

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
      x=3.4,
      y=0.98,
      yanchor="top",
      yref="paper",
      text="Methodology change \nin 2022-23                 ",
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
           shapes=list(type='line', yref = "paper", x0= 2.5, x1= 2.5, y0=0, y1=max(yrange), line=list(dash='dot', width=3, color="##b0b0b0"))
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

# # Absolute emissions
# # Separate figures for each farmtype
# 

# All

fig5a <- plotlybox(9, "Absolute emissions", "t CO2-e per ha", y_label="Absolute emissions (tCO<sub>2</sub>e/ha)")
fig5a
# Cereal

fig5b <- plotlybox(1, "Absolute emissions", "t CO2-e per ha", "Absolute emissions (tCO<sub>2</sub>e/ha)")

# General cropping

fig5c <- plotlybox(2, "Absolute emissions", "t CO2-e per ha", "Absolute emissions (tCO<sub>2</sub>e/ha)")

# Dairy

fig5d <- plotlybox(3, "Absolute emissions", "t CO2-e per ha", "Absolute emissions (tCO<sub>2</sub>e/ha)")

# Specialist sheep (LFA) 

fig5e <- plotlybox(4, "Absolute emissions", "t CO2-e per ha", "Absolute emissions (tCO<sub>2</sub>e/ha)")

# Specialist cattle (LFA)

fig5f <- plotlybox(5, "Absolute emissions", "t CO2-e per ha", "Absolute emissions (tCO<sub>2</sub>e/ha)")

# Cattle and sheep (LFA)

fig5g <- plotlybox(6, "Absolute emissions", "t CO2-e per ha", "Absolute emissions (tCO<sub>2</sub>e/ha)")

# Lowland cattle and sheep

fig5h <- plotlybox(7, "Absolute emissions", "t CO2-e per ha", "Absolute emissions (tCO<sub>2</sub>e/ha)")
  
# Mixed

fig5i <- plotlybox(8, "Absolute emissions", "t CO2-e per ha","Absolute emissions (tCO<sub>2</sub>e/ha)")




# Emissions intensity

fig6a <- plotlybox(9, "Emissions intensity", "kg CO2-e per kg output", "Emission intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))
fig6a


fig6b <- plotlybox(1, "Emissions intensity", "kg CO2-e per kg output", "Emission intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6c <- plotlybox(2, "Emissions intensity", "kg CO2-e per kg output", "Emission intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6d <- plotlybox(3, "Emissions intensity", "kg CO2-e per kg output", "Emission intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6e <- plotlybox(4, "Emissions intensity", "kg CO2-e per kg output", "Emission intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6f <- plotlybox(5, "Emissions intensity", "kg CO2-e per kg output", "Emission intensity ((kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6g <- plotlybox(6, "Emissions intensity", "kg CO2-e per kg output", "Emission intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6h <- plotlybox(7, "Emissions intensity", "kg CO2-e per kg output", "Emission intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6i <- plotlybox(8, "Emissions intensity", "kg CO2-e per kg output", "Emission intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))


# Nitrogen balance

fig7a <- plotlyboxnitrogen(9, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=lightpurple, yrange=c(-50,390))
fig7a


fig7b <- plotlyboxnitrogen(1, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=lightpurple, yrange=c(-50,390))

fig7c <- plotlyboxnitrogen(2, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=lightpurple, yrange=c(-50,390))

fig7d <- plotlyboxnitrogen(3, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=lightpurple, yrange=c(-50,390))

fig7e <- plotlyboxnitrogen(4, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=lightpurple, yrange=c(-50,390))

fig7f <- plotlyboxnitrogen(5, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=lightpurple, yrange=c(-50,390))

fig7g <- plotlyboxnitrogen(6, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=lightpurple, yrange=c(-50,390))

fig7h <- plotlyboxnitrogen(7, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=lightpurple, yrange=c(-50,390))

fig7i <- plotlyboxnitrogen(8, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=lightpurple, yrange=c(-50,390))

# Nitrogen use efficiency

fig8a <- plotlyboxnitrogen(9, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(0,130))
fig8a


fig8b <- plotlyboxnitrogen(1, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=lightpurple, yrange=c(0,130))

fig8c <- plotlyboxnitrogen(2, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=lightpurple, yrange=c(0,130))

fig8d <- plotlyboxnitrogen(3, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=lightpurple, yrange=c(0,130))

fig8e <- plotlyboxnitrogen(4, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=lightpurple, yrange=c(0,130))

fig8f <- plotlyboxnitrogen(5, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=lightpurple, yrange=c(0,130))

fig8g <- plotlyboxnitrogen(6, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=lightpurple, yrange=c(0,130))

fig8h <- plotlyboxnitrogen(7, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=lightpurple, yrange=c(0,130))

fig8i <- plotlyboxnitrogen(8, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=lightpurple, yrange=c(0,130))



