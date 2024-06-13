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
lightblue<-"#c3e3ff"
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
         size = 1,
         color = "darkblue")

# Make plotly function for figures

plotlybox <- function(i, title_label, quantity_label, y_label, colour_code=darkblue, fill_code=c("white","white","white",lightblue), yrange=c(0,24), NUE_flag=F){
  
  
  plotlybox <- plot_ly(filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label),
                       x = ~Year, type = "box", 
                       q1 = ~round_half_up(Q1, 2), 
                       median = ~round_half_up(Median, 2), 
                       q3 = ~round_half_up(Q3, 2),
                       lowerfence = ~round_half_up(Q1, 2), 
                       upperfence = ~round_half_up(Q3, 2),
                       line = list(color = colour_code),
                       hoverinfo = "none",
                       fillcolor=fill_code
  ) %>% 
    add_text(
      x=~Year,
      y=~Median,
      text=~paste("Year", Year, "Median:",round_half_up(Median,2)),
       textposition="top",
       inherit=F,
       textfont=list(family = 'Arial',
                     size = 1,
                     color = colour_code)
      ,
       showlegend=FALSE,
      hoverinfo="text",
       # colour="white"
    ) %>%
    layout(yaxis = list(title = list(text = y_label, font = t1),
                        range = yrange,
                        tickfont = t1),
           xaxis = list(title = list(font = t1),
                        tickfont = t1), boxgroupgap=0, showlegend = FALSE) 
  # %>% 
    # style(hoverinfo = "none")

}

# 
# plotlybox <- function(i, title_label, quantity_label, y_label, colour_code=darkblue, fill_code=c("white","white","white",lightblue), yrange=c(0,24), NUE_flag=F){
#   data<-filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label)
#   
#   data<-data %>% mutate(hovertext=paste("Year:",Year, "Median:", Median))
#  ggbox <-  ggplot(data, text=paste("Group:",Year, "\n",
#                                              "Class:", Median)) +
#     geom_boxplot(aes(x=Year, lower = Q1, upper = Q3,
#                      middle = Median, ymin = Q1, ymax = Q3, group=Year),
#                  stat = "identity")+
#     
#     #invisible layer of points
#     geom_point(alpha = 0)
#   
#   ggbox %>% plot_ly(data,
#                        x = ~Year, type = "box", 
#                        q1 = ~round_half_up(Q1, 2), 
#                        median = ~round_half_up(Median, 2), 
#                        q3 = ~round_half_up(Q3, 2),
#                        lowerfence = ~round_half_up(Q1, 2), 
#                        upperfence = ~round_half_up(Q3, 2),
#                        fillcolor=fill_code,
#                        line = list(color = colour_code),
#                        hoverinfo = "none"
#                        
#   ) %>% 
#     add_text(
#       x=~Year,
#       y=~max(Q3)+1, 
#       text=~round_half_up(Median,2),
#       textposition="top",
#       inherit=F,
#       font=list(family=t1),
#       showlegend=FALSE,
#       hoverinfo="none"
#     ) %>% 
#     layout(yaxis = list(title = list(text = y_label, font = t1),
#                         range = yrange,
#                         tickfont = t1),
#            xaxis = list(title = list(font = t1),
#                         tickfont = t1), boxgroupgap=0, showlegend = FALSE) 
#   # %>% 
#   # style(hoverinfo = "none")
#   
# }
# 
# 
# 
# 
# Master_plots<-Master_plots %>% 
#   mutate(hovertext=paste("Year:",Year, "Median:", Median))
# 
# fig <- plot_ly(Master_plots, x = ~Year)
# 
# plotlybox1 <- function(i, title_label, quantity_label, y_label, colour_code=darkblue,  yrange=c(0,24), NUE_flag=F){
#   data<-filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label)
#   
#   data<-data %>% mutate(hovertext=paste("Year:",Year, "Median:", Median))
#   
#   fill_code<-ifelse(data$Year=="2022-23", lightblue, "white")
#   
#                             fig<-plot_ly(data, x = ~Year, type = "box", boxpoints = "all", 
#                              q1 = ~round_half_up(Q1, 2), 
#                              median = ~round_half_up(Median, 2), 
#                              q3 = ~round_half_up(Q3, 2),
#                              lowerfence = ~round_half_up(Q1, 2), 
#                              upperfence = ~round_half_up(Q3, 2),
#                              fillcolor=fill_code,
#                              line = list(color = colour_code)
#                        #  text=~hovertext,
#                        # hoverinfo = "text"
#                        # 
#                        ) %>% 
#     layout(yaxis = list(title = list(text = y_label, font = t1),
#                         range = yrange,
#                                tickfont = t1),
#                   xaxis = list(title = list(font = t1),
#                                tickfont = t1), boxgroupgap=0, showlegend = FALSE) %>% 
#                               style(hoverinfo="none")
#   
# }
# 
# 
# plotlybox2 <- function(i, title_label, quantity_label, y_label, colour_code=darkblue,  yrange=c(0,24), NUE_flag=F){
#   data<-filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label)
#   
#   data<-data %>% mutate(hovertext=paste("Year:",Year, "Median:", Median))
#   
#   fill_code<-ifelse(data$Year=="2022-23", lightblue, "white")
#   
#   fig<-plot_ly(data, x = ~Year, type = "box", boxpoints = "all", 
#                q1 = ~round_half_up(Q1, 2), 
#                median = ~round_half_up(Median, 2), 
#                q3 = ~round_half_up(Q3, 2),
#                lowerfence = ~round_half_up(Q1, 2), 
#                upperfence = ~round_half_up(Q3, 2),
#                fillcolor=fill_code,
#                line = list(color = colour_code)
#                #  text=~hovertext,
#                # hoverinfo = "text"
#                # 
#   ) %>% 
#     layout(yaxis = list(title = list(text = y_label, font = t1),
#                         range = yrange,
#                         tickfont = t1),
#            xaxis = list(title = list(font = t1),
#                         tickfont = t1), boxgroupgap=0, showlegend = FALSE)
#   
# }
# 
# 
# 
# Master_plots<-Master_plots %>% 
#   mutate(hovertext=paste("Year:",Year, "Median:", Median))
# 
# fig <- plot_ly(Master_plots, x = ~Year)
# 
# plotlybox <- function(i, title_label, quantity_label, y_label, colour_code=darkblue, fill_code=c("white","white","white",lightblue), yrange=c(0,24), NUE_flag=F){
#   data<-filter(Master_plots, `Farm type`==fbs_type_words[i], Quantity == quantity_label)
#          
#          data<-data %>% mutate(hovertext=paste("Year:",Year, "Median:", Median))
#                        
#          fig<-plot_ly() %>% 
#            for(j in 1:row(data)){
#              fig<-fig %>% 
#                add_trace( type = "box", x = rep(stats$Year[j], 5), # Repeat year for each box parameter 
#                           lowerfence = stats$Min[j], q1 = stats$Q1[j], median = stats$Median[j], q3 = stats$Q3[j], upperfence = stats$Max[j], name = as.character(stats$Year[j]), fillcolor = fill_code[j %% length(fill_code) + 1], line = list(color = colour_code), text = stats$hovertext[j], hoverinfo = "text")
#            }
#     fig<-fig %>% layout(yaxis = list(title = list(text = y_label, font = t1),
#                         range = yrange,
#                         tickfont = t1),
#            xaxis = list(title = list(font = t1),
#                         tickfont = t1), boxgroupgap=0, showlegend = FALSE)
# }
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

fig6a <- plotlybox(9, "Emissions intensity", "kg CO2-e per kg output", "Emissions intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))
fig6a


fig6b <- plotlybox(1, "Emissions intensity", "kg CO2-e per kg output", "Emissions intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6c <- plotlybox(2, "Emissions intensity", "kg CO2-e per kg output", "Emissions intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6d <- plotlybox(3, "Emissions intensity", "kg CO2-e per kg output", "Emissions intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6e <- plotlybox(4, "Emissions intensity", "kg CO2-e per kg output", "Emissions intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6f <- plotlybox(5, "Emissions intensity", "kg CO2-e per kg output", "Emissions intensity ((kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6g <- plotlybox(6, "Emissions intensity", "kg CO2-e per kg output", "Emissions intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6h <- plotlybox(7, "Emissions intensity", "kg CO2-e per kg output", "Emissions intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))

fig6i <- plotlybox(8, "Emissions intensity", "kg CO2-e per kg output", "Emissions intensity (kg CO<sub>2</sub>e/kg output)", yrange=c(0,42))


# Nitrogen balance

fig7a <- plotlybox(9, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(-50,390))
fig7a


fig7b <- plotlybox(1, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(-50,390))

fig7c <- plotlybox(2, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(-50,390))

fig7d <- plotlybox(3, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(-50,390))

fig7e <- plotlybox(4, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(-50,390))

fig7f <- plotlybox(5, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(-50,390))

fig7g <- plotlybox(6, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(-50,390))

fig7h <- plotlybox(7, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(-50,390))

fig7i <- plotlybox(8, "Nitrogen balance", "Nitrogen surplus (kg)", "Nitrogen balance (kg N surplus/ha)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(-50,390))
# Nitrogen use efficiency

fig8a <- plotlybox(9, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(0,130))
fig8a


fig8b <- plotlybox(1, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(0,130))

fig8c <- plotlybox(2, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(0,130))

fig8d <- plotlybox(3, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(0,130))

fig8e <- plotlybox(4, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(0,130))

fig8f <- plotlybox(5, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(0,130))

fig8g <- plotlybox(6, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(0,130))

fig8h <- plotlybox(7, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(0,130))

fig8i <- plotlybox(8, "Nitrogen use efficiency", "Nitrogen use efficiency (%)", "Nitrogen use efficiency (%)", purple, fill_code=c(lightpurple,lightpurple,lightpurple,lightpurple), yrange=c(0,130))



