library(sgplot)
library(plotly)
library(ggrepel)
sgplot::use_sgplot()
#  import data -----
source("scripts/data_sort.R")

# set parameters-----

xaxislabels=c(
  CurrentYear-31,"",
  " ", "", "",
  " ", CurrentYear-23,
  CurrentYear-21,
  " ", CurrentYear-19,
  " ", CurrentYear-17, 
  " ", CurrentYear-15,
  " ", CurrentYear-13, 
  " ", CurrentYear-11,
  " ", CurrentYear-9,
  " ", CurrentYear-7,
  " ", CurrentYear-5, 
  " ", CurrentYear-3,
  " ", CurrentYear-1,
  CurrentYear, "", "", "", "")

t1 <- list(family = 'Arial',
           size = 16,
           color = 'rgb(82, 82, 82)')

xaxis <- list(title = list(text = "Year", font = t1),
              ticktext = list(xaxislabels),
              #tickvals = list(seq(1990, CurrentYear)),
              tickmode = "array",
              range = c(1989.5, CurrentYear+3),
              fixedrange = TRUE,
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 5,
              tickfont = t1)

yaxis <- list(title = list( text = "MtCO<sub>2</sub>e", font = t1),
              showgrid = TRUE,
              zeroline = FALSE,
              showline = FALSE,
              showticklabels = TRUE,
              tickfont = t1)

margin <-  list(
  l = 80,
  r = 80,
  b = 50,
  t = 50,
  pad = 0
)

# data_sort-----
sec_tot_98 <- sec_tot %>% filter(Year > 1995)
latest_year_data <- sec_tot_98 %>% filter(Year %in% CurrentYear)
nineties_data <- sec_tot %>% filter(Year == 1990)

nat_tot_98 <- nat_tot %>% filter(Year > 1995)
latest_year_gross_data <- nat_tot_98 %>% filter(Year %in% CurrentYear)
nineties_gross_data <- nat_tot %>% filter(Year == 1990)

# build plots----
# National - NCC/industry gross emissions
fig <- plot_ly(nat_tot_98, x = ~Year)

# add lines to chart
fig <-
  fig %>%  add_trace(
    y = ~get(industry_names[1]),
    type = 'scatter',
    name = industry_names[1],
    mode = 'lines',
    line = list(color = sg_colour_palettes$focus[1], width = 4),
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_trace(
    y = ~ get(industry_names[8]),
    type = 'scatter',
    name = "Total emissions" ,
    mode = 'lines',
    line = list(color = sg_colour_palettes$focus[2], width = 4),
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )

# add axis settings 
fig <- fig %>% layout( xaxis = xaxis, yaxis = yaxis,
                       autosize = TRUE,
                       showlegend = FALSE,
                       margin = margin,
                       hovermode= "x unified"
)  


#add marker to latest year
fig <-
  fig %>%  add_markers(
    x = ~ CurrentYear ,
    y = ~ get(industry_names[1]),
    data = latest_year_gross_data,
    name = industry_names[1],
    marker = list(color = sg_colour_palettes$focus[1], size = 12),
    inherit = TRUE,
    hoverinfo = "none"
  )

fig <-
  fig %>%  add_markers(
    x = ~ CurrentYear ,
    y = ~ get(industry_names[8]),
    data = latest_year_gross_data,
    name = "Total emissions",
    marker = list(color = sg_colour_palettes$focus[2], size = 12),
    inherit = TRUE,
    hoverinfo = "none"
  )

# add marker to 1990
fig <-
  fig %>%  add_markers(
    x = ~ 1990 ,
    y = ~ get(industry_names[1]),
    data = nineties_gross_data,
    name = industry_names[1],
    marker = list(color = sg_colour_palettes$focus[1], size = 12),
    inherit = TRUE,
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )

fig <-
  fig %>%  add_markers(
    x = ~ 1990 ,
    y = ~ get(industry_names[8]),
    data = nineties_gross_data,
    name = "Total emissions",
    marker = list(color = sg_colour_palettes$focus[2], size = 12),
    inherit = TRUE,
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )

#add annotations to latest year marker
fig <- fig %>% add_annotations(
  x = 0.93,
  y = ~ get(industry_names[1]),
  data = latest_year_gross_data,
  text = industry_names[1],
  xref = 'paper',
  yref = 'y',
  xanchor = 'left',
  font = list(
    family = 'Arial',
    size = 16,
    color = sg_colour_palettes$focus[1]
  ),
  showarrow = FALSE
)

fig <- fig %>% add_annotations(
  x = 0.93,
  y = ~ get(industry_names[8]),
  data = latest_year_gross_data,
  text = "Total emissions",
  xref = 'paper',
  yref = 'y',
  xanchor = 'left',
  font = list(
    family = 'Arial',
    size = 16,
    color = sg_colour_palettes$focus[2]
  ),
  showarrow = FALSE
)
fig1 <- fig
fig1

#National - current year industry/NCC gross emissions

fig <-plot_ly(nat_tot_current, x = ~Industry,
              y =~ ghg_emiss,
    type = 'bar',
    marker = list(color = sg_colour_values),
    hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e'<extra></extra>'",
    hovertext = "none"
  )


fig <- fig %>% layout(yaxis = list(title = list(text = 'MtCO<sub>2</sub>e', font = t1),
                                   tickfont = t1),
                      xaxis = list(title = list(font = t1),
                                   tickfont = t1),
                      barmode = 'stack',
                      legend = list(font = t1,
                                    text = source_names),
                      hoverlabel =list(font = list(color  = "white")))
fig1b <- fig
fig1b




# National - ghg by subsector ----
fig <- plot_ly(sec_tot_98, x = ~Year)


# add lines to chart
fig <-
  fig %>%  add_trace(
    y = ~ get(sectors[1]),
    type = 'scatter',
    name = sectors[1],
    mode = 'lines',
    line = list(color = sg_colour_values[1], width = 4),
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_trace(
    y = ~ get(sectors[2]),
    type = 'scatter',
    name = sectors[2],
    mode = 'lines',
    line = list(color = sg_colour_values[2], width = 4),
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_trace(
    y = ~ get(sectors[3]),
    type = 'scatter',
    name = sectors[3],
    mode = 'lines',
    line = list(color = sg_colour_values[3], width = 4),
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_trace(
    y = ~ get(sectors[4]),
    type = 'scatter',
    name = sectors[4],
    mode = 'lines',
    line = list(color = sg_colour_values[4], width = 4),
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_trace(
    y = ~ get(sectors[5]),
    type = 'scatter',
    name = sectors[5],
    mode = 'lines',
    line = list(color = sg_colour_values[5], width = 4),
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_trace(
    y = ~ get(sectors[6]),
    type = 'scatter',
    name = sectors[6],
    mode = 'lines',
    line = list(color = sg_colour_values[6], width = 4),
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )

# add axis settings 
fig <- fig %>% layout( xaxis = xaxis, yaxis = yaxis,
                      autosize = TRUE,
                      showlegend = FALSE,
                      margin = margin,
                      hovermode= "x unified"
                      )  


#add marker to latest year
fig <-
  fig %>%  add_markers(
    x = ~ CurrentYear ,
    y = ~ get(sectors[1]),
    data = latest_year_data,
    name = sectors[1],
    marker = list(color = sg_colour_values[1], size = 12),
    inherit = TRUE,
    hoverinfo = "none"
  )
fig <-
  fig %>%  add_markers(
    x = ~ CurrentYear ,
    y = ~ get(sectors[2]),
    data = latest_year_data,
    name = sectors[2],
    marker = list(color = sg_colour_values[2], size = 12),
    inherit = TRUE,
    hoverinfo = "none"
  )
fig <-
  fig %>%  add_markers(
    x = ~ CurrentYear ,
    y = ~ get(sectors[3]),
    data = latest_year_data,
    name = sectors[3],
    marker = list(color = sg_colour_values[3], size = 12),
    inherit = TRUE,
    hoverinfo = "none"
  )
fig <-
  fig %>%  add_markers(
    x = ~ CurrentYear ,
    y = ~ get(sectors[4]),
    data = latest_year_data,
    name = sectors[4],
    marker = list(color = sg_colour_values[4], size = 12),
    inherit = TRUE,
    hoverinfo = "none"
  )
fig <-
  fig %>%  add_markers(
    x = ~ CurrentYear ,
    y = ~ get(sectors[5]),
    data = latest_year_data,
    name = sectors[5],
    marker = list(color = sg_colour_values[5], size = 12),
    inherit = TRUE,
    hoverinfo = "none"
  )
fig <-
  fig %>%  add_markers(
    x = ~ CurrentYear ,
    y = ~ get(sectors[6]),
    data = latest_year_data,
    name = sectors[6],
    marker = list(color = sg_colour_values[6], size = 12),
    inherit = TRUE,
    hoverinfo = "none"
  )


#add marker to 1990
fig <-
  fig %>%  add_markers(
    x = ~ 1990 ,
    y = ~ get(sectors[1]),
    data = nineties_data,
    name = sectors[1],
    marker = list(color = sg_colour_values[1], size = 12),
    inherit = TRUE,
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_markers(
    x = ~ 1990 ,
    y = ~ get(sectors[2]),
    data = nineties_data,
    name = sectors[2],
    marker = list(color = sg_colour_values[2], size = 12),
    inherit = TRUE,
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_markers(
    x = ~ 1990 ,
    y = ~ get(sectors[3]),
    data = nineties_data,
    name = sectors[3],
    marker = list(color = sg_colour_values[3], size = 12),
    inherit = TRUE,
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_markers(
    x = ~ 1990 ,
    y = ~ get(sectors[4]),
    data = nineties_data,
    name = sectors[4],
    marker = list(color = sg_colour_values[4], size = 12),
    inherit = TRUE,
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_markers(
    x = ~ 1990 ,
    y = ~ get(sectors[5]),
    data = nineties_data,
    name = sectors[5],
    marker = list(color = sg_colour_values[5], size = 12),
    inherit = TRUE,
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig <-
  fig %>%  add_markers(
    x = ~ 1990 ,
    y = ~ get(sectors[6]),
    data = nineties_data,
    name = sectors[6],
    marker = list(color = sg_colour_values[6], size = 12),
    inherit = TRUE,
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
   
  )


#add annotations to latest year marker
fig <- fig %>% add_annotations(
  x = 0.93,
  y = ~ get(sectors[1]),
  data = latest_year_data,
  text = sectors[1],
  xref = 'paper',
  yref = 'y',
  xanchor = 'left',
  font = list(
    family = 'Arial',
    size = 16,
    color = sg_colour_values[1]
  ),
  showarrow = FALSE
)


fig <- fig %>% add_annotations(
  x = 0.93,
  y = ~ get(sectors[2]),
  data = latest_year_data,
  text = sectors[2],
  xref = 'paper',
  yref = 'y',
  xanchor = 'left',
  font = list(
    family = 'Arial',
    size = 16,
    color = sg_colour_values[2]
  ),
  showarrow = FALSE
)

fig <- fig %>% add_annotations(
  x = 0.93,
  y = ~ get(sectors[3]),
  data = latest_year_data,
  text = sectors[3],
  xref = 'paper',
  yref = 'y',
  xanchor = 'left',
  font = list(
    family = 'Arial',
    size = 16,
    color = sg_colour_values[3]
  ),
  showarrow = FALSE
)

#to do: try to add data label for latest year
fig <- fig %>% add_annotations(
  x = 0.93,
  y = ~ get(sectors[6]),
  data = latest_year_data,
  text = sectors[6],
  # text = paste(
  #   sectors[6],
  #   ":",
  #   CurrentYear,
  #   round(latest_year_data[sectors[6]], 2),
  #   "MtCO<sub>2</sub>"
  #),
  xref = 'paper',
  yref = 'y',
  xanchor = 'left',
  font = list(
    family = 'Arial',
    size = 16,
    color = sg_colour_values[6]
  ),
  showarrow = FALSE
)

fig <-
  fig %>% add_annotations(
    x = 0.93,
    y = ~ get(sectors[4]) + 0.1,
    data = latest_year_data,
    text = sectors[4],
    xref = 'paper',
    yref = 'y',
    xanchor = 'left',
    font = list(
      family = 'Arial',
      size = 16,
      color = sg_colour_values[4]
    ),
    showarrow = FALSE
  )

fig <- fig %>% add_annotations(
  x = 0.93,
  y = ~ get(sectors[5]),
  data = latest_year_data,
  #xshift = 3,
  text = sectors[5],
  xref = 'paper',
  yref = 'y',
  xanchor = 'left',
  font = list(
    family = 'Arial',
    size = 16,
    color = sg_colour_values[5]
  ),
  showarrow = FALSE
)

fig2 <-  fig

fig2

# emissions by pollutant ----

fig <-plot_ly(sec_comp_latest, x = ~Sector)

fig <-
  fig %>%  add_trace(
    y = ~Methane,
    type = 'bar',
    name = 'Methane',
    marker = list(color = "#002d54"),
    hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e of methane'<extra></extra>'",
    hovertext = "none"
  )

fig <- fig %>% add_trace(y = ~`Carbon Dioxide`, type = 'bar', 
                         name = 'Carbon Dioxide',
                         marker = list(color = "#2b9c93"),
                         hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e of carbon dioxide'<extra></extra>'",
                         text = "")

fig <- fig %>% add_trace(y = ~`Nitrous Oxide`, type = 'bar', 
                         name = 'Nitrous Oxide', marker = list(color = "#6a2063"),
                         hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e of nitrous oxide'<extra></extra>'",
                         text = "")




fig <- fig %>% layout(yaxis = list(title = list(text = 'MtCO<sub>2</sub>e', font = t1),
                                   tickfont = t1),
                      xaxis = list(title = list(font = t1),
                                   tickfont = t1),
                      barmode = 'stack',
                      legend = list(font = t1))
fig3 <- fig

# emissions by source ----

fig <-plot_ly(sec_source, x = ~Sector)

fig <-
  fig %>%  add_trace(
    y = ~get(source_names[1]),
    type = 'bar',
    name = source_names[1],
    marker = list(color = sg_colour_values[1]),
    hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e from enteric fermentation'<extra></extra>'",
    hovertext = "none"
  )

fig <-
  fig %>%  add_trace(
    y = ~get(source_names[2]),
    type = 'bar',
    name = source_names[2],
    marker = list(color = sg_colour_values[2]),
    hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e from manure management'<extra></extra>'",
    hovertext = "none"
  )

fig <-
  fig %>%  add_trace(
    y = ~get(source_names[3]),
    type = 'bar',
    name = source_names[3],
    marker = list(color = sg_colour_values[3]),
    hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e from agricultural soils'<extra></extra>'",
    hovertext = "none"
  )

fig <-
  fig %>%  add_trace(
    y = ~get(source_names[4]),
    type = 'bar',
    name = source_names[4],
    marker = list(color = sg_colour_values[4]),
    hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e from fuel combustion'<extra></extra>'",
    hoverlabel = list(color = "white")
  )

fig <-
  fig %>%  add_trace(
    y = ~get(source_names[5]),
    type = 'bar',
    name = source_names[5],
    marker = list(color = sg_colour_values[5]),
    hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e from liming'<extra></extra>'",
    hovertext = "none"
  )

fig <-
  fig %>%  add_trace(
    y = ~get(source_names[6]),
    type = 'bar',
    name = source_names[6],
    marker = list(color = sg_colour_values[6]),
    hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e from urea application'<extra></extra>'",
    hovertext = "none"
  )

fig <-
  fig %>%  add_trace(
    y = ~get(source_names[7]),
    type = 'bar',
    name ="Non-energy products <br>from fuels and solvent use</br>",
    marker = list(color = sg_colour_values[7]),
    hovertemplate = "%{x}: %{y:.2f} MtCO<sub>2</sub>e from non-energy products <br>from fuels and solvent use</br>'<extra></extra>'",
    hovertext = "none"
  )
fig


fig <- fig %>% layout(yaxis = list(title = list(text = 'MtCO<sub>2</sub>e', font = t1),
                                   tickfont = t1),
                      xaxis = list(title = list(font = t1),
                                   tickfont = t1),
                      barmode = 'stack',
                      legend = list(font = t1,
                                    text = source_names),
                      hoverlabel =list(font = list(color  = "white")))
fig4 <- fig
fig4


