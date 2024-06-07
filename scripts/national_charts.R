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


# build plots----

# National - CCP industry gross emissions - fig 1 ----
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
    y = ~get(industry_names[8]),
    type = 'scatter',
    name = "Total emissions" ,
    mode = 'lines',
    line = list(color = sg_colour_palettes$focus[2], width = 4),
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )
fig
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
    y = ~get(industry_names[1]),
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

# add marker to 1995
fig <-
  fig %>%  add_markers(
    x = ~ 1995 ,
    y = ~get(industry_names[1]),
    data = nat_tot_95,
    name = industry_names[1],
    marker = list(color = sg_colour_palettes$focus[1], size = 12),
    inherit = TRUE,
    hovertemplate = "%{y:.2f} MtCO<sub>2</sub>e"
  )

fig <-
  fig %>%  add_markers(
    x = ~ 1995 ,
    y = ~ get(industry_names[8]),
    data = nat_tot_95,
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
  text = "Agriculture",
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



# National - agri subsector - emissions fig 2 ----

fig <-plot_ly(sec_tot, x = ~Year,
                            y =~Subsector,
                  type = 'bar',
                  orientation = 'h',
                  # may need to change depending on where agriculture comes in order
                  marker = list(color = sg_colour_values[1:6]
                  ),
                  hovertemplate = "%{y}: %{x:.2f} MtCO<sub>2</sub>e<extra></extra>"  )


fig <- fig %>% layout(yaxis = list(title = list(font = t1),
                                   tickfont = t1),
                      xaxis = list(title = list(text = 'MtCO<sub>2</sub>e', font = t1),
                                   tickfont = t1),
                      barmode = 'stack',
                      legend = list(font = t1,
                                    text = source_names),
                      hoverlabel =list(font = list(color  = "white",
                                                   font = t1))
                      )

fig2 <- fig
fig2

# National - agri sub sector - emissions by source fig 3----

fig <-plot_ly(sec_source, 
              x =~get(source_names[1]) , 
              y = ~Sector,
    type = 'bar',
    orientation = 'h',
    name = source_names[1],
    marker = list(color = sg_colour_values[1]),
    hovertemplate = "%{y}: %{x:.2f} MtCO<sub>2</sub>e from enteric fermentation'<extra></extra>'",
    hovertext = "none"
  )

fig <-
  fig %>%  add_trace(
    x = ~get(source_names[2]),
    y =  ~Sector,
    type = 'bar',
    orientation = 'h',
    name = source_names[2],
    marker = list(color = sg_colour_values[2]),
    hovertemplate = "%{y}: %{x:.2f} MtCO<sub>2</sub>e from manure management'<extra></extra>'",
    hovertext = "none"
  )

fig <-
  fig %>%  add_trace(
    x = ~get(source_names[3]),
    y = ~Sector,
    type = 'bar',  
    orientation = 'h',
    name = source_names[3],
    marker = list(color = sg_colour_values[3]),
    hovertemplate = "%{y}: %{x:.2f} MtCO<sub>2</sub>e from agricultural soils'<extra></extra>'",
    hovertext = "none"
  )

fig <-
  fig %>%  add_trace(
    x = ~get(source_names[4]),
    y = ~Sector,
    type = 'bar',
    orientation = 'h',
    name = source_names[4],
    marker = list(color = sg_colour_values[4]),
    hovertemplate = "%{y}: %{x:.2f} MtCO<sub>2</sub>e from fuel combustion'<extra></extra>'",
    hoverlabel = list(color = "white")
  )

fig <-
  fig %>%  add_trace(
    x = ~get(source_names[5]),
    y = ~Sector,
    type = 'bar',
    orientation = 'h',
    name = source_names[5],
    marker = list(color = sg_colour_values[5]),
    hovertemplate = "%{y}: %{x:.2f} MtCO<sub>2</sub>e from liming'<extra></extra>'",
    hovertext = "none"
  )

fig <-
  fig %>%  add_trace(
    x = ~get(source_names[8]),
    y = ~Sector,
    type = 'bar',
    orientation = 'h',
    name = source_names[8],
    marker = list(color = sg_colour_values[6]),
    hovertemplate = "%{y}: %{x:.2f} MtCO<sub>2</sub>e from other emission sources'<extra></extra>'",
    hovertext = "none"
  )

fig


fig <- fig %>% layout(xaxis = list(title = list(text = 'MtCO<sub>2</sub>e', font = t1),
                                   tickfont = t1),
                      yaxis = list(title = list(font = t1),
                                   tickfont = t1),
                      barmode = 'stack',
                      legend = list(font = t1,
                                    text = source_names),
                      hoverlabel =list(font = list(color  = "white")))
fig3 <- fig
fig3


