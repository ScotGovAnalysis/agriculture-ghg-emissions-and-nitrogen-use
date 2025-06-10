library(highcharter)
library(dplyr)
library(tidyr)

# Load data
load("ghg_data.RData")

# Fig 1 ----
# Filter the data for 'Total' and 'Agriculture'
filtered_data <- national_total %>%
  filter(Industry %in% c("Total", "Agriculture"))

# Rename 'Total' to 'Total emissions'
filtered_data$Industry <- ifelse(filtered_data$Industry == "Total", "Total emissions", filtered_data$Industry)

# Extend the years sequence to include 2034
years <- seq(min(filtered_data$Year), 2030)
industries <- unique(filtered_data$Industry)
complete_data <- expand.grid(Year = years, Industry = industries)

# Merge with filtered data to ensure all years are represented, filling missing values with NA
complete_data <- complete_data %>%
  left_join(filtered_data, by = c("Year", "Industry"))

# Define the specific colors for 'Total emissions' and 'Agriculture'
color_map <- c("Total emissions" = "#949494", "Agriculture" = "#002d54")

# Create the Highcharter line chart with zoom option
hc <- highchart() %>%
  hc_chart(type = "line", zoomType = "xy", style = list(fontFamily = "Arial", fontSize = "16px")) %>%
  hc_yAxis(title = list(text = "Emissions (MtCO₂e)", style = list(color = "#000000", fontSize = "16px", fontFamily = "Arial")),
           labels = list(style = list(color = "#000000", fontSize = "16px", fontFamily = "Arial"))) %>%
  hc_xAxis(
    labels = list(
      style = list(color = "#000000", fontSize = "16px", fontFamily = "Arial"),
      formatter = JS("function() {
        if (this.value === 2030) {
          return '';
        } else {
          return this.value;
        }
      }")
    ),
    title = list(text = "Year", style = list(color = "#000000", fontSize = "16px", fontFamily = "Arial")),
    categories = years,
    tickInterval = 5  # Set tick interval to 5 years
  ) %>%
  hc_legend(enabled = FALSE) %>%
  hc_tooltip(
    headerFormat = "<span style='font-size: 16px; font-family: Arial'>{point.key}</span><br/>",
    pointFormat = "<span style='font-size: 16px;; font-family: Arial'>{series.name}: <b>{point.y:.1f} MtCO₂e</b></span>", 
    style = list(fontSize = "16px", fontFamily = "Arial")
  ) %>%
  hc_plotOptions(line = list(connectNulls = FALSE, colorByPoint = FALSE))

# Prepare data for each industry
hc_data <- complete_data %>%
  group_by(Industry) %>%
  group_split()

# Add series data to the chart
for (industry_data in hc_data) {
  industry <- industry_data$Industry[1]
  data_points <- industry_data %>%
    select(x = Year, y = Value)
  
  # Create custom markers
  data_points_list <- lapply(1:nrow(data_points), function(i) {
    point <- data_points[i, ]
    marker <- if (point$x %in% c(1990, 1995, 2022)) {
      list(radius = 5, fillColor = color_map[[industry]], symbol = "circle")
    } else {
      list(enabled = FALSE)  # Disable markers for other points
    }
    list(x = point$x, y = point$y, marker = marker)
  })
  
  # Add the label for 2022 point
  last_point_2022 <- data_points_list[[which(data_points$x == 2022)]]
  last_point_2022$dataLabels <- list(
    enabled = TRUE,
    align = 'left',
    x = 1,  # Adjust this value to move the label more to the right
    y = -1,  # Adjust this value to move the label up or down
    format = industry,
    style = list(fontSize = '16px', fontFamily = 'Arial', color = color_map[[industry]])  # Adjust fontSize to make the label bigger
  )
  
  data_points_list[[which(data_points$x == 2022)]] <- last_point_2022
  
  hc <- hc %>%
    hc_add_series(
      name = industry,
      data = data_points_list,
      color = color_map[[industry]],
      marker = list(enabled = TRUE, symbol = "circle")
    )
}
 fig1 <- hc

# Fig 2 ----

# Filter data for the year 2022 and exclude 'Total'
data_2022 <- subsector_total %>%
  filter(Year == 2022 & Subsector != 'Total') %>%
  arrange(desc(Value)) %>%
  mutate(Value = as.numeric(Value))

# Preset list of colors
preset_colors <- c("#002d54", "#2b9c93", "#6a2063", "#e5682a",
                   "#0b4c0b", "#5d9f3c", "#592c20", "#ca72a2")

# Assign colors to subsectors
color_map <- setNames(preset_colors[1:nrow(data_2022)], data_2022$Subsector)

# Generate data list for highcharter
data_list <- lapply(1:nrow(data_2022), function(i) {
  list(
    name = data_2022$Subsector[i],
    y = data_2022$Value[i],
    color = color_map[[data_2022$Subsector[i]]]
  )
})

# Create the Highcharter bar chart with zoom option
fig2 <- highchart() %>%
  hc_chart(type = "bar", zoomType = "xy") %>%
  hc_xAxis(categories = data_2022$Subsector, 
           labels = list(style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial'))) %>%
  hc_yAxis(title = list(text = "Emissions (MtCO₂e)", style = list(color = "#000000",fontSize = '16px', fontFamily = 'Arial')),
           labels = list(style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial')),
           tickInterval = 0.5) %>%
  hc_plotOptions(series = list(
    groupPadding = 0,
    pointPadding = 0.1,
    borderWidth = 0
  )) %>%
  hc_tooltip(
    headerFormat = "<span style='font-size: 16px; font-family: Arial'>{point.key}</span><br/>",
    pointFormat = "<span style='font-size: 16px; font-family: Arial'><b>{point.y:.2f} MtCO₂e</b></span>", 
    style = list(fontSize = "16px", fontFamily = "Arial")
  ) %>%
  hc_legend(enabled = FALSE) %>%  # Hide the legend
  hc_series(list(
    name = as.character(2022),
    data = data_list
  ))



#fig 3 ----
# Reshape the data from wide to long format
data_long <- subsector_source %>%
  pivot_longer(cols = -Source, names_to = "Subsector", values_to = "Value") 

data_long <- data_long %>%  filter(Source != "Urea application") %>% 
  filter(Source != "Non-energy products from fuels and solvent use") 

# Invert
data_long <- data_long %>%
  arrange(match(Source, rev(unique(Source))))

# Invert the preset list of colors
preset_colors <- rev(c("#002d54", "#2b9c93", "#6a2063", "#e5682a",
                       "#0b4c0b", "#5d9f3c"))

# Assign colors to sources
unique_sources <- unique(data_long$Source)
color_map <- setNames(preset_colors[1:length(unique_sources)], unique_sources)

# Generate data list for highcharter
series_data <- lapply(unique_sources, function(source) {
  list(
    name = source,
    data = data_long %>%
      filter(Source == source) %>%
      arrange(match(Subsector, unique(data_long$Subsector))) %>%
      pull(Value),
    color = color_map[[source]]
  )
})

# Create the Highcharter bar chart with zoom option
fig3 <- highchart() %>%
  hc_chart(type = "bar", zoomType = "xy") %>%
  hc_xAxis(categories = unique(data_long$Subsector), 
           labels = list(style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial'))) %>%
  hc_yAxis(title = list(text = "Emissions (MtCO₂e)", style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial')),
           labels = list(style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial')),
           tickInterval = 0.5) %>%
  hc_plotOptions(bar = list(
    stacking = "normal",
    groupPadding = 0,  # Adjust group padding
    pointPadding = 0.1,   # Adjust point padding
    borderWidth = 0    # Remove border between stacked bars
  )) %>%
  hc_tooltip(
    headerFormat = "<span style='font-size: 16px; font-family: Arial'>{point.key}</span><br/>",
    pointFormat = "<span style='font-size: 16px; font-family: Arial'>{series.name}: <b>{point.y:.2f} MtCO₂e</b>", 
    style = list(fontSize = "16px", fontFamily = "Arial")) %>%
  hc_legend(
    align = "right", 
    verticalAlign = "middle", 
    layout = "vertical",
    itemStyle = list(fontSize = '16px', fontFamily = 'Arial', fontWeight = 'normal')
  ) %>%
  hc_add_series_list(series_data)

fig3 

