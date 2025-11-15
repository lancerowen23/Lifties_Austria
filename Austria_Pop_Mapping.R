##Mapping Elderly Populations

library(readr)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)

#Bezirke
bez_url <- "https://raw.githubusercontent.com/lancerowen23/Lifties_Austria/main/austria_bezirke_simplified.geojson"  
bezirke <- st_read(bez_url, quiet = FALSE)

bez_csv <- read_csv('Desktop/Lifties_Austria/pop_75andUp_bez.csv')
View(bez_csv)

#join data
bez_merge <- merge(bezirke, bez_csv, by.x="g_id", by.y="id")

#map

value_column <- "percent"

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = bez_merge[[value_column]],
  na.color = "#cccccc"
)

# 4. Build the interactive leaflet map // Bezirke
leaflet(bez_merge) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(get(value_column)),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    
    # highlight on hover
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    
    # popup information
    popup = ~paste0(
      "<strong>", name, "</strong><br/>",
      get(value_column), "% age 75 and older."
    ),
    
    # label on hover
    label = ~paste0(percent, ": ", get(value_column)),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = bez_merge[[value_column]],
    opacity = 0.7,
    title = value_column,
    position = "bottomright"
  )


#Gemeinden
gem_url <- "https://raw.githubusercontent.com/lancerowen23/Lifties_Austria/main/austria_gemeinden_simplified.geojson"  
gemeinden <- st_read(gem_url, quiet = FALSE)

gem_csv <- read_csv('Desktop/Lifties_Austria/pop_75andUp_gem.csv',
                    locale = locale(encoding = "UTF-8"))

gem_csv <- readr::read_csv('Desktop/Lifties_Austria/pop_75andUp_gem.csv', locale = locale(encoding = "UTF-8"))
View(gem_csv)                   

#join data
gem_merge <- merge(gemeinden, gem_csv, by.x="g_id", by.y="id")

#create map
# 1. Load your merged GeoJSON

# 2. Choose a numeric column to visualize
# Replace "my_value" with the column from your CSV you want to map
value_column <- "percent"

# 3. Create a color palette for choropleth
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = gem_merge[[value_column]],
  na.color = "#cccccc"
)

# 4. Build the interactive leaflet map // Gemeinden
leaflet(gem_merge) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(get(value_column)),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    
    # highlight on hover
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    
    # popup information
    popup = ~paste0(
      "<strong>", name, "</strong><br/>",
      get(value_column), "% age 75 and older."
    ),
    
    # label on hover
    label = ~paste0(percent, ": ", get(value_column)),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = gem_merge[[value_column]],
    opacity = 0.7,
    title = value_column,
    position = "bottomright"
  )

