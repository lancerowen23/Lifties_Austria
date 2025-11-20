#Mapping with Leaflet

#Author: Lance R. Owen

library(readr)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)

#GeoJSON Import 

#Population
#Bezirke
bez_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/bez_finalpop_data.geojson"  
bez_pop <- st_read(bez_path, quiet = FALSE)
#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/gem_finalpop_data.geojson"   
gem_pop <- st_read(gem_path, quiet = FALSE)

#Housing
#Bezirke
bez_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/bez_finalhousing_data.geojson"  
bez_housing <- st_read(bez_path, quiet = FALSE)
#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/gem_finalhousing_data.geojson"   
gem_housing <- st_read(gem_path, quiet = FALSE)


#Create Map
map <- "gem_housing"
value_column <- "abs_2floor"

#Define color palette
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = gem_housing[[value_column]],
  na.color = "#cccccc")

#Build the interactive leaflet map
leaflet(gem_housing) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(get(value_column)),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    
    # highlight on hover
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    
    # popup information
    popup = ~paste0(
      "<strong>", name, "</strong><br/>",
      get(value_column), " buildings with 2 floors above ground."
    )) %>%
  addLegend(
    pal = pal,
    values = gem_housing[[value_column]],
    opacity = 0.8,
    title = "Pop. 75+",
    position = "bottomright"
  )
