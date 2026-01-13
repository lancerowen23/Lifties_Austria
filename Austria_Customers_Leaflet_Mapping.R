#Mapping Customer Aggregates with Leaflet

#Author: Lance R. Owen

library(readr)
library(sf)
library(dplyr)
library(leaflet)
library(htmltools)
library(RColorBrewer)

#GeoJSON Import 

#Bezirke
bez_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/bezirke_final.geojson"  
bez_customers <- st_read(bez_path, quiet = FALSE)
bez_customers <- bez_customers %>%
  mutate(address_count = if_else(is.na(address_count), 0L, address_count))

#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/gemeinden_final.geojson"   
gem_customers <- st_read(gem_path, quiet = FALSE)
gem_customers <- gem_customers %>%
  mutate(address_count = if_else(is.na(address_count), 0L, address_count))


#MAPPING

#BEZIRKE
#SET COLOR PALATTE  
bez_pal <- colorBin(
  palette = "YlOrRd",
  domain  = bez_customers$address_count,
  bins    = 15,
  pretty  = TRUE
)

leaflet(bez_customers) %>%
  addTiles() %>%
  addPolygons(
    fillColor   = ~bez_pal(address_count),
    fillOpacity = 0.75,
    color       = "#444444",
    weight      = 0.5,
    smoothFactor = 0.2,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#000000",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0(
      "<strong>", g_name, "</strong><br/>",
      "Addresses: ", address_count
    ) %>% lapply(htmltools::HTML)
  ) %>%
  addLegend(
    pal     = pal,
    values = ~address_count,
    opacity = 0.7,
    title   = "Address Count",
    position = "bottomright"
  )

#GEMEINDEN
#SET COLOR PALATTE 
gem_pal <- colorBin(
  palette = "YlOrRd",
  domain  = gem_customers$address_count,
  bins    = 20,
  pretty  = TRUE
)

leaflet(gem_customers) %>%
  addTiles() %>%
  addPolygons(
    fillColor   = ~gem_pal(address_count),
    fillOpacity = 0.75,
    color       = "#444444",
    weight      = 0.5,
    smoothFactor = 0.2,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#000000",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0(
      "<strong>", g_name, "</strong><br/>",
      "Addresses: ", address_count
    ) %>% lapply(htmltools::HTML)
  ) %>%
  addLegend(
    pal     = gem_pal,
    values = ~address_count,
    opacity = 0.7,
    title   = "Address Count",
    position = "bottomright"
  )
