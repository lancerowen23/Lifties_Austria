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
bez_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/bez_finalhousing_data.geojson"  
bez_housing <- st_read(bez_path, quiet = FALSE)

#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Customers/gemeinde_counts_total.geojson"   
gem_customers <- st_read(gem_path, quiet = FALSE)

gem_customers <- gem_customers %>%
  mutate(total_sum = if_else(is.na(total_sum), 0L, total_sum))

pal <- colorBin(
  palette = "YlOrRd",
  domain  = gem_customers$total_sum,
  bins    = 10,
  pretty  = TRUE
)


leaflet(gem_customers) %>%
  addTiles() %>%
  addPolygons(
    fillColor   = ~pal(total_sum),
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
      "Addresses: ", total_sum
    ) %>% lapply(htmltools::HTML)
  ) %>%
  addLegend(
    pal     = pal,
    values = ~total_sum,
    opacity = 0.7,
    title   = "Address Count",
    position = "bottomright"
  )
