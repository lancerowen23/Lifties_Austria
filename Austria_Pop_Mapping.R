##Mapping Elderly Populations // Austria 
#All data 2024
#Spatial and Pop Data from Statistik Austria

library(readr)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)

#GeoJSON Import 

#Bezirke
bez_url <- "https://raw.githubusercontent.com/lancerowen23/Lifties_Austria/main/austria_bezirke_simplified.geojson"  
bezirke <- st_read(bez_url, quiet = FALSE)
#Gemeinden
gem_url <- "https://raw.githubusercontent.com/lancerowen23/Lifties_Austria/main/austria_gemeinden_simplified.geojson"  
gemeinden <- st_read(gem_url, quiet = FALSE)

#Pop Files Import

#total population
bez_pop_total <- read_csv('Desktop/Lifties_Austria/pop_total_bez.csv')
gem_pop_total <- read.csv('Desktop/Lifties_Austria/pop_total_gem.csv', encoding = "UTF-8")
#population 60-74 years
bez_pop_60to74 <- read_csv('Desktop/Lifties_Austria/pop_60to74_bez.csv')
gem_pop_60to74 <- read.csv('Desktop/Lifties_Austria/pop_60to74_gem.csv', encoding = "UTF-8")
#population 75+ years
bez_pop_75andUp <- read_csv('Desktop/Lifties_Austria/pop_75andUp_bez.csv')
gem_pop_75andUp <- read.csv('Desktop/Lifties_Austria/pop_75andUp_gem.csv', encoding = "UTF-8")

#join data
#bezirke
bez_df <- merge(bezirke, bez_pop_total, by.x="g_id", by.y="id")
bez_df <- merge(bez_df, bez_pop_60to74, by.x="g_id", by.y="id")
bez_df <- merge(bez_df, bez_pop_75andUp, by.x="g_id", by.y="id")
bez_final_df <- bez_df %>% 
  select(g_id, name, total_pop, abs.x, percent.x, abs.y, percent.y, geometry) %>% 
  rename(id = g_id,
         pop_60to74 = abs.x,
         percent_60to74 = percent.x,
         pop_75andUp = abs.y,
         percent_75andUp = percent.y)
View(bez_final_df)

#gemeinden
gem_df <- merge(gemeinden, gem_pop_total, by.x="g_id", by.y="id")
gem_df <- merge(gem_df, gem_pop_60to74, by.x="g_id", by.y="id")
gem_df <- merge(gem_df, gem_pop_75andUp, by.x="g_id", by.y="id")
gem_final_df <- gem_df %>% 
  select(g_id, name, total_pop, abs.x, percent.x, abs.y, percent.y, geometry) %>% 
  rename(id = g_id,
         pop_60to74 = abs.x,
         percent_60to74 = percent.x,
         pop_75andUp = abs.y,
         percent_75andUp = percent.y)
View(gem_final_df)

#mapping with Leaflet

#bezirke 

value_column <- "percent_75andUp"

#Define color palette
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = bez_final_df[[value_column]],
  na.color = "#cccccc")

#Build the interactive leaflet map
leaflet(bez_final_df) %>%
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
      get(value_column), "% age 75 and older."
    )) %>%
  addLegend(
    pal = pal,
    values = bez_final_df[[value_column]],
    opacity = 0.8,
    title = "Pop. 75+",
    position = "bottomright"
  )

#Gemeinden

value_column <- "percent_75andUp"

#Define color palette
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = gem_final_df[[value_column]],
  na.color = "#cccccc")

#Build the interactive leaflet map
leaflet(gem_final_df) %>%
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
      get(value_column), "% age 75 and older."
    )) %>%
  addLegend(
    pal = pal,
    values = gem_final_df[[value_column]],
    opacity = 0.8,
    title = "Pop. 75+",
    position = "bottomright"
  )
