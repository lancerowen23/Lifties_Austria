#Mapping Population with Leaflet

#Author: Lance R. Owen

library(readr)
library(sf)
library(dplyr)
library(leaflet)
library(htmltools)
library(RColorBrewer)

#GeoJSON Import 
#Population
#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Population/gem_finalpop_data.geojson"   
gem_pop <- st_read(gem_path, quiet = FALSE)

active_data <- gem_pop   


# Define fields and display properties
fields <- list(
  list(col = "total_pop",       title = "Total Population",  type = "count"),
  list(col = "pop_60to74",      title = "Population 60–74",  type = "count"),
  list(col = "pop_75andUp",     title = "Population 75+",    type = "count"),
  list(col = "percent_60to74",  title = "Percent 60–74",     type = "percent"),
  list(col = "percent_75andUp", title = "Percent 75+",       type = "percent")
)

#Counts (abs) vs. Percents
format_value <- function(x, type = c("count","percent")) {
  type <- match.arg(type)
  if (type == "percent") {
    return(ifelse(is.na(x), "NA", sprintf("%.1f%%", x)))
  } else {
    return(ifelse(is.na(x), "NA", format(round(x), big.mark = ",", trim = TRUE)))
  }
}

# Palette
palette_for_type <- function(type = c("count","percent")) {
  "YlOrRd"
}

# Equal-interval bins
bins_for <- function(vec, type = c("count","percent")) {
  type <- match.arg(type)
  rng <- range(vec, na.rm = TRUE)
  seq(rng[1], rng[2], length.out = 6)  # 5 classes
}

# Mapping Loop (one map per field)
for (f in fields) {
  value_column <- f$col
  legend_title <- f$title
  field_type   <- f$type
  
  if (!value_column %in% names(active_data)) {
    warning(sprintf("Column '%s' not found; skipping.", value_column))
    next
  }
  
  pal_name <- palette_for_type(field_type)
  bins     <- bins_for(active_data[[value_column]], field_type)
  
  pal <- colorBin(
    palette = pal_name,
    domain  = active_data[[value_column]],
    bins    = bins,
    na.color = "#cccccc"
  )
  
  map <- leaflet(active_data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      fillColor = ~pal(get(value_column)),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "1",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      popup = ~paste0(
        "<strong>", htmlEscape(name), "</strong><br/>",
        format_value(get(value_column), field_type)
      )
    ) %>%
    addLegend(
      pal = pal,
      values = active_data[[value_column]],
      opacity = 0.8,
      title = paste0(legend_title, ""),
      position = "bottomright"
    )
  
  assign(paste0("map_", value_column), map)
}

# View maps
map_total_pop
map_pop_60to74
map_percent_60to74
map_pop_75andUp
map_percent_75andUp