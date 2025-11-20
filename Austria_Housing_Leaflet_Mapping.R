#Mapping Housing with Leaflet

#Author: Lance R. Owen

library(readr)
library(sf)
library(dplyr)
library(leaflet)
library(htmltools)
library(RColorBrewer)

#GeoJSON Import 

#Housing
#Bezirke
bez_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/bez_finalhousing_data.geojson"  
bez_housing <- st_read(bez_path, quiet = FALSE)
#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/gem_finalhousing_data.geojson"   
gem_housing <- st_read(gem_path, quiet = FALSE)

# Choose dataset
#active_data <- bez_housing
active_data <- gem_housing

# Define fields and their types
fields <- list(
  list(col = "percent_1floor",        title = "Percent 1 Floor",        type = "percent"),
  list(col = "abs_1floor",            title = "Count 1 Floor",       type = "count"),
  list(col = "percent_2floor",        title = "Percent 2 Floor",        type = "percent"),
  list(col = "abs_2floor",            title = "Count 2 Floor",       type = "count"),
  list(col = "percent_3to5floor",     title = "Percent 3–5 Floors",     type = "percent"),
  list(col = "abs_3to5floor",         title = "Count 3–5 Floors",    type = "count"),
  list(col = "percent_own_occ",       title = "Percent Owner Occupied", type = "percent"),
  list(col = "abs_own_occ",           title = "Count Owner Occupied",type = "count"),
  list(col = "percent_priv_one",      title = "Percent Private One Occupant",    type = "percent"),
  list(col = "abs_priv_one",          title = "Count Private One Occupant",   type = "count"),
  list(col = "percent_res_one_dwell", title = "Percent One Dwelling",   type = "percent"),
  list(col = "abs_res_one_dwell",     title = "Count One Dwelling",  type = "count")
)

# Counts vs. Percents
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
  if (type == "percent") {
    # Fixed bins for percentages for clarity
    c(0, 10, 20, 30, 40, 50, 100)
  } else {
    rng <- range(vec, na.rm = TRUE)
    seq(rng[1], rng[2], length.out = 6)  # 5 classes
  }
}

# Mapping Loop
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
        format_value(get(value_column), field_type), ""
      )
    ) %>%
    addLegend(
      pal = pal,
      values = active_data[[value_column]],
      opacity = 0.8,
      title = paste0(legend_title, ""),
      position = "bottomright"
    )
  
  # Assign map to variable dynamically
  assign(paste0("map_", value_column), map)
}

# Print maps to Viewer
map_percent_1floor
map_abs_1floor
map_percent_2floor
map_abs_2floor
map_percent_3to5floor
map_abs_3to5floor
map_percent_own_occ
map_abs_own_occ
map_percent_priv_one
map_abs_priv_one
map_percent_res_one_dwell
map_abs_res_one_dwell
