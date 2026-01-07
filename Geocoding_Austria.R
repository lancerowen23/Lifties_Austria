### Geocoding Addresses for Customer Analysis

## Author: Lance R. Owen 

# Geocoding addresses using HERE Geocoding service and aggregating to both 
# gemeinden and bezirke. 

library(dplyr)
library(stringr)
library(hereR)
library(sf)
library(leaflet)
library(ggplot2)
library(readr)

#data import
#2022
cust_22 <- read.csv('~/Downloads/P22_2.csv')
#2023
cust_23 <- read.csv('~/Downloads/P23_2.csv')
#2024
cust_24 <- read.csv('~/Downloads/P24_2.csv')

#Cleaning names for geocoding
library(stringr)

clean_street_at <- function(street) {
  
  street %>%
    str_trim() %>%
    
    # FIX: expand str. inside compound words
    str_replace_all(
      regex("(?<=\\p{L})str\\.(?=\\d|\\s|$)", ignore_case = TRUE),
      "strasse"
    ) %>%
    
    # Optional: normalize other abbreviations
    str_replace_all(regex("(?<=\\p{L})g\\.", ignore_case = TRUE), "gasse") %>%
    str_replace_all(regex("(?<=\\p{L})pl\\.", ignore_case = TRUE), "platz") %>%
    
    # German → ASCII
    str_replace_all("ß", "ss") %>%
    str_replace_all(c(
      "ä" = "ae", "ö" = "oe", "ü" = "ue",
      "Ä" = "Ae", "Ö" = "Oe", "Ü" = "Ue"
    )) %>%
    
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

#apply
cust_22 <- cust_22 %>%
  mutate(
    street_clean = clean_street_at(street)
  )

cust_23 <- cust_23 %>%
  mutate(
    street_clean = clean_street_at(street)
  )

cust_24 <- cust_24 %>%
  mutate(
    street_clean = clean_street_at(street)
  )

#create single string for geocoding as required by HERE geocoding service
cust_22 <- cust_22 %>%
  mutate(full_address = paste(street_clean, postal_code, city, "Austria"))
cust_23 <- cust_23 %>%
  mutate(full_address = paste(street_clean, postal_code, city, "Austria"))
cust_24 <- cust_24 %>%
  mutate(full_address = paste(street_clean, postal_code, city, "Austria"))

#combine into single df
cust_all <- bind_rows(cust_22, cust_23, cust_24)


#geocode using HERE

set_key("API KEY")

results <- geocode(cust_all$full_address)

# drop non matches
final_results <- results %>% filter(access != "POINT EMPTY")
write_csv(final_results, "Desktop/final_geocode_results.csv")

# map results
leaflet(data = final_results) %>%  # replace geo_sf with your sf object
  addTiles() %>%             # base OpenStreetMap tiles
  addCircleMarkers(
    ~st_coordinates(geometry)[,1],  # longitude
    ~st_coordinates(geometry)[,2],  # latitude
    radius = 4,
    color = "navy",
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0("<strong>", street, " ", house_number, "</strong><br>",
                    city, ", ", postal_code, "<br>",
                    "Score: ", round(score,2)))


#Gemeinden aggregation 
gem_path <- "Desktop/Lifties_Austria/austria_gemeinden_simplified.geojson"   
austria_gemeinden <- st_read(gem_path)
# Repair invalid geometries
austria_gemeinden <- st_make_valid(austria_gemeinden)
#check CRS
st_crs(austria_gemeinden)
st_crs(final_results)
final_results <- st_transform(final_results, st_crs(austria_gemeinden))

points_with_gemeinden <- st_join(final_results, austria_gemeinden, left = TRUE)

agg_gemeinden <- points_with_gemeinden %>%
  st_set_geometry(NULL) %>%       # drop geometry for aggregation
  group_by(g_id) %>%              # municipality name field
  summarise(address_count = n(), .groups = "drop") %>%
  arrange(desc(address_count))

gemeinden_customer_counts <- austria_gemeinden %>%
  left_join(agg_gemeinden, by = "g_id") %>%
  mutate(address_count = if_else(is.na(address_count), 0L, address_count))
st_write(gemeinden_customer_counts, "Desktop/Lifties_Austria/Final_Data_Cleaned/gemeinden_customer_counts.geojson", driver = 'GeoJSON')

#Bezirke aggregation 
bez_path <- "Desktop/Lifties_Austria/austria_bezirke_simplified.geojson"   
austria_bezirke <- st_read(bez_path)
# Repair invalid geometries
austria_bezirke <- st_make_valid(austria_bezirke)
#check CRS
st_crs(austria_bezirke)
st_crs(final_results)
final_results <- st_transform(final_results, st_crs(austria_bezirke))

#IMPORTANT: DROP ALL ROWS WITH VIENNA DISTRICTS BECAUSE THE ENTIRE CITY IS ITS OWN PB
austria_bezirke <- austria_bezirke[1:94, ]

points_with_bezirke <- st_join(final_results, austria_bezirke, left = TRUE)

agg_bezirke <- points_with_bezirke %>%
  st_set_geometry(NULL) %>%       # drop geometry for aggregation
  group_by(g_id) %>%              # municipality name field
  summarise(address_count = n(), .groups = "drop") %>%
  arrange(desc(address_count))

bezirke_customer_counts <- austria_bezirke %>%
  left_join(agg_bezirke, by = "g_id") %>%
  mutate(address_count = if_else(is.na(address_count), 0L, address_count))
st_write(bezirke_customer_counts, "Desktop/Lifties_Austria/Final_Data_Cleaned/bezirke_customer_counts.geojson", driver = 'GeoJSON')





# map results by year
leaflet(data = results_22) %>%  # replace geo_sf with your sf object
  addTiles() %>%             # base OpenStreetMap tiles
  addCircleMarkers(
    ~st_coordinates(geometry)[,1],  # longitude
    ~st_coordinates(geometry)[,2],  # latitude
    radius = 4,
    color = "navy",
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0("<strong>", street, " ", house_number, "</strong><br>",
                    city, ", ", postal_code, "<br>",
                    "Score: ", round(score,2))
  )

leaflet(data = results_23) %>%  # replace geo_sf with your sf object
  addTiles() %>%             # base OpenStreetMap tiles
  addCircleMarkers(
    ~st_coordinates(geometry)[,1],  # longitude
    ~st_coordinates(geometry)[,2],  # latitude
    radius = 4,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0("<strong>", street, " ", house_number, "</strong><br>",
                    city, ", ", postal_code, "<br>",
                    "Score: ", round(score,2))
  )

leaflet(data = results_24) %>%  # replace geo_sf with your sf object
  addTiles() %>%             # base OpenStreetMap tiles
  addCircleMarkers(
    ~st_coordinates(geometry)[,1],  # longitude
    ~st_coordinates(geometry)[,2],  # latitude
    radius = 4,
    color = "darkgreen",
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0("<strong>", street, " ", house_number, "</strong><br>",
                    city, ", ", postal_code, "<br>",
                    "Score: ", round(score,2))
  )

















st_crs(results_22)
st_crs(results_23)
st_crs(results_24)

#harmonize CRS
results_22 <- st_transform(results_22, st_crs(austria_gemeinden))
results_23 <- st_transform(results_23, st_crs(austria_gemeinden))
results_24 <- st_transform(results_24, st_crs(austria_gemeinden))

#join, aggregate, and write to geojson
points_with_gemeinde_22 <- st_join(results_22, austria_gemeinden, left = TRUE)
agg_gemeinde_22 <- points_with_gemeinde_22 %>%
  st_set_geometry(NULL) %>%       # drop geometry for aggregation
  group_by(g_id) %>%              # municipality name field
  summarise(address_count = n(), .groups = "drop") %>%
  arrange(desc(address_count))
gemeinde_counts_22 <- austria_gemeinden %>%
  left_join(agg_gemeinde_22, by = "g_id") %>%
  mutate(address_count = if_else(is.na(address_count), 0L, address_count))
st_write(gemeinde_counts_22, "Desktop/Lifties_Austria/Final_Data_Cleaned/gemeinde_counts_22.geojson", driver = 'GeoJSON')

points_with_gemeinde_23 <- st_join(results_23, austria_gemeinden, left = TRUE)
agg_gemeinde_23 <- points_with_gemeinde_23 %>%
  st_set_geometry(NULL) %>%       # drop geometry for aggregation
  group_by(g_id) %>%              # municipality name field
  summarise(address_count = n(), .groups = "drop") %>%
  arrange(desc(address_count))
gemeinde_counts_23 <- austria_gemeinden %>%
  left_join(agg_gemeinde_23, by = "g_id") %>%
  mutate(address_count = if_else(is.na(address_count), 0L, address_count))
st_write(gemeinde_counts_23, "Desktop/Lifties_Austria/Final_Data_Cleaned/gemeinde_counts_23.geojson", driver = 'GeoJSON')

points_with_gemeinde_24 <- st_join(results_24, austria_gemeinden, left = TRUE)
agg_gemeinde_24 <- points_with_gemeinde_24 %>%
  st_set_geometry(NULL) %>%       # drop geometry for aggregation
  group_by(g_id) %>%              # municipality name field
  summarise(address_count = n(), .groups = "drop") %>%
  arrange(desc(address_count))
gemeinde_counts_24 <- austria_gemeinden %>%
  left_join(agg_gemeinde_24, by = "g_id") %>%
  mutate(address_count = if_else(is.na(address_count), 0L, address_count))
st_write(gemeinde_counts_24, "Desktop/Lifties_Austria/Final_Data_Cleaned/gemeinde_counts_24.geojson", driver = 'GeoJSON')


#Combine all three years
agg_gem_cust_22_23_24 <- agg_gemeinde_22 %>%
  left_join(agg_gemeinde_23, by = "g_id") %>%
  left_join(agg_gemeinde_24, by = "g_id")
agg_gem_cust_22_23_24 <- agg_gem_cust_22_23_24 %>%
  mutate(total_sum = rowSums(across(c(address_count.x, address_count.y, address_count)), na.rm = TRUE)) %>% 
  select(g_id, total_sum)

# write final gemeinden file
gemeinde_counts_total <- austria_gemeinden %>%
  left_join(agg_gem_cust_22_23_24, by = "g_id")
st_write(gemeinde_counts_total, "Desktop/Lifties_Austria/Final_Data_Cleaned/Customers/gemeinde_counts_total.geojson", driver = 'GeoJSON')

# aggregate up to bezirke level and write to file
gem_totals_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Customers/gemeinde_counts_total.geojson"   
gem_totals <- st_read(gem_totals_path)
gem_totals <- gem_totals %>%
  mutate(total_sum = if_else(is.na(total_sum), 0L, total_sum))
