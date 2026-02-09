### Script 3: Geocoding Addresses for Customer Analysis

### This script takes the company data (three csv files with addresses for each
### Lift installation and the month of installation for 2022, 2023, and 2024) and 
### geocodes each address using the HERE geocoding service to produce a lat/long
### point for each location. Those location points are then aggregated to gemeinden
### level to anonymize them and to allows them to be analyzed vis-a-vis the gemeinden-level
### demographic and housing data. 

#NOTE: THIS SCRIPT CANNOT BE RUN WITHOUT THE API KEY FOR THE HERE GEOCODING SERVICE.

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
library(lubridate)
library(purrr)
library(tidyr)
library(ggthemes)

#data import
#2022
cust_22 <- read.csv('~/Downloads/P22_V3.csv')
#2023
cust_23 <- read.csv('~/Downloads/P23_V3.csv')
#2024
cust_24 <- read.csv('~/Downloads/P24_V3.csv')

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

#Create date field from month and year columns

cust_all$date <- dmy(
  paste("01", cust_all$month, cust_all$year)
)

# Create new index column
cust_all$id <- seq_len(nrow(cust_all))
# Remove number column (id for each of the three original dataframes)
cust_all <- cust_all %>% dplyr::select(-number)
# Move id to front
cust_all <- cust_all %>%
  relocate(id)

#geocode using HERE

set_key("API Key")

results <- geocode(cust_all$full_address)

final_results <- merge(results, cust_all, by.x = "id", by.y = "id")

# drop non matches
#final_results <- results %>% filter(access != "POINT EMPTY")
write_csv(final_results, "Desktop/final_geocode_results_Feb7.csv")

# map results
leaflet(data = final_results) %>%  # replace geo_sf with your sf object
  addTiles() %>%             # base OpenStreetMap tiles
  addCircleMarkers(
    ~st_coordinates(geometry)[,1],  # longitude
    ~st_coordinates(geometry)[,2],  # latitude
    radius = 2,
    color = "navy",
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste0("<strong>", street_clean, " ", house_number, "</strong><br>",
                    city.x, ", ", postal_code.y, "<br>",
                    "Score: ", round(score,2)))

# Check and plot seasonality

final_results <- final_results %>%
  mutate(
    month_date = month(date, label = TRUE, abbr = TRUE),
    year_date  = year(date)
  )

monthly_year <- final_results %>%
  count(year_date, month_date, name = "sales_n") %>%
  arrange(year_date, month_date)

ggplot(monthly_year %>% dplyr::filter(!is.na(month_date)),
     aes(month_date, sales_n, group = year_date, color = factor(year_date))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    x = "",
    y = "Installation count",
    color = "Year"
  ) 

monthly_ts <- final_results %>%
  mutate(year_month = floor_date(date, "month")) %>%
  count(year_month, name = "sales_n") %>%
  arrange(year_month)

ggplot(monthly_ts, aes(year_month, sales_n)) +
  geom_line(size = .5, color = "navy") +
  geom_point(size = 1, color = "navy") +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  theme_minimal() +
  labs(
    title = "Monthly Sales (2022–2024)",
    x = "",
    y = "Installation count"
  )

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
  group_by(g_name) %>%              # municipality name field
  summarise(address_count = n(), .groups = "drop") %>%
  arrange(desc(address_count))

gemeinden_customer_counts <- austria_gemeinden %>%
  left_join(agg_gemeinden, by = "g_name") %>%
  mutate(address_count = if_else(is.na(address_count), 0L, address_count))
st_write(gemeinden_customer_counts, "Desktop/Lifties_Austria/Final_Data_Cleaned/gemeinden_customer_counts.geojson", driver = 'GeoJSON')

#join to housing and population data and write as csv and geojson
gem_pop_housing <- read.csv("Desktop/Lifties_Austria/Final_Data_Cleaned/Customers/gem_final_combined_data.csv")
gem_final_data <- merge(gem_pop_housing, agg_gemeinden, by.x = "id", by.y = "g_id")
readr::write_csv(gem_final_data,
                 file = "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/gem_final_combined_data.csv")
gem_final_geojson <- merge(gemeinden_customer_counts, gem_pop_housing, by.x = "g_id", by.y = "id") %>% 
  select(-name.x)
st_write(gem_final_geojson, "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/gemeinden_final.geojson", driver = 'GeoJSON')

#Bezirke aggregation data and
bez_path <- "Desktop/Lifties_Austria/austria_bezirke_simplified.geojson"   
austria_bezirke <- st_read(bez_path)
# Repair invalid geometries
austria_bezirke <- st_make_valid(austria_bezirke)
#check CRS
st_crs(austria_bezirke)
st_crs(final_results)
final_results <- st_transform(final_results, st_crs(austria_bezirke))

#IMPORTANT: DROP ALL ROWS WITH VIENNA DISTRICTS BECAUSE THE ENTIRE CITY IS ITS OWN PB
#IN THE GEOJSON FROM STATISTIK AUSTIRA
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

#join to housing and population data and write as csv and geojson
bez_pop_housing <- read.csv("Desktop/Lifties_Austria/Final_Data_Cleaned/Customers/bez_final_combined_data.csv")
bez_final_data <- merge(bez_pop_housing, agg_bezirke, by.x = "id", by.y = "g_id")
readr::write_csv(bez_final_data,
                 file = "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/bez_final_combined_data.csv")

bez_final_geojson <- merge(bezirke_customer_counts, bez_pop_housing, by.x = "g_id", by.y = "id")
st_write(bez_final_geojson, "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/bezirke_final.geojson", driver = 'GeoJSON')



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
