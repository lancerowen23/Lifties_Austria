### INCOME/WEALTH DATA CLEANED ###

#pensioner income 
gem_pen_income <- read.csv('Desktop/Lifties_Austria/Income_Data_Raw/AvgGrossEarnPensionersFullYear.csv', encoding = "UTF-8")

#land price per square meter
gem_land_price <- read.csv('Desktop/Lifties_Austria/Income_Data_Raw/AvgLandPriceForConstr.csv', encoding = "UTF-8")

#bind to gemeinden GeoJSON
#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/gemeinden_final.geojson"   
gem_customers <- st_read(gem_path, quiet = FALSE)
gem_customers <- gem_customers %>%
  mutate(address_count = if_else(is.na(address_count), 0L, address_count))

View(gem_customers)

gem_combined <- merge(gem_customers, gem_pen_income, by.x = 'g_id', by.y = 'id')
gem_combined <- merge(gem_combined, gem_land_price, by.x = "g_id", by.y = "id")

gem_final <- gem_combined %>% select(-name.x, -name.y)
colnames(gem_final)

#write csv and geojson
gem_final %>%
  st_drop_geometry() %>%
  write.csv("Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/gem_final_combined_data.csv", row.names = FALSE)
st_write(gem_final, "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/gemeinden_final.geojson", driver = 'GeoJSON')
