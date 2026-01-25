### STATISTICAL ANALYSIS ###

#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/gemeinden_final.geojson"   
gem_customers <- st_read(gem_path, quiet = FALSE)
gem_customers <- gem_customers %>%
  mutate(address_count = if_else(is.na(address_count), 0L, address_count))

