##Mapping Elderly Populations // Austria 
#All data 2024
#Spatial and Pop Data from Statistik Austria

##Author: Lance R. Owen

library(readr)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)

#Pop Files Import

#total population
bez_pop_total <- read_csv('Desktop/Lifties_Austria/Population_Data_Raw/pop_total_bez.csv')
gem_pop_total <- read.csv('Desktop/Lifties_Austria/Population_Data_Raw/pop_total_gem.csv', encoding = "UTF-8")
#population 60-74 years
bez_pop_60to74 <- read_csv('Desktop/Lifties_Austria/Population_Data_Raw/pop_60to74_bez.csv')
gem_pop_60to74 <- read.csv('Desktop/Lifties_Austria/Population_Data_Raw/pop_60to74_gem.csv', encoding = "UTF-8")
#population 75+ years
bez_pop_75andUp <- read_csv('Desktop/Lifties_Austria/Population_Data_Raw/pop_75andUp_bez.csv')
gem_pop_75andUp <- read.csv('Desktop/Lifties_Austria/Population_Data_Raw/pop_75andUp_gem.csv', encoding = "UTF-8")

#join pop data
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
#save as GeoJSON
st_write(bez_final_df, "Desktop/Lifties_Austria/Final_Data_Cleaned/bez_finalpop_data.geojson", driver = 'GeoJSON')
#save with just pop data
bez_final_df %>%
  st_drop_geometry() %>%
  write.csv("Desktop/Lifties_Austria/Final_Data_Cleaned/bez_finalpop_data.csv", row.names = FALSE)

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
#save as GeoJSON
st_write(gem_final_df, "Desktop/Lifties_Austria/Final_Data_Cleaned/gem_finalpop_data.geojson", driver = 'GeoJSON')
#save with just pop data
gem_final_df %>%
  st_drop_geometry() %>%
  write.csv("Desktop/Lifties_Austria/Final_Data_Cleaned/gem_finalpop_data.csv", row.names = FALSE)


#housing
#buildings with 1 floor above ground
bez_one_floor <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/bez_buildings_with_one_floor_above_ground.csv')
gem_one_floor <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/gem_buildings_with_one_floor_above_ground.csv')
#buildings with 2 floors above ground
bez_two_floor <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/bez_buildings_with_two_floors_above_ground.csv')
gem_two_floor <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/gem_buildings_with_two_floors_above_ground.csv')
#buildings with 3-5 floors above ground
bez_3to5_floor <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/bez_buildings_3to5_floors_above_ground.csv')
gem_3to5_floor <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/gem_buildings_3to5_floors_above_ground.csv')
#owner-occupied dwellings
bez_owner_occ <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/bez_owner_occupied_dwellings.csv')
gem_owner_occ <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/gem_owner_occupied_dwellings.csv')
#private household with one person
bez_private_one <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/bez_private_household_1person.csv')
gem_private_one <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/gem_private_household_1person.csv')
#residential building with one dwelling
bez_res_one_dwelling <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/bez_res_build_with_one_dwelling.csv')
gem_res_one_dwelling <- read.csv('Desktop/Lifties_Austria/Housing_Data_Raw/gem_res_build_with_one_dwelling.csv')


#join housing data
#bezirke
bez_df2 <- merge(bezirke, bez_one_floor, by.x="g_id", by.y="id")
bez_df2 <- merge(bez_df2, bez_two_floor, by.x="g_id", by.y="id")
bez_df2 <- merge(bez_df2, bez_3to5_floor, by.x="g_id", by.y="id")
bez_df2 <- merge(bez_df2, bez_owner_occ, by.x="g_id", by.y="id")
bez_df2 <- merge(bez_df2, bez_private_one, by.x="g_id", by.y="id")
bez_df2 <- merge(bez_df2, bez_res_one_dwelling, by.x="g_id", by.y="id")

bez_final_df2 <- bez_df2 %>% 
  select(g_id, 
         g_name, 
         percent.x, 
         abs.x, 
         percent.y, 
         abs.y, 
         percent.x.1, 
         abs.x.1,
         percent.y.1, 
         abs.y.1, 
         percent.x.2,
         abs.x.2,
         percent.y.2, 
         abs.y.2) %>% 
  rename(id = g_id,
         name = g_name,
         percent_1floor = percent.x, 
         abs_1floor = abs.x, 
         percent_2floor = percent.y, 
         abs_2floor = abs.y, 
         percent_3to5floor = percent.x.1, 
         abs_3to5floor = abs.x.1,
         percent_own_occ = percent.y.1, 
         abs_own_occ = abs.y.1, 
         percent_priv_one = percent.x.2,
         abs_priv_one = abs.x.2,
         percent_res_one_dwell = percent.y.2, 
         abs_res_one_dwell = abs.y.2)
View(bez_final_df2)
#save as GeoJSON
st_write(bez_final_df2, "Desktop/Lifties_Austria/Final_Data_Cleaned/bez_finalhousing_data.geojson", driver = 'GeoJSON')
#save with just pop data
bez_final_df2 %>%
  st_drop_geometry() %>%
  write.csv("Desktop/Lifties_Austria/Final_Data_Cleaned/bez_finalhousing_data.csv", row.names = FALSE)


#gemeinden (housing)
gem_df2 <- merge(gemeinden, gem_one_floor, by.x="g_id", by.y="id")
gem_df2 <- merge(gem_df2, gem_two_floor, by.x="g_id", by.y="id")
gem_df2 <- merge(gem_df2, gem_3to5_floor, by.x="g_id", by.y="id")
gem_df2 <- merge(gem_df2, gem_owner_occ, by.x="g_id", by.y="id")
gem_df2 <- merge(gem_df2, gem_private_one, by.x="g_id", by.y="id")
gem_df2 <- merge(gem_df2, gem_res_one_dwelling, by.x="g_id", by.y="id")

gem_final_df2 <- gem_df2 %>% 
  select(g_id, 
         g_name, 
         percent.x, 
         abs.x, 
         percent.y, 
         abs.y, 
         percent.x.1, 
         abs.x.1,
         percent.y.1, 
         abs.y.1, 
         percent.x.2,
         abs.x.2,
         percent.y.2, 
         abs.y.2) %>% 
  rename(id = g_id,
         name = g_name,
         percent_1floor = percent.x, 
         abs_1floor = abs.x, 
         percent_2floor = percent.y, 
         abs_2floor = abs.y, 
         percent_3to5floor = percent.x.1, 
         abs_3to5floor = abs.x.1,
         percent_own_occ = percent.y.1, 
         abs_own_occ = abs.y.1, 
         percent_priv_one = percent.x.2,
         abs_priv_one = abs.x.2,
         percent_res_one_dwell = percent.y.2, 
         abs_res_one_dwell = abs.y.2)
View(gem_final_df2)

#save as GeoJSON
st_write(gem_final_df2, "Desktop/Lifties_Austria/Final_Data_Cleaned/gem_finalhousing_data.geojson", driver = 'GeoJSON')
#save with just pop data
gem_final_df2 %>%
  st_drop_geometry() %>%
  write.csv("Desktop/Lifties_Austria/Final_Data_Cleaned/gem_finalhousing_data.csv", row.names = FALSE)
