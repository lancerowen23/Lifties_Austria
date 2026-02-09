#Austria Pop and Housing Stats Analysis

library(corrplot)
library(GGally)
library(sf)
library(dplyr)
library(scales)
library(leaflet)

#Population
#Bezirke
bez_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Population/bez_finalpop_data.geojson"  
bez_pop <- st_read(bez_path, quiet = FALSE)
#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Population/gem_finalpop_data.geojson"   
gem_pop <- st_read(gem_path, quiet = FALSE)

#Housing
#Bezirke
bez_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Housing/bez_finalhousing_data.geojson"  
bez_housing <- st_read(bez_path, quiet = FALSE)
#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Housing/gem_finalhousing_data.geojson"   
gem_housing <- st_read(gem_path, quiet = FALSE)

#combine and drop geometry for analysis
bez_pop2 <- bez_pop %>% st_drop_geometry()
bez_housing2 <- bez_housing %>% st_drop_geometry()
bez_combined <- merge(bez_pop2, bez_housing2, by.x = "id", by.y = "id") %>% 
  dplyr::select(-name.y)
#write.csv(bez_combined, "Desktop/Lifties_Austria/Final_Data_Cleaned/bez_final_combined_data.csv", row.names = FALSE)


gem_pop2 <- gem_pop %>% st_drop_geometry()
gem_housing2 <- gem_housing %>% st_drop_geometry()
gem_combined <- merge(gem_pop2, gem_housing2, by.x = "id", by.y = "id") %>% 
  dplyr::select(-name.y)
#write.csv(gem_combined, "Desktop/Lifties_Austria/Final_Data_Cleaned/gem_final_combined_data.csv", row.names = FALSE)


cor_test_result <- cor.test(gem_pop$pop_75andUp, gem_pop$households_no_kids, method = "pearson")

# Print results
cor_test_result

# Plot to visualize
plot(gem_pop$pop_75andUp,
     gem_pop$households_no_kids,
     xlab = "Population Age 75+",
     ylab = "Number of Households w/ No Children",
     main = "Relationship Between Older Age Prevalence and No-Children Households")
abline(lm(households_no_kids ~ pop_75andUp, data = gem_pop), col = "red")


cor_test_result2 <- cor.test(gem_pop$pop_60to74, gem_pop$households_no_kids, method = "pearson")
cor_test_result2

# Plot to visualize
plot(gem_pop$pop_60to74,
     gem_pop$households_no_kids,
     xlab = "Population Age 60-74",
     ylab = "Number of Households w/ No Children",
     main = "Relationship Between Older Age Prevalence and No-Children Households")
abline(lm(households_no_kids ~ pop_60to74, data = gem_pop), col = "red")

#Look at housing data and age correlations
#bezirke
bez_combined_pct <- bez_combined %>% dplyr::select(total_pop, percent_60to74, percent_75andUp, 
                                            percent_households_no_kids,
                                            percent_1floor, percent_2floor, percent_3to5floor, 
                                            percent_own_occ, percent_priv_one, percent_res_one_dwell)

num_df <- bez_combined_pct[sapply(bez_combined_pct, is.numeric)]
cor_mat <- cor(num_df, use = "pairwise.complete.obs")

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",  # correlation values
  number.cex = 0.7,
  col = colorRampPalette(c("blue", "white", "red"))(200)
)

#gemeinden
gem_combined_pct <- gem_combined %>% dplyr::select(total_pop, percent_60to74, percent_75andUp, 
                                            percent_households_no_kids,
                                            percent_1floor, percent_2floor, percent_3to5floor, 
                                            percent_own_occ, percent_priv_one, percent_res_one_dwell)

num_df <- gem_combined_pct[sapply(gem_combined_pct, is.numeric)]
cor_mat <- cor(num_df, use = "pairwise.complete.obs")

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",  # correlation values
  number.cex = 0.7,
  col = colorRampPalette(c("blue", "white", "red"))(200)
)

# Plot to visualize
plot(gem_pop$percent_75andUp,
     gem_housing$percent_1floor,
     xlab = "Population Age 75+",
     ylab = "Buildings with only one floor above ground",
     main = "Relationship Between Older Age Prevalence and Two Storeys")
abline(lm(households_no_kids ~ pop_60to74, data = gem_pop), col = "red")

#Building an index for 75+ and buildings with 2-5 floors above ground
#calculate total buildings from % and absolute building count in category
bez_combined <- bez_combined %>%
  mutate(
    total_buildings = abs_2floor / (percent_2floor / 100))

#create index, including a step of combining 2 floors above ground and 3 to 5 floors above ground
bez_index <- bez_combined %>% 
  mutate(pct_2to5floors = (abs_2floor + abs_3to5floor) / total_buildings) %>% 
  select(id, percent_75andUp, pct_2to5floors, percent_priv_one) %>% 
  mutate(
    n_age    = rescale(percent_75andUp, to = c(0,1)),
    n_height = rescale(pct_2to5floors, to = c(0,1)),
    n_private = rescale(percent_priv_one, to = c(0,1)),
    
    ASDI = 0.33 * n_age + 0.33 * n_height * 0.33 * n_private
  ) %>%
  arrange(desc(ASDI))

#join to geojson and map
bez_index_map <- bez_pop %>% 
  left_join(bez_index, by = c("id" = "id")) %>% 
  dplyr::select(id, name, ASDI, geometry)

# Create a color palette for 0–1 index
pal <- colorNumeric(
  palette = "viridis",   # or "magma", "plasma", "inferno"
  domain  = bez_index_map$ASDI
)

# Build map
leaflet(bez_index_map) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    fillColor   = ~pal(ASDI),
    fillOpacity = 0.75,
    color       = "white",
    weight      = 1,
    opacity     = 1,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0(
      "<strong>", name, "</strong><br/>",
      "ASDI: ", round(ASDI, 3)
    ) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-size" = "13px")
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~ASDI,
    title = "ASDI (0–1)",
    opacity = 1
  )

#Same with gemeinden
#calculate total building column 
gem_combined <- gem_combined %>%
  mutate(
    total_buildings = abs_2floor / (percent_2floor / 100))

#Build an index, combine 2floor and 2-5floor numbers
gem_index <- gem_combined %>% 
  mutate(pct_2to5floors = (abs_2floor + abs_3to5floor) / total_buildings) %>% 
  select(id, percent_75andUp, pct_2to5floors, percent_priv_one) %>% 
  mutate(
    n_age    = rescale(percent_75andUp, to = c(0,1)),
    n_height = rescale(pct_2to5floors, to = c(0,1)),
    n_private = rescale(percent_priv_one, to = c(0,1)),
    
    Gem_ASDI = 0.33 * n_age + 0.33 * n_height * 0.33 * n_private
  ) %>%
  arrange(desc(Gem_ASDI))

#join to geojson and map
gem_index_map <- gem_pop %>% 
  left_join(gem_index, by = c("id" = "id")) %>% 
  dplyr::select(id, name, Gem_ASDI, geometry)

# Create a color palette for 0–1 index
pal <- colorNumeric(
  palette = "viridis",   # or "magma", "plasma", "inferno"
  domain  = gem_index_map$Gem_ASDI
)

# Build map
leaflet(gem_index_map) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    fillColor   = ~pal(Gem_ASDI),
    fillOpacity = 0.75,
    color       = "white",
    weight      = 1,
    opacity     = 1,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0(
      "<strong>", name, "</strong><br/>",
      "ASDI: ", round(Gem_ASDI, 3)
    ) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-size" = "13px")
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~Gem_ASDI,
    title = "ASDI (0–1)",
    opacity = 1
  )
