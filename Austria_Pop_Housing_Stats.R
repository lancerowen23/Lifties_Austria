#Austria Pop and Housing Stats Analysis

library(corrplot)
library(GGally)

#Population
#Bezirke
bez_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/bez_finalpop_data.geojson"  
bez_pop <- st_read(bez_path, quiet = FALSE)
#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/gem_finalpop_data.geojson"   
gem_pop <- st_read(gem_path, quiet = FALSE)

#Housing
#Bezirke
bez_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/bez_finalhousing_data.geojson"  
bez_housing <- st_read(bez_path, quiet = FALSE)
#Gemeinden
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/gem_finalhousing_data.geojson"   
gem_housing <- st_read(gem_path, quiet = FALSE)

#combine and drop geometry for analysis
bez_pop2 <- bez_pop %>% st_drop_geometry()
bez_housing2 <- bez_housing %>% st_drop_geometry()
bez_combined <- merge(bez_pop2, bez_housing2, by.x = "id", by.y = "id") %>% 
  select(-name.y)
write.csv(bez_combined, "Desktop/Lifties_Austria/Final_Data_Cleaned/bez_final_combined_data.csv", row.names = FALSE)


gem_pop2 <- gem_pop %>% st_drop_geometry()
gem_housing2 <- gem_housing %>% st_drop_geometry()
gem_combined <- merge(gem_pop2, gem_housing2, by.x = "id", by.y = "id") %>% 
  select(-name.y)
write.csv(gem_combined, "Desktop/Lifties_Austria/Final_Data_Cleaned/gem_final_combined_data.csv", row.names = FALSE)


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
bez_combined_pct <- bez_combined %>% select(total_pop, percent_60to74, percent_75andUp, 
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
gem_combined_pct <- gem_combined %>% select(total_pop, percent_60to74, percent_75andUp, 
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
