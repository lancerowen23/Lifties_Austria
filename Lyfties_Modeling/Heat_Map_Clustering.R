### Austria Mapping Lyfties Combined Data
### Author: Lance R. Owen
### Date: 6 February 2026

library(sf)
library(dplyr)
library(leaflet)
library(MASS)     # glm.nb
library(scales)   # for pretty breaks (optional)

gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/gemeinden_final.geojson"
gem_complete <- st_read(gem_path, quiet = FALSE)

gem <- gem_complete %>%
  mutate(
    address_count = as.numeric(address_count),
    population    = as.numeric(total_pop)
  ) %>%
  filter(!is.na(address_count), !is.na(population), population > 0)

# --- Fit Poisson with offset; switch to NegBin if overdispersed ---
m_pois <- glm(address_count ~ 1 + offset(log(population)),
              family = poisson(), data = gem)

disp <- sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)

m <- if (disp > 1.5) {
  glm.nb(address_count ~ 1 + offset(log(population)), data = gem)
} else {
  m_pois
}

# --- Per-feature significance (approx): Pearson residual -> z -> p -> FDR q ---
gem_sig <- gem %>%
  mutate(
    pearson_resid = residuals(m, type = "pearson"),
    z             = pearson_resid,
    p_value       = 2 * pnorm(-abs(z)),
    q_value       = p.adjust(p_value, method = "BH"),
    heat          = -log10(pmax(q_value, 1e-300)),  # intensity
    signed_heat   = ifelse(pearson_resid >= 0, heat, -heat),
    sig_class = case_when(
      q_value < 0.001 ~ "<0.001",
      q_value < 0.01  ~ "<0.01",
      q_value < 0.05  ~ "<0.05",
      TRUE            ~ "ns"
    )
  )

# Leaflet expects lon/lat
gem_sig_ll <- st_transform(gem_sig, 4326)

# --- Color scales ---
# Heat (always positive)
pal_heat <- colorNumeric(palette = "viridis", domain = gem_sig_ll$heat, na.color = "transparent")

# Signed heat (diverging)
pal_signed <- colorNumeric(palette = "RdBu", domain = gem_sig_ll$signed_heat, reverse = TRUE,
                           na.color = "transparent")

# Popup (adjust ID/name field if you have it)
popup_html <- function(df) {
  sprintf(
    "<strong>%s</strong><br/>
     address_count: %s<br/>
     population: %s<br/>
     Pearson resid: %.2f<br/>
     p: %.3g<br/>
     q (FDR): %.3g<br/>
     -log10(q): %.2f<br/>
     class: %s",
    if ("name" %in% names(df)) df$name else "Gemeinde",
    df$address_count, df$population,
    df$pearson_resid, df$p_value, df$q_value, df$heat,
    df$sig_class
  )
}

leaflet(gem_sig_ll, options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Layer 1: Heat intensity only
  addPolygons(
    group = "Significance heat (-log10 q)",
    fillColor = ~pal_heat(heat),
    fillOpacity = 0.75,
    color = "#FFFFFF", weight = 0.3,
    popup = ~popup_html(gem_sig_ll)
  ) %>%
  
  # Layer 2: Signed heat (higher-than-expected vs lower-than-expected)
  addPolygons(
    group = "Signed heat (higher vs lower)",
    fillColor = ~pal_signed(signed_heat),
    fillOpacity = 0.75,
    color = "#FFFFFF", weight = 0.3,
    popup = ~popup_html(gem_sig_ll)
  ) %>%
  
  addLayersControl(
    baseGroups = c("Significance heat (-log10 q)", "Signed heat (higher vs lower)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addLegend(
    position = "bottomright",
    pal = pal_heat,
    values = ~heat,
    title = HTML("-log10(FDR q)"),
    opacity = 0.75
  )


library(spdep)

# Use the object from before
gem_sp <- gem_sig_ll

# Neighbors (queen contiguity is standard for polygons)
nb <- poly2nb(gem_sp, queen = TRUE)

# Spatial weights
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Global Moran's I on signed significance
moran_global <- moran.test(
  gem_sp$signed_heat,
  lw,
  zero.policy = TRUE
)

moran_global


lisa <- localmoran(
  gem_sp$signed_heat,
  lw,
  zero.policy = TRUE
)

gem_sp$lisa_I     <- lisa[, "Ii"]
gem_sp$lisa_p     <- lisa[, "Pr(z != E(Ii))"]

# Mean for quadrant classification
mean_val <- mean(gem_sp$signed_heat, na.rm = TRUE)

gem_sp$lisa_cluster <- case_when(
  gem_sp$signed_heat >= mean_val & gem_sp$lisa_I > 0 & gem_sp$lisa_p < 0.05 ~ "High–High",
  gem_sp$signed_heat <  mean_val & gem_sp$lisa_I > 0 & gem_sp$lisa_p < 0.05 ~ "Low–Low",
  gem_sp$signed_heat >= mean_val & gem_sp$lisa_I < 0 & gem_sp$lisa_p < 0.05 ~ "High–Low",
  gem_sp$signed_heat <  mean_val & gem_sp$lisa_I < 0 & gem_sp$lisa_p < 0.05 ~ "Low–High",
  TRUE ~ "Not significant"
)


pal_lisa <- colorFactor(
  palette = c(
    "High–High" = "#B2182B",
    "Low–Low"   = "#2166AC",
    "High–Low"  = "#EF8A62",
    "Low–High"  = "#67A9CF",
    "Not significant" = "#EEEEEE"
  ),
  domain = gem_sp$lisa_cluster
)

leaflet(gem_sp, options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal_lisa(lisa_cluster),
    fillOpacity = 0.8,
    color = "#444444", weight = 0.3,
    popup = ~paste0(
      "<strong>LISA cluster:</strong> ", lisa_cluster, "<br/>",
      "Local p-value: ", signif(lisa_p, 3), "<br/>",
      "Signed heat: ", round(signed_heat, 2)
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_lisa,
    values = ~lisa_cluster,
    title = "Local Moran’s I clusters"
  )

#Is the number of 75+ residents significantly associated with address counts, controlling for total population?
  
library(dplyr)
library(MASS)

gem <- gem_complete %>%
  mutate(
    address_count = as.numeric(address_count),
    population    = as.numeric(total_pop),
    pop75         = as.numeric(pop_75andUp)
  ) %>%
  filter(population > 0, !is.na(pop75))

# Baseline model (population only)
m0 <- glm(
  address_count ~ offset(log(population)),
  family = poisson(),
  data = gem
)

# Check dispersion
disp <- sum(residuals(m0, type = "pearson")^2) / df.residual(m0)

family_use <- if (disp > 1.5) "negbin" else "poisson"

# Model with 75+ population
m1 <- if (family_use == "negbin") {
  glm.nb(address_count ~ pop75 + offset(log(population)), data = gem)
} else {
  glm(address_count ~ pop75 + offset(log(population)),
      family = poisson(), data = gem)
}

summary(m1)

# Model with income
m2 <- if (family_use == "negbin") {
  glm.nb(address_count ~ income + offset(log(population)), data = gem)
} else {
  glm(address_count ~ income + offset(log(population)),
      family = poisson(), data = gem)
}

summary(m2)


#Is the spatial clustering we observed actually driven by the 75+ population?

library(spdep)

gem$resid_m1 <- residuals(m1, type = "pearson")

nb <- poly2nb(gem, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

moran.test(gem$resid_m1, lw, zero.policy = TRUE)

# basic total_population map

gem_complete$log_total_pop <- log1p(gem_complete$total_pop)


p_pop <- ggplot(gem_complete) +
  geom_sf(aes(fill = log_total_pop), color = "lightgrey", linewidth = 0.01) +
  scale_fill_gradient(
    low = "#F2E5FF",
    high = "#5B2A86",
    name = "Log(Total Population)",
    labels = comma
  ) +
  labs(
    title = "Total Population by Municipality",
    subtitle = "Log-scaled to reflect large differences in population size",
    caption = "Source: Statistik Austria"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right"
  )

p_pop


# residuals maps

# ---- Fit a simple population-only baseline model ----
# log1p handles skew and zeros safely
gem <- gem_complete %>% mutate(log_total_pop = log1p(total_pop))

m_pop <- lm(address_count ~ log_total_pop, data = gem)

# ---- Add predictions + residuals (Observed - Predicted) ----
gem_res <- gem %>%
  mutate(
    pred_pop = predict(m_pop, newdata = gem),
    resid_pop = address_count - pred_pop
  )

# ---- (Optional) Winsorize residuals for nicer mapping (reduces outlier domination) ----
lims <- quantile(gem_res$resid_pop, probs = c(0.02, 0.98), na.rm = TRUE)
gem_res <- gem_res %>%
  mutate(resid_pop_w = pmin(pmax(resid_pop, lims[1]), lims[2]))

# ---- Residual map ----
p_resid <- ggplot(gem_res) +
  geom_sf(aes(fill = resid_pop_w), color = "white", linewidth = 0.01) +
  scale_fill_gradient2(
    low = "#2B8CBE",
    mid = "white",
    high = "#E34A33",
    midpoint = 0,
    name = "Residual\n(Observed - Predicted)",
    labels = comma
  ) +
  labs(
    title = "Residuals After Accounting for Population",
    subtitle = "Baseline model: address_count ~ log(1 + total_pop)",
    caption = "Positive = higher than expected for population size; Negative = lower than expected"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right"
  )

p_resid

# Structural uplift difference mapping

# import Catboost predictions
catboost_pred <- read.csv("Desktop/Lifties_Austria/Lyfties_Modeling/model_predictions_residuals.csv")
catboost_pred <- catboost_pred %>% dplyr::select(g_id, pred_catboost)

# join to geojson
gem <- merge(gem_complete, catboost_pred, by.x = 'g_id', by.y = 'g_id')

# ---- Population-only baseline model ----
m_pop <- lm(address_count ~ log_total_pop, data = gem)

gem <- gem %>%
  mutate(
    pred_pop = predict(m_pop, newdata = gem),
    uplift   = catboost_pred - pred_pop   # "structural uplift" beyond population
  )

# ---- Optional: winsorize to keep outliers from dominating the color scale ----
lims <- quantile(gem$uplift, probs = c(0.02, 0.98), na.rm = TRUE)
gem <- gem %>%
  mutate(uplift_w = pmin(pmax(uplift, lims[1]), lims[2]))

# ---- Map: CatBoost vs population-only baseline ----
p_uplift <- ggplot(gem) +
  geom_sf(aes(fill = uplift_w$pred_catboost), color = "grey", linewidth = 0.05) +
  scale_fill_gradient2(
    low = "#2B8CBE",
    mid = "white",
    high = "#E34A33",
    midpoint = 0,
    name = "CatBoost − Pop Baseline",
    labels = comma
  ) +
  labs(
    title = "Structural Uplift Beyond Population Scale",
    subtitle = "Positive values = higher predicted outcomes than population alone would suggest",
    caption = "Uplift = CatBoost prediction minus population-only baseline prediction"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right"
  )

p_uplift

