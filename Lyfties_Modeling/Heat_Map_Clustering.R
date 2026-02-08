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


#Is the spatial clustering we observed actually driven by the 75+ population?

library(spdep)

gem$resid_m1 <- residuals(m1, type = "pearson")

nb <- poly2nb(gem, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

moran.test(gem$resid_m1, lw, zero.policy = TRUE)

