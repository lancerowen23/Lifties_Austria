#GEOSPATIAL GEOJSON DATA PULL FROM STATISTIK.AT

library(sf)
library(rmapshaper)

###GEMEINDEN
# --- 1. WFS → GeoJSON (server converts MULTISURFACE → MULTIPOLYGON) ---
url <- paste0(
  "https://www.statistik.at/gs-open/GEODATA/ows?",
  "service=WFS&version=2.0.0&request=GetFeature&",
  "typeName=GEODATA:STATISTIK_AUSTRIA_GEM_20240101&",   # <- Gemeinden dataset
  "outputFormat=application/json"  # <- crucial!!
)

# --- 2. Read directly into sf (clean MULTIPOLYGON) ---
gdf <- st_read(url, quiet = FALSE)

# --- 3. Transform to WGS84 for mapping ---
gdf <- st_transform(gdf, 4326)

# --- 4. Simplify for performance ---
gdf <- ms_simplify(gdf, keep = 0.1, keep_shapes = TRUE)

# --- 5. Plot to verify ---
plot(gdf["geometry"], main = "Simplified Austrian Gemeinden (gdf)")

# --- 6. Export to GeoJSON ---
st_write(
  gdf,
  "Desktop/Lifties_Austria/austria_gemeinden_simplified.geojson",
  driver = "GeoJSON",
  delete_dsn = TRUE
)

###PolBez
# --- 1. WFS → GeoJSON (server converts MULTISURFACE → MULTIPOLYGON) ---
url <- paste0(
  "https://www.statistik.at/gs-open/GEODATA/ows?",
  "service=WFS&version=2.0.0&request=GetFeature&",
  "typeName=GEODATA:STATISTIK_AUSTRIA_POLBEZ_20240101&",   # <- Gemeinden dataset
  "outputFormat=application/json"  # <- crucial!!
)

# --- 2. Read directly into sf (clean MULTIPOLYGON) ---
gdf <- st_read(url, quiet = FALSE)

# --- 3. Transform to WGS84 for mapping ---
gdf <- st_transform(gdf, 4326)

# --- 4. Simplify for performance ---
gdf <- ms_simplify(gdf, keep = 0.1, keep_shapes = TRUE)

# --- 5. Plot to verify ---
plot(gdf["geometry"], main = "Simplified Austrian Bezirke")

# --- 6. Export to GeoJSON ---
st_write(
  gdf,
  "Desktop/Lifties_Austria/austria_bezirke_simplified.geojson",
  driver = "GeoJSON",
  delete_dsn = TRUE
)
