### Austria Mapping Lyfties Bivariate Maps
### Author: Lance R. Owen
### Date: 7 February 2026

gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/gemeinden_final.geojson"
gem_complete <- st_read(gem_path, quiet = FALSE)

# =========================
# Packages
# =========================
library(sf)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(biscale)
library(tidyr)
library(patchwork)
library(scales)

# =========================
# Prep / derived variables
# =========================
dat <- gem_complete %>%
  mutate(
    # sales per capita (per 1,000 people is often easier to read)
    sales_pc = ifelse(total_pop > 0, address_count / total_pop, NA_real_),
    sales_per_1000 = 1000 * sales_pc,
    
    # price is character -> numeric (handles comma decimals too)
    price_sq_m_num = readr::parse_number(price_sq_m, locale = readr::locale(decimal_mark = ".")),
    price_sq_m_num = ifelse(is.na(price_sq_m_num),
                            readr::parse_number(price_sq_m, locale = readr::locale(decimal_mark = ",")),
                            price_sq_m_num)
  )

# Optional: drop rows missing key vars (keeps maps clean)
dat_core <- dat %>%
  filter(
    !is.na(sales_pc),
    !is.na(total_pop)
  )

# =========================
# MAP 1: Bivariate choropleth
# Sales per capita × Population
# =========================
set.seed(1)

dat_bi1 <- dat_core %>%
  mutate(
    total_pop_j = jitter(total_pop, factor = 1e-6),
    sales_pc_j  = jitter(sales_pc,  factor = 1e-6)
  ) %>%
  bi_class(
    x = total_pop_j,
    y = sales_pc_j,
    style = "quantile",
    dim = 3
  )

p_bi1 <- ggplot(dat_bi1) +
  geom_sf(aes(fill = bi_class), color = NA) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Bivariate: Population × Sales per Capita",
    subtitle = "Gemeinden",
    x = NULL, y = NULL
  )
p_bi1

leg_bi1 <- bi_legend(
  pal = "DkBlue",
  dim = 3,
  xlab = "Higher population →",
  ylab = "Higher sales per capita →",
  size = 8
)

p_bi1 + leg_bi1

# =========================
# MAP 2: Bivariate choropleth
# Sales per capita × % 75+
# =========================
set.seed(1)

dat_bi2 <- dat %>%
  filter(!is.na(sales_pc), !is.na(percent_75andUp)) %>%
  mutate(
    sales_pc_j = jitter(sales_pc, factor = 1e-6),
    pct75_j    = jitter(percent_75andUp, factor = 1e-6)
  ) %>%
  bi_class(
    x = pct75_j,
    y = sales_pc_j,
    style = "quantile",
    dim = 3
  )

p_bi2 <- ggplot(dat_bi2) +
  geom_sf(aes(fill = bi_class), color = NA) +
  bi_scale_fill(pal = "BlueOr", dim = 3) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Bivariate: % Age 75+ × Sales per Capita",
    subtitle = "Quantiles (3×3)",
    x = NULL, y = NULL
  )

leg_bi2 <- bi_legend(
  pal = "BlueOr",
  dim = 3,
  xlab = "Higher % 75+ →",
  ylab = "Higher sales per capita →",
  size = 8
)

# =========================
# MAP 3: Residual (over/under-performance) map
# Model expected sales given demographics/housing/economics
# =========================
dat_model <- dat %>%
  # keep only rows with all model vars present
  filter(
    !is.na(address_count),
    !is.na(total_pop),
    !is.na(percent_75andUp),
    !is.na(percent_res_one_dwell),
    !is.na(income),
    !is.na(price_sq_m_num)
  )

# Poisson model for counts with exposure offset (population)
# Interprets coefficients as effects on sales rate per person
m <- glm(
  address_count ~ percent_75andUp + percent_res_one_dwell + income + price_sq_m_num,
  data = st_drop_geometry(dat_model),
  family = poisson(),
  offset = log(total_pop)
)

dat_model <- dat_model %>%
  mutate(
    fitted = fitted(m),
    resid_pearson = residuals(m, type = "pearson")
  )

p_resid <- ggplot(dat_model) +
  geom_sf(aes(fill = resid_pearson), color = NA) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient2(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Over/Under-performance (Residuals)",
    subtitle = "Poisson rate model (offset = log(population)); Pearson residuals",
    fill = "Residual",
    x = NULL, y = NULL
  )

# =========================
# MAP 4: Housing small multiples
# Same fill (sales per 1000), facets by housing structure
# =========================
housing_long <- dat %>%
  mutate(sales_per_1000 = 1000 * sales_pc) %>%
  dplyr::select(g_id, g_name, sales_per_1000,
         percent_1floor, percent_2floor, percent_3to5floor, percent_res_one_dwell,
         geometry) %>%
  pivot_longer(
    cols = c(percent_1floor, percent_2floor, percent_3to5floor, percent_res_one_dwell),
    names_to = "housing_var",
    values_to = "housing_pct"
  ) %>%
  filter(!is.na(sales_per_1000), !is.na(housing_pct))

p_housing <- ggplot(housing_long) +
  geom_sf(aes(fill = sales_per_1000), color = NA) +
  facet_wrap(~ housing_var, ncol = 2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Sales Intensity (per 1,000) Across Housing Structure",
    fill = "Sales/1,000",
    x = NULL, y = NULL
  )

# =========================
# Display outputs
# =========================
# Bivariate Map 1 + legend
(p_bi1 | wrap_elements(full = leg_bi1)) + plot_layout(widths = c(4, 1))

# Bivariate Map 2 + legend
(p_bi2 | wrap_elements(full = leg_bi2)) + plot_layout(widths = c(4, 1))

# Residual map
p_resid

# Housing small multiples
p_housing
