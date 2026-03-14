### SARIMA Model

library(dplyr)
library(lubridate)
library(forecast)
library(patchwork)
library(ggplot2)
library(zoo)


### Creating monthly installs table from geocoded results (CONFIDENTIAL)
## RESULT TABLE ADDED TO GITHUB AND REFERENCED IN LINE 30.
# df <- read.csv('Desktop/final_geocode_results_Feb7.csv')
# 
# # drop NAs
# df <- df %>%
#   filter(!is.na(date))
# 
# # aggregate address sales by month
# df_monthly <- df %>%
#   mutate(
#     year_month = floor_date(date, unit = "month")
#   ) %>%
#   group_by(year_month) %>%
#   summarise(
#     sales = n(),
#     .groups = "drop"
#   ) %>%
#   arrange(year_month)
# 
# # write df_monthly
# write.csv(df_monthly, 'Desktop/final_monthly_sales.csv')

df_monthly <- read.csv('Desktop/Lifties_Austria/Final_Data_Cleaned/Customers/final_monthly_sales.csv')

# create time series object
sales_ts <- ts(
  df_monthly$sales,
  start = c(
    year(min(df_monthly$year_month)),
    month(min(df_monthly$year_month))
  ),
  frequency = 12
)

plot(sales_ts, main = "Monthly Address Sales (Austria)")

# Nicer looking plot

# Ensure year_month is Date (adjust format if needed)
df_monthly$year_month <- as.Date(df_monthly$year_month)

# If it's stored like "2023-01" use instead:
# df_monthly$year_month <- ym(df_monthly$year_month)

# Plot
ggplot(df_monthly, aes(x = year_month, y = sales)) +
  geom_line(size = .5) +
  geom_point(size = 1) +
  labs(
    title = "Monthly Installations in Austria",
    x = NULL,
    y = "Installations"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(
    limits = c(0, 100),
    expand = c(0, 0)) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.minor = element_blank()
  )

# fit SARIMA and check summary/residuals

sarima_model <- auto.arima(
  sales_ts,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(sarima_model)

checkresiduals(sarima_model)

# Forecast
fc <- forecast(sarima_model, h = 12)

autoplot(fc) +
  labs(
    title = "12-Month Forecast of Address Installs",
    y = "Installations"
  )

# Log transform
sarima_log <- auto.arima(
  log1p(sales_ts),
  seasonal = TRUE
)

forecast(sarima_log, h = 12) %>%
  autoplot()

# force seasonlity just to see
sarima_forced <- Arima(
  sales_ts,
  order = c(2,1,1),
  seasonal = c(0,1,1)  # minimal seasonal structure
)

#compare
AIC(sarima_model, sarima_forced)

#check residuals
checkresiduals(sarima_forced)

#Visuals

fc <- forecast(sarima_model, h = 12)

p_main <- autoplot(fc) +
  labs(
    title = "Monthly Installations: Observed, Fitted, and 12-Month Forecast",
    subtitle = "Model: ARIMA(2,1,1) with drift",
    x = NULL,
    y = "Installations"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Optional: add actual points over the line (often looks sharper in slides)
p_main <- p_main +
  autolayer(sales_ts, series = "Observed", size = 0.6)
  

# ---- Residual ACF (secondary visual)
resid_vec <- residuals(sarima_model)

p_acf <- ggAcf(resid_vec, lag.max = 18) +
  labs(
    title = "Residual ACF",
    subtitle = "No strong remaining autocorrelation",
    x = "Lag (months)",
    y = "ACF"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ---- Combine: big main + small ACF inset-like panel
# Layout: main takes ~3/4 width, ACF takes ~1/4 width
p_combo <- p_main + p_acf + plot_layout(widths = c(3.2, 1))

p_combo


library(cowplot)

p_combo_inset <- ggdraw(p_main) +
  draw_plot(p_acf, x = 0.63, y = 0.08, width = 0.35, height = 0.38)

p_combo_inset

### Create plot with legand for buffers and forecast line
# Convert forecast object to dataframe
fc_df <- as.data.frame(fc)

# Extract historical data
hist_df <- data.frame(
  date = as.Date(time(sales_ts)),
  value = as.numeric(sales_ts)
)

# Forecast dataframe
fc_df$date <- as.Date(time(fc$mean))

ggplot() +
  
  # Historical
  geom_line(data = hist_df,
            aes(x = date, y = value, color = "Observed"),
            linewidth = 1) +
  
  # Forecast interval
  geom_ribbon(data = fc_df,
              aes(x = date,
                  ymin = `Lo 95`,
                  ymax = `Hi 95`,
                  fill = "95% Forecast Interval"),
              alpha = 0.2) +
  
  # Forecast mean
  geom_line(data = fc_df,
            aes(x = date, y = `Point Forecast`,
                color = "Forecast"),
            linewidth = 1.2) +
  
  scale_color_manual(
    name = NULL,
    values = c("Observed" = "black",
               "Forecast" = "blue")
  ) +
  
  scale_fill_manual(
    name = NULL,
    values = c("95% Forecast Interval" = "blue")
  ) +
  
  labs(
    title = "Monthly Installations: Observed and 12-Month Forecast",
    subtitle = "Model: ARIMA(2,1,1) with drift",
    x = NULL,
    y = "Installations"
  ) +
  
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
