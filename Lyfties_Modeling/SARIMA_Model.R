### SARIMA Model

library(dplyr)
library(lubridate)
library(forecast)

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


