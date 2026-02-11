### Catboost visualizations and correlation plots

library(readr)
library(dplyr)
library(ggplot2)

# Read CSV
catboost_output <- read_csv("Desktop/Lifties_Austria/Lyfties_Modeling/catboost_output.csv")

# Plot
ggplot(catboost_output, aes(
  x = Importance,
  y = reorder(Feature, Importance)
)) +
  geom_col(fill = "#4C72B0") +
  labs(
    title = "Top Predictors (CatBoost) by Relative Importance",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

