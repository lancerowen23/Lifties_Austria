### Catboost visualizations and correlation plots

library(readr)
library(dplyr)
library(ggplot2)

# Read CSV
catboost_output <- read_csv("Desktop/Lifties_Austria/Lyfties_Modeling/catboost_output.csv")

# Flag top 5
catboost_plot <- catboost_output %>%
  arrange(desc(Importance)) %>%
  mutate(top5 = row_number() <= 5)

# Redo labels
feature_labels <- c(
  "total_pop" = "Total Population",
  "income" = "Pensioner Income",
  "percent_2floor" = "% Two-Floor Buildings",
  "percent_75andUp" = "% Population 75+",
  "percent_own_occ" = "% of Owner-Occupied Dwellings",
  "percent_1floor" = "% One-Story Buildings",
  "percent_priv_one" = "% of Private Dwellings (One Person)",
  "percent_res_one_dwell" = "% of Dwellings with One Resident",
  "percent_60to74" = "% Population 60-74",
  "percent_households_no_kids" = "% No-Child Households",
  "percent_3to5floor" = "% 3-to-5 Storey Buildings")

ggplot(catboost_plot, aes(
  x = Importance,
  y = reorder(Feature, Importance),
  fill = top5
)) +
  geom_col() +
  scale_fill_manual(
    values = c("TRUE" = "#4C72B0", "FALSE" = "grey80"),
    guide = "none"
  ) +
  geom_text(
    aes(label = round(Importance, 2)),
    hjust = -0.15,
    size = 3.5
  ) +
  scale_y_discrete(labels = function(x) ifelse(x %in% names(feature_labels), feature_labels[x], x)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
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

