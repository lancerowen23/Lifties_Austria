### Mapping aspects of final dataset of installation counts, demographic features, 
### and housing

#Author: Lance R. Owen

library(readr)
library(sf)
library(dplyr)
library(leaflet)
library(htmltools)
library(RColorBrewer)
options(scipen = 999)


#GeoJSON Import 

#Gemeinden data import
gem_path <- "Desktop/Lifties_Austria/Final_Data_Cleaned/Combined/gemeinden_final.geojson"   
gem_final <- st_read(gem_path, quiet = FALSE)


# ---- Settings ----
field <- "address_count"

# Breaks that isolate 0 and handle the long tail up to 67
breaks <- c(-Inf, 0, 1, 2, 4, 7, 15, 67)

# Labels that match those bins
labels <- c("0", "1", "2", "3–4", "5–7", "8–15", "16–67")

# ---- Create binned factor for mapping ----
gem_plot <- gem_final %>%
  mutate(
    bin = cut(
      x = .data[[field]],
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = TRUE
    ),
    bin = factor(bin, levels = labels)  # keep legend in order
  )

# ---- Plot ----
ggplot(gem_plot) +
  geom_sf(aes(fill = bin), color = "lightgrey", linewidth = 0.05) +
  scale_fill_brewer(
    palette = "Oranges",
    na.value = "grey80",
    drop = FALSE,
    name = "Total Lift Installations"
  ) +
  guides(
    fill = guide_legend(reverse = TRUE)) +
  labs(title = "Lift Installations by Gemeinden", 
       subtitle = "2022-2024") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

## Raw population fields

pop_fields <- names(gem_final)[grepl("^pop", names(gem_final))]

# --- quantile breaks that won't crash if there are ties ---
qbreaks_safe <- function(x, n = 5) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(0, 1))
  qs <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  br <- sort(unique(as.numeric(qs)))
  # if ties collapsed breaks too much, reduce n until breaks are usable
  while (length(br) < 3 && n > 2) {
    n <- n - 1
    qs <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
    br <- sort(unique(as.numeric(qs)))
  }
  # last fallback
  if (length(br) < 2) br <- c(min(x), max(x))
  br
}

# --- build breaks: 0 as its own class if present, then quantiles for positives ---
make_breaks_quantiles <- function(x, n_pos = 5) {
  x_ok <- x[is.finite(x)]
  if (length(x_ok) == 0) return(c(0, 1))
  
  if (any(x_ok == 0) && any(x_ok > 0)) {
    pos <- x_ok[x_ok > 0]
    br_pos <- qbreaks_safe(pos, n = n_pos)
    # (-Inf,0] then (0, br_pos[-1]] ...
    br <- sort(unique(c(-Inf, 0, br_pos[-1])))
    return(br)
  } else {
    return(qbreaks_safe(x_ok, n = n_pos))
  }
}

for (fld in pop_fields) {
  
  br <- make_breaks_quantiles(gem_final[[fld]], n_pos = 5)
  
  gem_plot <- gem_final %>%
    mutate(bin = cut(.data[[fld]], breaks = br, include.lowest = TRUE, right = TRUE))
  
  p <- ggplot(gem_plot) +
    geom_sf(aes(fill = bin), color = "lightgrey", linewidth = 0.05) +
    scale_fill_brewer(palette = "YlGnBu", na.value = "grey80", name = fld, drop = FALSE) +
    labs(title = paste0(fld, " (quantiles)")) +
    guides(
      fill = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  
  print(p)
}

###
options(scipen = 999)

library(dplyr)
library(ggplot2)
library(scales)

pop_fields <- names(gem_final)[grepl("^pop", names(gem_final))]

# --- quantile breaks that won't crash if there are ties ---
qbreaks_safe <- function(x, n = 5) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(0, 1))
  qs <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  br <- sort(unique(as.numeric(qs)))
  
  while (length(br) < 3 && n > 2) {
    n <- n - 1
    qs <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
    br <- sort(unique(as.numeric(qs)))
  }
  
  if (length(br) < 2) br <- c(min(x), max(x))
  br
}

# --- build breaks: 0 as its own class if present ---
make_breaks_quantiles <- function(x, n_pos = 5) {
  x_ok <- x[is.finite(x)]
  if (length(x_ok) == 0) return(c(0, 1))
  
  if (any(x_ok == 0) && any(x_ok > 0)) {
    pos <- x_ok[x_ok > 0]
    br_pos <- qbreaks_safe(pos, n = n_pos)
    br <- sort(unique(c(-Inf, 0, br_pos[-1])))
    return(br)
  } else {
    return(qbreaks_safe(x_ok, n = n_pos))
  }
}

# --- create friendly labels ---
make_number_labels <- function(br) {
  
  lo_vals <- br[-length(br)]
  hi_vals <- br[-1]
  
  lo <- ifelse(
    is.infinite(lo_vals),
    "0",
    comma(round(lo_vals, 0))
  )
  
  hi <- ifelse(
    is.infinite(hi_vals),
    paste0(">", comma(round(lo_vals, 0))),
    comma(round(hi_vals, 0))
  )
  
  labs <- paste0(lo, "–", hi)
  
  # fix upper infinite class
  if (any(is.infinite(hi_vals))) {
    idx <- which(is.infinite(hi_vals))
    labs[idx] <- paste0(">", comma(round(lo_vals[idx], 0)))
  }
  
  labs
}

for (fld in pop_fields) {
  
  br <- make_breaks_quantiles(gem_final[[fld]], n_pos = 5)
  labs <- make_number_labels(br)
  
  gem_plot <- gem_final %>%
    mutate(
      bin = cut(
        .data[[fld]],
        breaks = br,
        labels = labs,
        include.lowest = TRUE,
        right = TRUE
      ),
      bin = factor(bin, levels = labs)
    )
  
  p <- ggplot(gem_plot) +
    geom_sf(aes(fill = bin),
            color = "lightgrey",
            linewidth = 0.05) +
    scale_fill_brewer(
      palette = "PuBuGn",
      na.value = "grey80",
      name = fld,
      drop = FALSE
    ) +
    labs(title = paste0(fld, " (quantiles)")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  print(p)
}
###

## Quick comparison of all raw count fields

abs_fields <- names(gem_final)[grepl("^abs_", names(gem_final))]

# --- quantile breaks that won't crash if there are ties ---
qbreaks_safe <- function(x, n = 5) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(0, 1))
  qs <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  br <- sort(unique(as.numeric(qs)))
  # if ties collapsed breaks too much, reduce n until breaks are usable
  while (length(br) < 3 && n > 2) {
    n <- n - 1
    qs <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
    br <- sort(unique(as.numeric(qs)))
  }
  # last fallback
  if (length(br) < 2) br <- c(min(x), max(x))
  br
}

# --- build breaks: 0 as its own class if present, then quantiles for positives ---
make_breaks_quantiles <- function(x, n_pos = 5) {
  x_ok <- x[is.finite(x)]
  if (length(x_ok) == 0) return(c(0, 1))
  
  if (any(x_ok == 0) && any(x_ok > 0)) {
    pos <- x_ok[x_ok > 0]
    br_pos <- qbreaks_safe(pos, n = n_pos)
    # (-Inf,0] then (0, br_pos[-1]] ...
    br <- sort(unique(c(-Inf, 0, br_pos[-1])))
    return(br)
  } else {
    return(qbreaks_safe(x_ok, n = n_pos))
  }
}

for (fld in abs_fields) {
  
  br <- make_breaks_quantiles(gem_final[[fld]], n_pos = 5)
  
  gem_plot <- gem_final %>%
    mutate(bin = cut(.data[[fld]], breaks = br, include.lowest = TRUE, right = TRUE))
  
  p <- ggplot(gem_plot) +
    geom_sf(aes(fill = bin), color = "lightgrey", linewidth = 0.05) +
    scale_fill_brewer(palette = "PuBuGn", na.value = "grey80", name = fld, drop = FALSE) +
    guides(
      fill = guide_legend(reverse = TRUE)) +
    labs(title = paste0(fld, " (quantiles)")) +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  
  print(p)
}

### fixed

options(scipen = 999)

library(dplyr)
library(ggplot2)
library(scales)

abs_fields <- names(gem_final)[grepl("^abs_", names(gem_final))]

# --- quantile breaks that won't crash if there are ties ---
qbreaks_safe <- function(x, n = 5) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(0, 1))
  qs <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  br <- sort(unique(as.numeric(qs)))
  while (length(br) < 3 && n > 2) {
    n <- n - 1
    qs <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
    br <- sort(unique(as.numeric(qs)))
  }
  if (length(br) < 2) br <- c(min(x), max(x))
  br
}

# --- build breaks: 0 as its own class if present, then quantiles for positives ---
make_breaks_quantiles <- function(x, n_pos = 5) {
  x_ok <- x[is.finite(x)]
  if (length(x_ok) == 0) return(c(0, 1))
  
  if (any(x_ok == 0) && any(x_ok > 0)) {
    pos <- x_ok[x_ok > 0]
    br_pos <- qbreaks_safe(pos, n = n_pos)
    sort(unique(c(-Inf, 0, br_pos[-1])))
  } else {
    qbreaks_safe(x_ok, n = n_pos)
  }
}

# --- friendly numeric labels for breaks (no brackets/parentheses) ---
make_abs_labels <- function(br) {
  lo_vals <- br[-length(br)]
  hi_vals <- br[-1]
  
  lo <- ifelse(is.infinite(lo_vals), "0", comma(round(lo_vals, 0)))
  
  labs <- character(length(lo_vals))
  for (i in seq_along(lo_vals)) {
    if (is.infinite(hi_vals[i])) {
      labs[i] <- paste0(">", comma(round(lo_vals[i], 0)))
    } else {
      labs[i] <- paste0(lo[i], "–", comma(round(hi_vals[i], 0)))
    }
  }
  
  labs
}

for (fld in abs_fields) {
  
  br   <- make_breaks_quantiles(gem_final[[fld]], n_pos = 5)
  labs <- make_abs_labels(br)
  
  gem_plot <- gem_final %>%
    mutate(
      bin = cut(
        .data[[fld]],
        breaks = br,
        labels = labs,
        include.lowest = TRUE,
        right = TRUE
      ),
      bin = factor(bin, levels = labs)  # preserves full set + order
    )
  
  p <- ggplot(gem_plot) +
    geom_sf(aes(fill = bin), color = "lightgrey", linewidth = 0.05) +
    scale_fill_brewer(
      palette = "PuBuGn",
      na.value = "grey80",
      name = fld,
      drop = FALSE
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +  # high values on top
    labs(title = paste0(fld, " (quantiles)")) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  print(p)
}

###

### Percent fields for quick visual comparison

# all percent* fields (anywhere in the name)
pct_fields <- names(gem_final)[grepl("percent", names(gem_final), ignore.case = TRUE)]

# equal-interval breaks (5 bins) with safety for constant values
equal_breaks_5 <- function(x) {
  x <- as.numeric(x)
  x_ok <- x[is.finite(x)]
  if (length(x_ok) == 0) return(c(0, 1))
  
  mn <- min(x_ok)
  mx <- max(x_ok)
  
  # if constant (or nearly), avoid cut() failure
  if (mn == mx) return(c(mn, mx + 1e-6))
  
  seq(mn, mx, length.out = 6)  # 6 breakpoints => 5 categories
}

for (fld in pct_fields) {
  
  br <- equal_breaks_5(gem_final[[fld]])
  
  gem_plot <- gem_final %>%
    mutate(bin = cut(.data[[fld]], breaks = br, include.lowest = TRUE, right = TRUE))
  
  p <- ggplot(gem_plot) +
    geom_sf(aes(fill = bin), color = "lightgrey", linewidth = 0.05) +
    scale_fill_brewer(
      palette = "PuBuGn",
      na.value = "grey80",
      drop = FALSE,
      name = fld
    ) +
    labs(title = "") +
    guides(
      fill = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  print(p)
}

###
options(scipen = 999)

library(dplyr)
library(ggplot2)
library(scales)

# all percent* fields (anywhere in the name)
pct_fields <- names(gem_final)[grepl("percent", names(gem_final), ignore.case = TRUE)]

# equal-interval breaks (5 bins) with safety for constant values
equal_breaks_5 <- function(x) {
  x <- as.numeric(x)
  x_ok <- x[is.finite(x)]
  if (length(x_ok) == 0) return(c(0, 1))
  
  mn <- min(x_ok)
  mx <- max(x_ok)
  
  # if constant (or nearly), avoid cut() failure
  if (mn == mx) return(c(mn, mx + 1e-6))
  
  seq(mn, mx, length.out = 6)  # 6 breakpoints => 5 categories
}

# friendly percent labels (no brackets/parentheses)
make_pct_labels <- function(br) {
  # Heuristic: if values are <= ~1.5, treat as proportions (0–1)
  is_prop <- max(br, na.rm = TRUE) <= 1.5
  
  if (is_prop) {
    lo <- percent(br[-length(br)], accuracy = 1)
    hi <- percent(br[-1],          accuracy = 1)
  } else {
    lo <- paste0(number(br[-length(br)], accuracy = 0.1, big.mark = ","), "%")
    hi <- paste0(number(br[-1],          accuracy = 0.1, big.mark = ","), "%")
  }
  
  paste0(lo, "–", hi)
}

for (fld in pct_fields) {
  
  br <- equal_breaks_5(gem_final[[fld]])
  labs <- make_pct_labels(br)
  
  gem_plot <- gem_final %>%
    mutate(
      bin = cut(
        .data[[fld]],
        breaks = br,
        labels = labs,
        include.lowest = TRUE,
        right = TRUE
      ),
      # keep ordering stable and keep empty bins if any
      bin = factor(bin, levels = labs)
    )
  
  p <- ggplot(gem_plot) +
    geom_sf(aes(fill = bin), color = "lightgrey", linewidth = 0.05) +
    scale_fill_brewer(
      palette = "PuBuGn",
      na.value = "grey80",
      drop = FALSE,
      name = fld
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +   # high on top
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  print(p)
}

###

####
options(scipen = 999)

library(dplyr)
library(ggplot2)
library(scales)

# all percent* fields (anywhere in the name)
pct_fields <- names(gem_final)[grepl("percent", names(gem_final), ignore.case = TRUE)]

# equal-interval breaks (5 bins) with safety for constant values
equal_breaks_5 <- function(x) {
  x <- as.numeric(x)
  x_ok <- x[is.finite(x)]
  if (length(x_ok) == 0) return(c(0, 1))
  
  mn <- min(x_ok)
  mx <- max(x_ok)
  
  # if constant (or nearly), avoid cut() failure
  if (mn == mx) return(c(mn, mx + 1e-6))
  
  seq(mn, mx, length.out = 6)  # 6 breakpoints => 5 categories
}

# format breaks into clean percent-range labels
make_pct_labels <- function(br) {
  # Detect whether data look like proportions (0–1) or already percents (0–100)
  is_prop <- max(br, na.rm = TRUE) <= 1.5
  
  if (is_prop) {
    lo <- percent(br[-length(br)], accuracy = 1)
    hi <- percent(br[-1],          accuracy = 1)
  } else {
    lo <- paste0(number(br[-length(br)], accuracy = 0.1, big.mark = ","), "%")
    hi <- paste0(number(br[-1],          accuracy = 0.1, big.mark = ","), "%")
  }
  
  # Use en dash and avoid brackets/parentheses entirely
  paste0(lo, "–", hi)
}

for (fld in pct_fields) {
  
  br <- equal_breaks_5(gem_final[[fld]])
  labs <- make_pct_labels(br)
  
  gem_plot <- gem_final %>%
    mutate(
      bin = cut(
        .data[[fld]],
        breaks = br,
        labels = labs,
        include.lowest = TRUE,
        right = TRUE
      ),
      # keep all bins even if empty, and control ordering (low->high)
      bin = factor(bin, levels = labs)
    )
  
  p <- ggplot(gem_plot) +
    geom_sf(aes(fill = bin), color = "lightgrey", linewidth = 0.05) +
    scale_fill_brewer(
      palette = "PuBuGn",
      na.value = "grey80",
      drop = FALSE,
      name = fld
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  print(p)
}

####

## Income

# Prevent scientific notation
options(scipen = 999)

# Create quantile breaks
brks <- quantile(
  gem_final$income,
  probs = seq(0, 1, 0.2),
  na.rm = TRUE
)

# Create formatted labels (comma separated)
labs <- paste0(
  comma(round(brks[-length(brks)], 0)),
  " – ",
  comma(round(brks[-1], 0))
)

# Create binned variable
gem_plot <- gem_final %>%
  mutate(
    income_bin = cut(
      income,
      breaks = brks,
      include.lowest = TRUE,
      labels = labs
    ),
    # Reverse levels so highest income appears at top of legend
    income_bin = factor(income_bin, levels = rev(levels(income_bin)))
  )

# Plot
ggplot(gem_plot) +
  geom_sf(aes(fill = income_bin),
          color = "lightgrey",
          linewidth = 0.05) +
  scale_fill_brewer(
    palette = "Blues",
    direction = -1,  # Darker = higher income
    na.value = "grey80",
    name = "Annual Earnings (EUR)"
  ) +
  labs(
    title = "Pensioners' Average Annual Gross Earnings"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# What about installations per 75+ population?

gem_final$install_per_75up <- 1000*(gem_final$address_count/gem_final$pop_75andUp)
gem_final$install_per_60to74 <- 1000*(gem_final$address_count/gem_final$pop_60to74)

options(scipen = 999)

field <- "install_per_75up"

# Tailored breaks based on your distribution
breaks <- c(
  -Inf,   # for safety
  0,
  1,
  3,
  5,
  10,
  25,
  Inf
)

labels <- c(
  "0",
  "1",
  "2–3",
  "4–5",
  "6–10",
  "11–25",
  ">25"
)

gem_plot <- gem_final %>%
  mutate(
    bin = cut(
      .data[[field]],
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = TRUE
    ),
    bin = factor(bin, levels = labels)
  )

ggplot(gem_plot) +
  geom_sf(aes(fill = bin), color = "lightgrey", linewidth = 0.05) +
  scale_fill_brewer(
    palette = "Reds",
    na.value = "grey80",
    drop = FALSE,
    name = "Installations per 1,000"
  ) +
  guides(
    fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Installations per Population 75+",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

### and for 60-74
options(scipen = 999)

field <- "install_per_60to74"

# Tailored breaks based on your distribution
breaks <- c(
  -Inf,   # for safety
  0,
  1,
  3,
  5,
  10,
  25,
  Inf
)

labels <- c(
  "0",
  "1",
  "2–3",
  "4–5",
  "6–10",
  "11–25",
  ">25"
)

gem_plot <- gem_final %>%
  mutate(
    bin = cut(
      .data[[field]],
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = TRUE
    ),
    bin = factor(bin, levels = labels)
  )

ggplot(gem_plot) +
  geom_sf(aes(fill = bin), color = "lightgrey", linewidth = 0.05) +
  scale_fill_brewer(
    palette = "Reds",
    na.value = "grey80",
    drop = FALSE,
    name = "Installations per 1,000"
  ) +
  labs(
    title = "Installations per Population Aged 60-74",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

