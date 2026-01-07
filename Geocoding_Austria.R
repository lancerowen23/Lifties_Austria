### Geocoding Addresses for Customer Analysis

library(dplyr)
library(stringr)
library(hereR)

cust_22 <- read.csv('~/Downloads/P22_2.csv')
cust_23 <- read.csv('~/Downloads/P23_2.csv')
cust_24 <- read.csv('~/Downloads/P24_2.csv')

# Cleaning names for geocoding
library(stringr)

library(stringr)

clean_street_at <- function(street) {
  
  street %>%
    str_trim() %>%
    
    # FIX: expand str. inside compound words
    str_replace_all(
      regex("(?<=\\p{L})str\\.(?=\\d|\\s|$)", ignore_case = TRUE),
      "strasse"
    ) %>%
    
    # Optional: normalize other abbreviations
    str_replace_all(regex("(?<=\\p{L})g\\.", ignore_case = TRUE), "gasse") %>%
    str_replace_all(regex("(?<=\\p{L})pl\\.", ignore_case = TRUE), "platz") %>%
    
    # German → ASCII
    str_replace_all("ß", "ss") %>%
    str_replace_all(c(
      "ä" = "ae", "ö" = "oe", "ü" = "ue",
      "Ä" = "Ae", "Ö" = "Oe", "Ü" = "Ue"
    )) %>%
    
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

#apply
cust_22 <- cust_22 %>%
  mutate(
    street_clean = clean_street_at(street)
  )

cust_23 <- cust_23 %>%
  mutate(
    street_clean = clean_street_at(street)
  )

cust_24 <- cust_24 %>%
  mutate(
    street_clean = clean_street_at(street)
  )

#create single string for geocoding
cust_22 <- cust_22 %>%
  mutate(full_address = paste(street_clean, postal_code, city, "Austria"))
cust_23 <- cust_23 %>%
  mutate(full_address = paste(street_clean, postal_code, city, "Austria"))
cust_24 <- cust_24 %>%
  mutate(full_address = paste(street_clean, postal_code, city, "Austria"))


# geocode using HERE

set_key("API_Key")

results_22 <- geocode(cust_22$full_address)
results_23 <- geocode(cust_23$full_address)
results_24 <- geocode(cust_24$full_address)
