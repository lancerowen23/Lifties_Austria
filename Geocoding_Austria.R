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


# geocode using HERE

set_key(Sys.getenv("HERE_API_KEY"))

results_22 <- geocode(
  address = cust_22$Aufstellort.Adresse,
  city = cust_22$Aufstellort.Ort,
  postalCode = cust_22$Aufstellort.PLZ,
  country = "AUT"
)

results_23 <- geocode(
  address = cust_23$Aufstellort.Adresse,
  city = cust_23$Aufstellort.Ort,
  postalCode = cust_23$Aufstellort.PLZ,
  country = "AUT"
)

results_24 <- geocode(
  address = cust_24$Aufstellort.Adresse,
  city = cust_24$Aufstellort.Ort,
  postalCode = cust_24$Aufstellort.PLZ,
  country = "AUT"
)