# This script will generate a target county list.

source("./R/SCRIPTS/000-Libraries.R")
library(mapview)
## Getting ready to merge the stations with the nearest county.
# Coastline shapefile that excludes the Great Lakes region.
coast <- coastline() %>%
  filter(!NAME == "Great Lakes")
# County shapefile that includes only coastal states
coastal_counties <- counties(cb=TRUE) %>%
  filter(STATEFP %in% c("01", "06", "09", "10", "12", "13", "22", "23",
                        "24", "25", "28", "33", "34", "36", "37", "41", 
                        "42", "44", "45", "48", "51", "53"))

# Finding the counties that intersect with the coastline
z<-st_intersection(coastal_counties, coast)
z <-  c(unique(z$GEOID),
        "06055", # Napa county CA
        "06095", # Solano CA
        "06013", # Contra Costa CA
        "06001", # Alameda CA
        "06085", # Santa Clara CA
        "48409", # San Patricio TX
        "48391", # Refugio TX
        "37049", # Craven NC
        "37137",
        "37013",
        "37177",
        "37187",
        "37015",
        "37041",
        "37143",
        "37139",
        "37053",
        "37029",
        "51550",
        "51740",
        "51800",
        "51700",
        "51093",
        "36061",
        "34017",
        "34003",
        "44003",
        "44001",
        "44007",
        "25017")

countys <- data.frame(GEOID = z)

write.csv(z, "./R/DATA-RAW/countylist.csv")
# mapview(countys)