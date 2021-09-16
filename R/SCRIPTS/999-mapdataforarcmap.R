
dotplot <- read_csv("./R/DATA-PROCESSED/RESULTS/summary_table_county.csv")

period1 <- dotplot %>%
  filter(year %in% c(2015,2016,2017)) %>%
  group_by(FIPS, scenario) %>%
  do(fitHour = tidy(lm(cost ~ 1, weights = commuters, data = .))) %>% 
  unnest(fitHour)  %>%
  dplyr::select(GEOID=FIPS, "x20152017" = estimate) 

period2 <- dotplot %>%
  filter(year %in% c(2060) & scenario == "intermediate") %>%
  group_by(FIPS, scenario) %>%
  do(fitHour = tidy(lm(cost ~ 1, weights = commuters, data = .))) %>% 
  unnest(fitHour)  %>%
  dplyr::select(GEOID=FIPS, "Intermediate" = estimate) 

mapdat <- left_join(period1, period2)

countyshp <- counties() %>%
  left_join(., mapdat)

counties <- get_acs("county", variables = "B19013_001",geometry = TRUE)
states <- get_acs("state", variables = "B19013_001",geometry = TRUE) %>%
  left_join(., fipslist) %>%
  dplyr::select(-name, -CNTYID, -fips) %>%
  unique() %>%
  na.omit()

counties2 <- left_join(counties, mapdat) %>%
  filter(!is.na(Intermediate))

st_write(counties2, "./R/DATA-PROCESSED/MapStuff/counties_delays.shp", append=FALSE)