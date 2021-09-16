###------EXTENDED DATA -----
## @knitr extendeddat

fipslist <- read_csv(file="https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_and_county_fips_master.csv",
                     col_names = TRUE) %>%
  mutate(GEOID = str_pad(fips, 5, pad = "0"),
         STATEID = substr(GEOID,1,2),
         CNTYID = substr(GEOID,3,5)) %>%
  dplyr::select(GEOID, name, state) %>%
  na.omit()

testplot <- a %>%
  filter(!stateyear %in% c(paste(c("25", "28", "33"), rep(1,3), "_"))) %>%
  dplyr::select(-stateyear)

period1 <- a %>%
  filter(year %in% c(2002,2003,2004)) %>%
  group_by(FIPS, scenario) %>%
  do(fitHour = tidy(lm(cost ~ 1, weights = commuters, data = .))) %>% 
  unnest(fitHour)  %>%
  dplyr::select(GEOID=FIPS, "x20022004" = estimate) 

period2 <- a %>%
  filter(year %in% c(2015,2016,2017)) %>%
  group_by(FIPS, scenario) %>%
  do(fitHour = tidy(lm(cost ~ 1, weights = commuters, data = .))) %>% 
  unnest(fitHour)  %>%
  dplyr::select(GEOID=FIPS, "x20152017" = estimate) 

period3 <- a %>%
  filter(year %in% c(2060)) %>%
  group_by(FIPS, scenario) %>%
  do(fitHour = tidy(lm(cost ~ 1, weights = commuters, data = .))) %>% 
  unnest(fitHour)  %>%
  dplyr::select(GEOID=FIPS, scenario, estimate) %>%
  pivot_wider(names_from = scenario,
              values_from = estimate) 

extendeddat <- left_join(period2, period1) %>%
  left_join(., period3) %>%
  mutate(`2002-2004 Rank` = dense_rank(desc(x20022004)),
         `2015-2017 Rank` = dense_rank(desc(x20152017)),
         `2002-2004` = prettyNum(`x20022004`, digits = 3, big.mark = ","),
         `2015-2017` = prettyNum(`x20152017`, digits = 3, big.mark = ","),
         `2060 int Rank` = dense_rank(desc(intermediate)),
         `2060 Intermediate [low - extreme]` = paste0(prettyNum(intermediate, digits = 3, big.mark = ","),
                                                      " [", prettyNum(low, digits = 3, big.mark = ","), " - ",
                                                      prettyNum(extreme, digits = 3, big.mark = ","), "]")
  )%>%
  left_join(., fipslist) %>%
  dplyr::select(FIPS = GEOID, name, state, `2002-2004`, `2015-2017`, `2060 Intermediate [low - extreme]`,
                `2002-2004 Rank`, `2015-2017 Rank`, `2060 int Rank`) %>%
  mutate(`2002-2004` = na_if(`2002-2004`, "NA")) %>%
  arrange(`2015-2017 Rank`) %>%
  mutate(name = str_remove(name, " County"),
         name = str_remove(name, " Parish"))

median_2060int <- median(period3$intermediate)
