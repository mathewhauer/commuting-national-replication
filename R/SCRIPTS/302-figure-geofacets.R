###------FIGURE GEOFACETS-----
## @knitr geofacets

testplot <- read_csv("../R/DATA-PROCESSED/RESULTS/summary_table_county.csv")  %>%
  mutate(STATEID = substr(FIPS,1,2),
  stateyear = paste(STATEID, year, "_")) %>%
  filter(!stateyear %in% c(paste(c("25", "33"), rep(seq(2002,2010,1),2), "_")),
         !stateyear %in% c("28 2002 _", "28 2003 _")) %>%
  group_by(STATEID, year, scenario) %>%
  do(fitHour = tidy(lm(cost ~ 1, weights = commuters, data = .))) %>% 
  unnest(fitHour)  %>%
  pivot_wider(names_from = scenario,
              values_from = estimate) %>%
  mutate(low = case_when(
    year< 2050 ~ historic,
    year > 2050 ~ low
  ),
  extreme = case_when(
    year< 2050 ~ historic,
    year > 2050 ~ extreme
  ),
  intermediate = case_when(
    year< 2050 ~ historic,
    year > 2050 ~ intermediate
  ),
  code = STATEID
  ) %>%
  ungroup()

testplot2 <- testplot %>% filter(year <= 2017)
USgrid <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7),
  col = c(8, 7, 1, 8, 1, 7, 6, 6, 2, 5, 7, 6, 5, 5, 4, 2, 3, 4, 2, 5, 1),
  code = c("23", "33", "53", "25", "41", "44", "36", "34", "06", "10", "09", "24", "51", "37", "45", "28", "01", "13", "22", "12", "48"),
  name = c("ME", "NH", "WA", "MA", "OR", "RI", "NY", "NJ", "CA", "DE", "CT", "MD", "VA", "NC", "SC", "MS", "AL", "GA", "LA", "FL", "TX"),
  stringsAsFactors = FALSE
)

historic<-
  ggplot(testplot2, aes(x = year, y =historic)) +
  geom_smooth(se=T, size = 0.5) + 
  geom_point(size=0.5) +
  theme_bw() +
  coord_cartesian(xlim = c(2002, 2017), ylim = c(0, NA)) +
  labs(y = "Total Annual Travel Cost per Commuter (minutes)",
       x= "Year") +
  theme(
    strip.text.x = element_text(margin = margin(.00, 0, .01, 0, "cm"),
                                size = 5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =6),
    axis.text.y = element_text(size=6)
  ) +
  facet_geo(~code, grid = USgrid, label = "name",
            scales = "free_y"
  ) +
  NULL
historic