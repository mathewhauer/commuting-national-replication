commutingareas <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv") %>%
  mutate(h_state = substr(h_county,1,2))

coeffs_avebase <- data.frame()
coeffs_tideffect <- data.frame()
countylist <- sort(unique(commutingareas$h_county))
hwpairs_wide <- data.frame()
hwpairs_long <- data.frame()
costs_decompsummary <- data.frame()
for(this.county in countylist){
  
  print(this.county)
  tryCatch(
    {
      # Generating the files listed in the COMMUTINGCOSTS folder

  zipF <- list.files(path = "./R/DATA-PROCESSED/COMMUTINGCOSTS/", recursive=TRUE, 
                     pattern = paste0("^h",this.county,".*.rds"),  full.names = TRUE)
  
  # Reading the files in and then filtering to just 2002-2004 and 2015-2017
dat <- zipF %>%
  map(readRDS) %>% 
  data.table::rbindlist() %>%
  mutate(period = case_when(
    year %in% c(2002, 2003, 2004) ~ 1,
    year %in% c(2015, 2016, 2017) ~ 2
  )) %>%
  filter(period %in% c(1,2))

# Pulling in the tide data
tides <- read_csv("./R/DATA-PROCESSED/tideheightbins_cnties.csv") %>%
  filter(GEOID %in% this.county) %>%
  mutate(period = case_when(
    year %in% c(2002, 2003, 2004) ~ 1,
    year %in% c(2015, 2016, 2017) ~ 2
  )) %>%
  filter(period %in% c(1,2)) %>%
  group_by(period, GEOID) %>%
  dplyr::select(period, GEOID, `1`:`5`) %>%
  dplyr::summarise_all(list(mean)) 
tides[(is.na(tides))] <- 0



# Subsetting to only the relevating columns
costs <- dat[,c(7, 10, 28, 26, 16,25,18:24, 1:6)]

# Calculating the mean over the two periods.
costs3 <- costs[, by = c("period", "h_blkgrp", "w_blkgrp"),
                lapply(.SD, mean)] %>%  
  dplyr::select(period:avebase, cost_dry:cost_5)

# Commuters are the only different thing. Some periods are missing in the data so we just divide by 3,
# implicitly assinging 0 to missing years.
costs_comm <- costs[, by = c("period", "h_blkgrp", "w_blkgrp"),
                    lapply(.SD, sum)] %>%  
  dplyr::select(h_blkgrp, w_blkgrp, period,commuters) %>%
  mutate(commuters= commuters/3)
costs3 <- left_join(costs3, costs_comm)

# Getting the unique data and then going wide and then long to create  missing values.
costs2 <- unique(dat[, c(7,10, 28)]) %>%
  pivot_wider(names_from = period,
              values_from = period) %>%
  pivot_longer(cols = c(`1`, `2`),
               names_to = "period") %>%
  dplyr::select(-value) %>%
  mutate(period = as.numeric(period)) %>%
  left_join(., tides) %>%
  left_join(., costs3) %>%
  mutate(totdays = `1` + `2` + `3` + `4` +`5`)
costs2$commuters[is.na(costs2$commuters)] <- 0

# We then fix NA values.
costs2 <- costs2 %>%
  group_by(h_blkgrp, w_blkgrp) %>% 
  mutate(cost_dry = cost_dry[!is.na(cost_dry)][1L],
         cost_1 = cost_1[!is.na(cost_1)][1L],
         cost_2 = cost_2[!is.na(cost_2)][1L],
         cost_3 = cost_3[!is.na(cost_3)][1L],
         cost_4 = cost_4[!is.na(cost_4)][1L],
         cost_5 = cost_5[!is.na(cost_5)][1L]
  )

# Calculate the totalcost perday.
costs3 <- costs2 %>% # group_by(h_blkgrp, w_blkgrp) %>% fill(cost_dry:cost_5) %>%
  mutate(totalcostperday = ((cost_1 - cost_dry) * `1` +
                              (cost_2 - cost_dry) * `2` +
                              (cost_3 - cost_dry) * `3` +
                              (cost_4 - cost_dry) * `4` +
                              (cost_5 - cost_dry) * `5`) /totdays,
         commuters = commuters
  ) %>%
  dplyr::select(h_blkgrp:GEOID, commuters, totalcostperday, avebase) %>%
  na.omit()

perday1 <- costs3[which(costs3$period == 1),] %>%
  dplyr::select(h_blkgrp, w_blkgrp, GEOID, totalcostperday1 = totalcostperday,
                commuters1 = commuters)
perday2 <- costs3[which(costs3$period == 2),] %>%
  dplyr::select(h_blkgrp, w_blkgrp, GEOID, totalcostperday2 = totalcostperday,
                commuters2 = commuters)

costs4 <- left_join(costs3, perday1) %>%
  left_join(., perday2)

costs5 <- costs4[which(costs4$totalcostperday2>0),]

totaleffect2 <-summary(
  lm(totalcostperday*500 ~ period, weights = commuters, data = costs5))$coefficients
tideeffect <- summary(lm(totalcostperday*500 ~ period, weights = commuters1, data = costs5))
accomeffect <- summary(lm(totalcostperday2*500 ~ period, weights = commuters, data = costs5))

costs_decomp <- costs3 %>% 
  pivot_wider(names_from = period,
              values_from = c(avebase, commuters, totalcostperday)) %>%
  mutate(totaleffect = (commuters_2*totalcostperday_2) - (commuters_1*totalcostperday_1),
         tide_effect = (totalcostperday_2*commuters_1) - (totalcostperday_1*commuters_1),
         pop_effect = (totalcostperday_2*commuters_2) - (totalcostperday_2*commuters_1),
         h_blkgrp = as.character(h_blkgrp),
         w_blkgrp = as.character(w_blkgrp)) %>%
  na.omit()

totaleffect2 <-summary(lm(totalcostperday*500 ~ period, weights = commuters, data = costs3))$coefficients


totaleffect1 <-lm(totalcostperday_1*500 ~ 1, weights = commuters_1, data = costs_decomp)$coefficients
tideeffect2 <- lm(totalcostperday_2 ~ 1, weights = commuters_1, data = costs_decomp)$coefficients
tideeffect1 <- lm(totalcostperday_1 ~ 1, weights = commuters_1, data = costs_decomp)$coefficients
accomeffect2 <-lm(totalcostperday_2 ~ 1, weights = commuters_2, data = costs_decomp)$coefficients
accomeff1 <- lm(totalcostperday_2 ~ 1, weights = commuters_1, data = costs_decomp)$coefficients


(accomeffect2 - accomeff1)*500
(tideeffect2 - tideeffect1)*500
(totaleffect2 - totaleffect1)*500

costs_decompi <- costs_decomp %>%
  group_by(GEOID) %>%
  dplyr::summarise(totaleffect = weighted.mean(totaleffect)*500,
                   tide_effect = mean(tide_effect)*500,
                   accommodation_effect = mean(pop_effect)*500)

costs_decompsummary <- rbind(costs_decompsummary, costs_decompi)

saveRDS(costs_decomp, file = paste0("./R/DATA-PROCESSED/ACCOMODATION/h",this.county,".rds"))


}
 , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

costs_decompsummary <- data.frame()
for(this.county in countylist){
  
  print(this.county)
  tryCatch(
    {
zipF <- list.files(path = "./R/DATA-PROCESSED/ACCOMODATION/", recursive=TRUE, 
                   pattern = paste0("^h",this.county,".*.rds"),  full.names = TRUE)
costs_decomp <- readRDS(zipF)
costs_decompi <- costs_decomp %>%
  group_by(GEOID) %>%
  dplyr::summarise(totaleffect = mean(totaleffect)*500,
                   tide_effect = mean(tide_effect)*500,
                   accommodation_effect = mean(pop_effect)*500)

costs_decompsummary <- rbind(costs_decompsummary, costs_decompi)
    }
, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

costs_decomp <- zipF %>%
  map(readRDS) %>% 
  data.table::rbindlist() 
costs_decompi <- costs_decomp %>%
  group_by(GEOID) %>%
  dplyr::summarise(totaleffect = mean(totaleffect)*500,
                   tide_effect = mean(tide_effect)*500,
                   accommodation_effect = mean(pop_effect)*500) %>%
  mutate(per = accommodation_effect / totaleffect)

write_csv(costs, "./R/DATA-PROCESSED/accomodation_12086.csv")

coeffs_avebase2 <- coeffs_avebase
coeffs_avebase2$h_county = paste0(coeffs_avebase2$h_county, "\t")

write_csv(coeffs_avebase2, "./R/DATA-PROCESSED/accomodation_avebase2.csv")
write_csv(hwpairs, "./R/DATA-PROCESSED/accomodation_hwpairs_all_05042021.csv")
write_xlsx(coeffs_avebase, "./R/DATA-PROCESSED/accomodation_avebase.xlsx")
write_csv(coeffs_tideffect, "./R/DATA-PROCESSED/accomodation_tideeffect.csv")
write_xlsx(coeffs_tideffect, "./R/DATA-PROCESSED/accomodation_tideffect.xlsx")

costs$rat_commuters = (costs$commuters_2 - costs$commuters_1) / costs$commuters_1
costs$diff_commute = costs$totalcostperday_2 - costs$totalcostperday_1
hwpairs$h_county <- substr(hwpairs$h_blkgrp,1,5)
test <- hwpairs[which(hwpairs$h_county == "12086"),]

(hwpairs_reg <- summary(lm(costs$rat_commuters ~ costs$diff_commute * costs$totalcostperday_1)))

summary(lm(h12086$pop_effect ~ h12086$tide_effect))

samp <- randomRows(costs_decomp, 100000)

# hbin <- hexbin(cost_decomp$tide_effect, cost_decomp$pop_effect, xbins=400)
# plot(hbin)
a <- ggplot(samp, aes(x = tide_effect, y = pop_effect)) +
  # geom_hex() +
  geom_point(alpha = 1/100) +
  # scale_x_continuous(trans='log10') +
  geom_smooth(method = "lm") +
  # stat_regline_equation(label.x = 2000, label.y = 3) +
  # stat_cor(label.y = -2000, label.x= 1500) +
  theme_bw() +
  labs(x = "Total Commuting Minutes in 2002-2004",
       y = "Accomodation Effect in 2015-2017",
       title = "All HW pairs with non-zero Delays in 2002-2004",
       caption = prettyNum(paste0("n = ", nrow(costs_decomp)), big.mark = ","))

ggsave("./FIGURES/accomodation.png",a )


coeffs2 <- coeffs %>% #filter(abs(tstat) >=5) %>%
  mutate(GEOID = h_county)

counties <- get_decennial("county",
                                   variables = "H001001",
                                   year = 2010,
                                   sumfile = "sf1",
                                   geometry = TRUE,
                                   shift_geo = TRUE)


counties <- left_join(counties, coeffs2)

ggplot(counties) +
  geom_sf(aes(fill=pop_effect)) +
  scale_fill_viridis(option="cividis") +
  # scale_fill_brewer(palette = "Spectral") +
  theme_void()

ggplot(costs2, aes(x = avebase_1, y = pop_effect)) +
  geom_point() +
  geom_smooth(method = "lm") +
  # stat_regline_equation(label.x = 2000, label.y = 3) +
  # stat_cor(label.y = -2000, label.x= 1500) +
  theme_bw() +
  labs(x = "Total Commuting Minutes in 2002-2004",
       y = "Accomodation Effect in 2015-2017",
       title = "Norfolk VA")

commuters <- h37055 %>%
  dplyr::select(7,10,16, period) %>%
  group_by(period, h_blkgrp, w_blkgrp) %>%
  dplyr::summarise_all(list(mean))%>%
  filter(h_blkgrp == "370559701021",
         w_blkgrp == "370919504021")

days <- h37055 %>%
  dplyr::select(Days1:totdays, period) %>%
  unique() %>%
  group_by(period) %>%
  dplyr::summarise_all(list(mean))

combined <- left_join(costs, commuters) %>%
  left_join(., days)

combined$totalcost <- (combined$cost_1 - combined$cost_dry) * combined$Days1 +
  (combined$cost_2 - combined$cost_dry) * combined$Days2 +
  (combined$cost_3 - combined$cost_dry) * combined$Days3 +
  (combined$cost_4 - combined$cost_dry) * combined$Days4 +
  (combined$cost_5 - combined$cost_dry) * combined$Days5

combined$totalcostperday <- combined$totalcost / combined$totdays

combined$avebase <- (2 * 250 * combined$totalcostperday) * combined$commuters
combined$percommuter <- combined$totalcostperday / combined$commuters