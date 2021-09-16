###------TABLE STATES-----
## @knitr statestable-setup
source("./R/SCRIPTS/000-Libraries.R")

fipslist <- read_csv(file="https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_and_county_fips_master.csv", 
                     col_names = TRUE) %>%
  mutate(GEOID = str_pad(fips, 5, pad = "0"),
         STATEID = substr(GEOID,1,2),
         CNTYID = substr(GEOID,3,5)) %>%
  na.omit()

commutingareas <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv") %>%
  mutate(h_state = substr(h_county,1,2))


statelist <- sort(unique(commutingareas$h_county))

costs_statesummary <- data.frame()

for(this.state in missed){
  print(this.state)
  tryCatch(
    {
      # Getting the file list for `this.state`
      zipF <- list.files(path = "./R/DATA-PROCESSED/COMMUTINGCOSTS/", recursive=TRUE, 
                         pattern = paste0("^h",this.state,".*.rds"),  full.names = TRUE)
      
      # Pulling in the state's data and subsetting it to only 2002-2004, 2015-2017, and 2060.
      dat <- data.frame()
      for(this.file in zipF){
        print(this.file)
        dat2 <- readRDS(this.file) %>%
          mutate(period = case_when(
            year %in% c(2002, 2003, 2004) ~ 1,
            year %in% c(2015, 2016, 2017) ~ 2,
            year == 2060 ~ 3
          )) %>%
          filter(period %in% c(1,2,3))
        dat <- rbind(dat, dat2)
        rm(dat2)
        gc(reset=TRUE)
      }
      
      # Pulling in the tide data
      tides <- read_csv("./R/DATA-PROCESSED/tideheightbins_cnties.csv") %>%
        filter(GEOID %in% this.state) %>%
        mutate(period = case_when(
          year %in% c(2002, 2003, 2004) ~ 1,
          year %in% c(2015, 2016, 2017) ~ 2,
          year == 2060 ~ 3
        )) %>%
        filter(period %in% c(1,2,3)) %>%
        group_by(period, scenario, GEOID) %>%
        dplyr::select(period, GEOID, `1`:`5`) %>%
        dplyr::summarise_all(list(mean))
      tides[(is.na(tides))] <- 0
      
      # Subsetting to only the relevating columns
      costs <- dat[,c(7, 10,13, 26,15, 16,25,18:24, 28, 1:6)]
      rm(dat)
      gc(reset=TRUE)
      # Calculating the mean over the two periods.
      costs3 <- costs[, by = c("period", "h_county", "h_blkgrp", "w_blkgrp"),
                      lapply(.SD, mean)] %>%
        dplyr::select(period:avebase, cost_dry:cost_5)
      
      # Commuters are the only different thing. Some periods are missing in the data so we just divide by 3,
      # implicitly assinging 0 to missing years.
      costs_comm <- costs[, by = c("period","h_county", "h_blkgrp", "w_blkgrp"),
                          lapply(.SD, sum)] %>%
        dplyr::select(h_blkgrp, w_blkgrp, period, h_county, commuters) %>%
        mutate(commuters= commuters/3)
      costs3 <- left_join(costs3, costs_comm)
      rm(costs_comm)
      gc(reset=TRUE)
      
      # Getting the unique data and then going wide and then long to create  missing values.
      costs2 <- unique(costs[, c(1,2, 3, 15)]) %>% # these h_blkgrp, w_blkgrp, h_county, and period
        pivot_wider(names_from = period,
                    values_from = period) %>%
        pivot_longer(cols = c(`1`, `2`, `3`),
                     names_to = "period") %>%
        dplyr::select(everything(), -value, GEOID = h_county) %>%
        mutate(period = as.numeric(period)) %>%
        left_join(., tides) %>%
        left_join(., costs3) %>%
        mutate(totdays = `1` + `2` + `3` + `4` +`5`)
      # rm(costs)
      # rm(costs3)
      gc(reset=TRUE)
      costs2$commuters[is.na(costs2$commuters)] <- 0
      
      ##We then fix NA values.
      costs2 <- costs2 %>%
        group_by(h_blkgrp, w_blkgrp) %>%
        mutate(cost_dry = cost_dry[!is.na(cost_dry)][1L],
               cost_1 = cost_1[!is.na(cost_1)][1L],
               cost_2 = cost_2[!is.na(cost_2)][1L],
               cost_3 = cost_3[!is.na(cost_3)][1L],
               cost_4 = cost_4[!is.na(cost_4)][1L],
               cost_5 = cost_5[!is.na(cost_5)][1L]
        )
      
      ## Calculate the totalcost perday.
      ## costs2 <- costs2 %>% # group_by(h_blkgrp, w_blkgrp) %>% fill(cost_dry:cost_5) %>%
        costs2$totalcostperday <- ((costs2$cost_1 - costs2$cost_dry) * costs2$`1` +
                                     (costs2$cost_2 - costs2$cost_dry) * costs2$`2` +
                                     (costs2$cost_3 - costs2$cost_dry) * costs2$`3` +
                                     (costs2$cost_4 - costs2$cost_dry) * costs2$`4` +
                                     (costs2$cost_5 - costs2$cost_dry) * costs2$`5`) /costs2$totdays
      costs2 <- costs2[,c(1:5,19,21)] # h_blkgrp, w_blkgrp, GEOID, period, scenario, commuters, totalcostperday
      gc(reset=TRUE)
      # Making new objects for each period, including 2060.
      perday1 <- costs2[which(costs2$period == 1),] %>%
        dplyr::select(h_blkgrp, w_blkgrp, GEOID, totalcostperday1 = totalcostperday,
                      commuters1 = commuters)
      perday2 <- costs2[which(costs2$period == 2),] %>%
        dplyr::select(h_blkgrp, w_blkgrp, GEOID, totalcostperday2 = totalcostperday,
                      commuters2 = commuters)
      # perday3 <- costs2[which(costs2$period == 3),] %>%
      #   dplyr::select(h_blkgrp, w_blkgrp, GEOID, scenario,totalcostperday3 = totalcostperday,
      #                 commuters3 = commuters)
      
      # joining them back up.
      costs4 <- left_join(costs2, perday1) %>%
        left_join(., perday2) %>%
        filter(period < 3)
      costs5 <- costs4[which(costs4$totalcostperday2>0),]
      toteffect_all <-
        lm(totalcostperday*500 ~ period, weights = commuters, data = costs4)
      
     tideeffect_all <- summary(lm(totalcostperday*500 ~ period, weights = commuters1, data = costs4))
      accomeffect_all <- summary(lm(totalcostperday2*500 ~ period, weights = commuters, data = costs4))
      
      toteffect_imp <- try(summary(
        lm(totalcostperday*500 ~ period, weights = commuters, data = costs5)), silent=T)
      
      tideeffect_imp  <- try(summary(lm(totalcostperday*500 ~ period, weights = commuters1, data = costs5)), silent=T)
      accomeffect_imp <- try(summary(lm(totalcostperday2*500 ~ period, weights = commuters, data = costs5)), silent=T)
      
      statsum <- data.frame("a")
      statsum$FIPS <- this.state
      statsum$commutersall <-  sum(costs4$commuters)
      statsum$commutersimp <-  sum(costs5$commuters)
      statsum$toteffect_all <- toteffect_all$coefficients[2]
      statsum$tideeffect_all <- tideeffect_all$coefficients[2]
      statsum$accomeffect_all <- accomeffect_all$coefficients[2]
      
      statsum$toteffect_imp <- if (inherits(toteffect_imp, "try-error")) 0 else toteffect_imp$coefficients[2]
      statsum$tideeffect_imp <- if (inherits(tideeffect_imp, "try-error")) 0 else tideeffect_imp$coefficients[2]
      statsum$accomeffect_imp <- if (inherits(accomeffect_imp, "try-error")) 0 else accomeffect_imp$coefficients[2]

            costs_statesummary <- rbind(costs_statesummary, statsum)
    rm(toteffect_imp, tideeffect_imp, accomeffect_imp)
      gc(reset=TRUE)
    }
    , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


missed <- setdiff(statelist, costs_statesummary$FIPS)

costs_statesummary2 <- costs_statesummary %>%
  filter(!FIPS %in% c("25005", "25009", "25017", "25021", "25023",
                      "28045", "28047", "28059", 
                      "33015"))

write_csv(costs_statesummary2, "./R/DATA-PROCESSED/RESULTS/summary_accommodation.csv")
costs_statesummary2$per <- costs_statesummary2$accomeffect_imp / (costs_statesummary2$accomeffect_imp + costs_statesummary2$tideeffect_imp)


a<-read_csv("./R/DATA-PROCESSED/RESULTS/summary_accommodation.csv") %>%
  mutate(GEOID = FIPS)

countyshp <- counties <- get_acs("county", variables = "B19013_001",geometry = TRUE)%>%
  left_join(., costs_statesummary2 %>%
              mutate(GEOID = FIPS)) %>%
  filter(!is.na(FIPS))

st_write(countyshp, "./R/DATA-PROCESSED/MapStuff/counties_accom.shp", append=FALSE)

costs_statesummary2 <- costs_statesummary2 %>% 
  mutate(tideeffect_imp = ifelse(is.na(tideeffect_imp), 0, tideeffect_imp),
    per = accomeffect_imp / tideeffect_imp,
         per = case_when(
           tideeffect_imp <0 ~ per*-1,
           tideeffect_imp >=0 ~ per
         )) %>%
  mutate(ranks = rank(accomeffect_imp, "first", na.last="keep")) 

tabletop <- costs_statesummary2 %>%
  top_n(8, ranks)
tablebot <- costs_statesummary2 %>%
  top_n(-8, ranks)

(a<-rbind(tablebot, tabletop) %>%
  dplyr::select(GEOID = FIPS, everything()) %>%
  left_join(., fipslist) %>%
  mutate(name = paste0(name,", ", state),
         per = percent(per)) %>%
  dplyr::select(ranks,name, commutersimp, toteffect_imp, tideeffect_imp, accomeffect_imp, per) %>%
  arrange(ranks) %>%
  mutate(commutersimp = prettyNum(commutersimp, big.mark =",", digits = 1),
         toteffect_imp= prettyNum(toteffect_imp, big.mark =",", digits = 1),
         tideeffect_imp = prettyNum(tideeffect_imp, big.mark =",", digits = 1),
         accomeffect_imp = prettyNum(accomeffect_imp, big.mark =",", digits = 1)) %>%
  dplyr::select(Rank = ranks, everything(),
                Name = name,
                Commuters = commutersimp,
                `Tot Increase` = toteffect_imp,
                `Tide Effect` = tideeffect_imp,
                `Accommodation` = accomeffect_imp,
                `Accomodation %` = per)
)

stargazer(a, summary=FALSE, rownames=FALSE,
          out = "./R/accommodation2.htm")

kbl(a)  %>%
  kable_classic(full_width = F) %>%
  kable_styling(bootstrap_options = "striped") %>%
  save_kable(file = "./R/accommodation2.htm", self_contained = T)

# Convert the accommodation.htm -> pdf in windows.

fig <-image_read_pdf("./R/DATA-RAW/accommodation2.pdf")
image_write(fig, path = "./R/DATA-RAW/accommodation2.svg", format = "svg")
c <- ggdraw() +
  draw_image("./R/DATA-RAW/accommodation2.svg")

map <- ggdraw() +
  draw_image("./MANUSCRIPT/AccomodationMap.svg")


right <- plot_grid(c,a, ncol=1, labels = c("a", "b"))
plot_grid(c, map, ncol=2, labels = c("a", "b"))
ggsave("./R/accomodation.pdf")