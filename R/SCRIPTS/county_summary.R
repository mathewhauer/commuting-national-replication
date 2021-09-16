###------TABLE STATES-----
## @knitr statestable-setup
source("./R/SCRIPTS/000-Libraries.R")

fipslist <- read_csv(file="https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_and_county_fips_master.csv", 
                     col_names = TRUE) %>%
  mutate(GEOID = str_pad(fips, 5, pad = "0"),
         STATEID = substr(GEOID,1,2),
         CNTYID = substr(GEOID,3,5)) %>%
  # dplyr::select(-fips, -name, -GEOID, -CNTYID) %>%
  # mutate(state = ifelse(STATEID == "00", "TOT", state)) %>%
  # unique() %>%
  na.omit()

commutingareas <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv") %>%
  mutate(h_state = substr(h_county,1,2))


statelist <- sort(unique(commutingareas$h_county))

costs_statesummary <- data.frame()

for(this.state in statelist){
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
      costs <- dat[,c(7, 10,13, 28, 26, 16,25,18:24, 1:6)]
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
      costs2 <- unique(costs[, c(1,2, 3,4)]) %>% # these h_blkgrp, w_blkgrp, h_county, and period
        pivot_wider(names_from = period,
                    values_from = period) %>%
        pivot_longer(cols = c(`1`, `2`, `3`),
                     names_to = "period") %>%
        dplyr::select(everything(), -value, GEOID = h_county) %>%
        mutate(period = as.numeric(period)) %>%
        left_join(., tides) %>%
        left_join(., costs3) %>%
        mutate(totdays = `1` + `2` + `3` + `4` +`5`)
      rm(costs)
      rm(costs3)
      gc(reset=TRUE)
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
      # costs2 <- costs2 %>% # group_by(h_blkgrp, w_blkgrp) %>% fill(cost_dry:cost_5) %>%
      costs2$totalcostperday <- ((costs2$cost_1 - costs2$cost_dry) * costs2$`1` +
                                   (costs2$cost_2 - costs2$cost_dry) * costs2$`2` +
                                   (costs2$cost_3 - costs2$cost_dry) * costs2$`3` +
                                   (costs2$cost_4 - costs2$cost_dry) * costs2$`4` +
                                   (costs2$cost_5 - costs2$cost_dry) * costs2$`5`) /costs2$totdays
      costs2 <- costs2[,c(1:5,19,21)] # h_blkgrp, w_blkgrp, GEOID, period, scenario, commuters, totalcostperday
      gc(reset=TRUE)
      # Making new objects for each period, including 2060.
      perday1 <- costs2[which(costs2$period == 1),] %>%
        dplyr::select(h_blkgrp, w_blkgrp, GEOID, scenario, totalcostperday1 = totalcostperday,
                      commuters1 = commuters)
      perday2 <- costs2[which(costs2$period == 2),] %>%
        dplyr::select(h_blkgrp, w_blkgrp, GEOID, scenario, totalcostperday2 = totalcostperday,
                      commuters2 = commuters)
      perday3 <- costs2[which(costs2$period == 3),] %>%
        dplyr::select(h_blkgrp, w_blkgrp, GEOID, scenario,totalcostperday3 = totalcostperday,
                      commuters3 = commuters)
      
      # joining them back up.
      costs4 <- left_join(costs2, perday1) %>%
        left_join(., perday2) %>%
        left_join(., perday3)
      
      # Have to separate out the historic from the projected values.
      # costs_impacted2 <- costs4[which(costs4$totalcostperday2>0),]
      costs_decomp_hist <- costs2[which(costs2$scenario == "historic"),] %>% 
        pivot_wider(names_from = period,
                    values_from = c(commuters, totalcostperday)) %>%
        na.omit()
      costs_decomp_proj <- costs2[which(costs2$scenario != "historic"),] %>% 
        pivot_wider(names_from = scenario,
                    values_from = c(commuters, totalcostperday)) %>%
        na.omit()
      rm(costs2, perday1, perday2, perday3)
      gc(reset=TRUE)
      costs5 <- costs4[which(costs4$scenario=="historic"),]
      # Generating the weighted means based on some regressions.
      (totaleffect1 <-lm(totalcostperday_1*500 ~ 1, weights = commuters_1, data = costs_decomp_hist)$coefficients)
      (totaleffect2 <-lm(totalcostperday_2*500 ~ 1, weights = commuters_2, data = costs_decomp_hist)$coefficients)
      (totaleffectdiff <- lm(totalcostperday*500 ~ period, weights = commuters, data = costs5)$coefficients)
      (tideeffect <- lm(totalcostperday*500 ~ period, weights = commuters1, data = costs5)$coefficients)
      (accomeffect <- lm(totalcostperday2*500 ~ period, weights = commuters, data = costs5)$coefficients)
      com <- sum(costs5$commuters)
      (totaleffect3l <-lm(totalcostperday_low*500 ~ 1, weights = commuters_low, data = costs_decomp_proj)$coefficients)
      (totaleffect3m <-lm(totalcostperday_intermediate*500 ~ 1, weights = commuters_intermediate, data = costs_decomp_proj)$coefficients)
      (totaleffect3h <-lm(totalcostperday_extreme*500 ~ 1, weights = commuters_extreme, data = costs_decomp_proj)$coefficients)
      
      # Storing the results for this.state
      statsum <- data.frame("a")
      statsum$FIPS <- this.state
      statsum$name <- fipslist[which(fipslist$GEOID==this.state),]$name
      statsum$state <- fipslist[which(fipslist$GEOID==this.state),]$state
      statsum$`2002-2004` <- prettyNum(totaleffect1, big.mark = ',', digits = 3)
      statsum$`2015-2017` <- prettyNum(totaleffect2, big.mark = ',', digits = 3)
      statsum$`% Increase` <- percent((totaleffect2 - totaleffect1) / totaleffect1)
      statsum$`2060 Intermediate [low - extreme]` <- paste0(prettyNum(totaleffect3m, big.mark = ',', digits = 3),
                                                            " [", prettyNum(totaleffect3l, big.mark = ',', digits = 3), " - ",
                                                            prettyNum(totaleffect3h, big.mark = ',', digits = 3), "]")
      statsum$accomodation <- accomeffect[2]
      statsum$commuters <- com
      # Joining it back to our final object.
      costs_statesummary <- rbind(costs_statesummary, statsum)
      
      # # Cleaning the workspace
      rm(costs_decomp_hist, costs_decomp_proj)
      gc(reset=TRUE)
    }
    , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


write_csv(costs_statesummary, "./R/DATA-PROCESSED/summary_table_county.csv")