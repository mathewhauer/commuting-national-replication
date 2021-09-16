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
        dat2 <- readRDS(this.file)
          # mutate(period = case_when(
          #   year %in% c(2002, 2003, 2004) ~ 1,
          #   year %in% c(2015, 2016, 2017) ~ 2,
          #   year == 2060 ~ 3
          # )) %>%
          # filter(period %in% c(1,2,3))
        dat <- rbind(dat, dat2)
        rm(dat2)
        gc(reset=TRUE)
      }
      
     
      costs_collect <- data.frame()
      for(this.year in 2002:2017){
        
        costs_decomp_hist <- dat[which(dat$year == this.year),]
        statsum <- data.frame("a")
        statsum$FIPS <- this.state
        statsum$year = this.year
        statsum$scenario = "historic"
        statsum$commuters <-  sum(costs_decomp_hist$commuters)
        statsum$cost <- if(nrow(costs_decomp_hist)>0){lm(totalcostperday*500 ~ 1, weights = commuters, data = costs_decomp_hist)$coefficients} else {NA}

        costs_collect <- rbind(costs_collect, statsum)
      }
      for(this.scenario in c("low", "intermediate", "extreme")){
        
        costs_decomp_hist <- dat[which(dat$scenario == this.scenario),]
        statsum <- data.frame("a")
        statsum$FIPS <- this.state
        statsum$year = 2060
        statsum$scenario = this.scenario
        statsum$commuters <-  sum(costs_decomp_hist$commuters)
        statsum$cost <- if(nrow(costs_decomp_hist)>0){lm(totalcostperday*500 ~ 1, weights = commuters, data = costs_decomp_hist)$coefficients} else {NA}
        costs_collect <- rbind(costs_collect, statsum)
      }
      costs_statesummary <- rbind(costs_statesummary, costs_collect)
      
      gc(reset=TRUE)
    }
    , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

a <- costs_statesummary %>% filter(year %in% c(2002,2003,2004))
b <- costs_statesummary %>% filter(year %in% c(2015,2016,2017))
lm(cost ~ 1, weights = commuters, data = a)$coefficients
lm(cost ~ 1, weights = commuters, data = b)$coefficients


write_csv(costs_statesummary, "./R/DATA-PROCESSED/RESULTS/summary_table_county.csv")