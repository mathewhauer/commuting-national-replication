### The commuting data is very large and we don't want to need all of the commuting data -- only those with our target
### CBG pairs. This script will generate the raw numbers of commuters between our CBG pairs.

source("./R/SCRIPTS/000-Libraries.R")

datcom <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv")

a <- datcom %>%
  mutate(pairs = paste(h_county, w_county))

states <- unique(substr(a$h_county,1,2))
# This will download a list of FIPS codes from online.
fipslist <- read_csv(file="https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_and_county_fips_master.csv", 
                     col_names = TRUE) %>%
  mutate(GEOID = str_pad(fips, 5, pad = "0"),
         STATEID = substr(GEOID,1,2),
         CNTYID = substr(GEOID,3,5)) %>%
  filter(STATEID %in% states)
states <- na.omit(tolower(unique(fipslist$state)))

# Reading in the unique CBG pair list.
cbgpairs <- fread("./R/DATA-PROCESSED/CBGLIST-ALL.csv.gz", fill = TRUE, sep = ",", integer64 = "character")
  # cbgpairs$pairs = paste(cbgpairs$h_blkgrp, cbgpairs$w_blkgrp)
cbgpairs$h_county = str_pad(as.character(cbgpairs$h_county),5,pad = "0")
cbgpairs$w_county = str_pad(as.character(cbgpairs$w_county),5,pad = "0")



# datcom <- data.frame()
for(i in 2002:2017){
  
  tryCatch({

    zipF <-list.files(path = paste0("./R/DATA-RAW/", i), recursive=TRUE, pattern = "*.gz", full.names = TRUE)
    
    dat <- rbindlist(lapply(zipF, fread, fill = TRUE, sep = ",", integer64 = "character", select= c(1:3)))
    
    dat$h_county = substr(dat$h_geocode,1,5)     # Identifying the HOME county fips
    dat$w_county = substr(dat$w_geocode,1,5)     # Identifying the WORK county fips
    dat$pairs = paste(dat$h_county, dat$w_county)
    
    dat <- unique(dat[which(dat$pairs %in% a$pairs),])
    dat$h_blkgrp = substr(dat$h_geocode,1,12)
    dat$w_blkgrp = substr(dat$w_geocode,1,12)
    dat<-dat[, sum(S000),by=list(h_blkgrp, w_blkgrp)]
   
    cbgpairs[dat, on = c("h_blkgrp", "w_blkgrp"), paste0("_",i):= i.V1]

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

write.csv(cbgpairs[,4:21], file=gzfile(paste0("./R/DATA-PROCESSED/ALLCOMMUTERS.csv.gz")))

commutingareas <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv") %>%
  mutate(h_state = substr(h_county,1,2))


cbgpairs <- fread("./R/DATA-PROCESSED/ALLCOMMUTERS.csv.gz", fill = TRUE, sep = ",", integer64 = "character"
                  # ,
                  # select=c(2,3)
)
cbgpairs$h_county = substr(cbgpairs$h_blkgrp,1,5)
cbgpairs$w_county = substr(cbgpairs$w_blkgrp,1,5)

parsecommuters <- function(cntyst){
  print(cntyst)
  h <- cbgpairs[which(cbgpairs$h_county == cntyst),]
  h$pairs <- paste(h$h_blkgrp, h$w_blkgrp)
  saveRDS(h, paste0("./R/DATA-PROCESSED/COMMUTERS/h", cntyst,".RDS"), compress=FALSE)
}

l <- unique(sort(commutingareas$h_county))
lapply(l, parsecommuters)