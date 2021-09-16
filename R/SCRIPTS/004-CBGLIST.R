### The data is very large and must be chunked out for processing. 
### This will generate a unique list of CBG pairs.

source("./R/SCRIPTS/000-Libraries.R")

## Getting ready to merge the stations with the nearest county.
targets<- read.csv("./R/DATA-RAW/countylist.csv") %>%
  mutate(GEOID = as.character(x),
         GEOID = str_pad(GEOID,5, "left", "0"))
z <-  unique(targets$GEOID)

## Pulling in the communting areas list. These are withn 100 miles and 1% OF COMMUTERS
datcom <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv")

a <- datcom %>%
  mutate(pairs = paste(h_county, w_county))

datcom <- data.frame(h_county = character(0), 
w_county = character(0), 
h_blkgrp=character(0), 
w_blkgrp=character(0))


for(i in 2002:2017){
  print(i)
  tryCatch({
    zipF <-list.files(path = paste0("./R/DATA-RAW/", i), recursive=TRUE, pattern = "*.gz", full.names = TRUE)
    # Reading in all of the commuting data for a single year into one dataframe.
    dat <- rbindlist(lapply(zipF, fread, fill = TRUE, sep = ",", integer64 = "character"))
    
    # Declaring some variables
    dat$h_county = substr(dat$h_geocode,1,5)     # Identifying the HOME county fips
    dat$w_county = substr(dat$w_geocode,1,5)     # Identifying the WORK county fips
    dat$h_blkgrp = substr(dat$h_geocode,1,12)
    dat$w_blkgrp = substr(dat$w_geocode,1,12)
    dat$pairs = paste(dat$h_county, dat$w_county)
    dat <- dat[,c(14:18)]
    ## Generating the unqiue list of O-D pairs.
    dat <- unique(dat[which(dat$pairs %in% a$pairs),c(-5)])
    dat <- setdiff(dat, datcom)
    
    datcom <- rbind(datcom, dat)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Writing it to the hard drive. This is the "masterfile" containing all information.
# write.csv(datcom, file=gzfile("./R/DATA-PROCESSED/CBGLIST-ALL.csv.gz"))

commutingareas <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv") %>%
  mutate(h_state = substr(h_county,1,2))


#### The following file is generated from the CommuterFlows.R script.
cbgpairs <- fread("./R/DATA-PROCESSED/ALLCOMMUTERS.csv.gz", fill = TRUE, sep = ",", integer64 = "character"
)
cbgpairs$h_county = substr(cbgpairs$h_blkgrp,1,5)
cbgpairs$w_county = substr(cbgpairs$w_blkgrp,1,5)

## The data is very large and must be chunked out for computational tractability. This function just filters the data for each
## county and saves its own data as an RDS file.
parsecommuters <- function(cntyst){
  print(cntyst)
h <- cbgpairs[which(cbgpairs$h_county == cntyst),]
h$pairs <- paste(h$h_blkgrp, h$w_blkgrp)
saveRDS(h, paste0("./R/DATA-PROCESSED/COMMUTERS/h", cntyst,".RDS"), compress=FALSE)
}

l <- unique(sort(commutingareas$h_county))
  lapply(l, parsecommuters)