### This script will download the commuting data for our target counties and subset the data to only counties within 100 miles
### of the coastline and with at least 1% of the coastal counties commuters.

source("./R/SCRIPTS/000-Libraries.R")

library(tigris)
library(sf)
library(zip)

# This will download a list of FIPS codes from online.
fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3)) %>%
  dplyr::rename(state = X1,
                STATEID = X2,
                CNTYID = X3,
                NAME = X4) %>%
  filter(!STATEID %in% c("60", "66", "69", "72", "74", "78")) # filtering out the outerlying areas.



## Getting ready to merge the stations with the nearest county.
# Coastline shapefile that excludes the Great Lakes region.
coast <- coastline() %>%
  filter(!NAME == "Great Lakes")

# This is the target coastal counties.
targets<- read.csv("./R/DATA-RAW/countylist.csv") %>%
  mutate(GEOID = as.character(x),
         GEOID = str_pad(GEOID,5, "left", "0"))

# County shapefile that includes only coastal states
coastal_counties <- counties( year = 2019, cb = TRUE) %>%
  filter(GEOID %in% targets$GEOID) %>%
  left_join(fipslist, by = "GEOID")
coastal_counties$state <- tolower(coastal_counties$state)

# Setting up the sequencing for downloading the LEHD-LODES data.
states <- c(sort(unique(coastal_counties$state)),rep(2002:2017))
states <- paste0(sort(unique(coastal_counties$state)), rep(seq(2002,2017), length(unique(coastal_counties$state))))
v<- c("COUNTY","NAME", sort(paste0("P012",LETTERS[1:7], rep(str_pad(seq(3,49), 3, pad = "0"),7))))  

# This function will actually download the LEHD-LODES data.
downloadODdata <- function(i){
    tryCatch({
      print(i)
      # It first creates a directory based on the year.
      dir.create(paste0("./R/DATA-RAW/",substr(i,3,6)))
      
      # First, we check to see if the underlying file exists in the newly created directory. If it doesn't exist, we download
      # the file. This is repeated in two steps for both auxiliary data and the main data.
      if(!file.exists(paste0("./R/DATA-RAW/",substr(i,3,6),"/",substr(i,1,2),"_od_main_JT00_",substr(i,3,6),".csv.gz"))){
        download.file(paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",substr(i,1,2),"/od/",substr(i,1,2),"_od_main_JT00_",substr(i,3,6),".csv.gz"),
                      destfile = paste0("./R/DATA-RAW/",substr(i,3,6),"/",substr(i,1,2),"_od_main_JT00_",substr(i,3,6),".csv.gz"))}
      
      if(!file.exists(paste0("./R/DATA-RAW/",substr(i,3,6),"/",substr(i,1,2),"_od_aux_JT00_",substr(i,3,6),".csv.gz"))){
        download.file(paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",substr(i,1,2),"/od/",substr(i,1,2),"_od_aux_JT00_",substr(i,3,6),".csv.gz"),
                      destfile = paste0("./R/DATA-RAW/",substr(i,3,6),"/",substr(i,1,2),"_od_aux_JT00_",substr(i,3,6),".csv.gz"))}
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# actually running the function above.
parallelStartSocket(detectCores() - 1)
parallelMap(downloadODdata, states[1])
parallelStop()

# With the data downloaded, we still need to format it. This code chunk will actually transform the data.  
datcom <- data.frame()
for(i in 2002:2017){

     tryCatch({
       # First, we get a list of the files for a given year in the year directory.
       zipF <-list.files(path = paste0("./R/DATA-RAW/", i), recursive=TRUE, pattern = "*.gz", full.names = TRUE)
       # We then read all of that data into R in a single tall and skinny dataframe.
       dat <- rbindlist(lapply(zipF, fread, fill = TRUE, sep = ",", integer64 = "character"))
        
       # with our data pulled into R, we need to declare some variables.
        dat$h_county = substr(dat$h_geocode,1,5)     # Identifying the HOME county fips
        dat$w_county = substr(dat$w_geocode,1,5)     # Identifying the WORK county fips
        dat$h_blkgrp = substr(dat$h_geocode,1,12)
        dat$w_blkgrp = substr(dat$w_geocode,1,12)
        dat$year = i
        dat<- dat[which(h_county %in% unique(coastal_counties$GEOID)),]
        dat <- dat     %>%
          group_by(h_county, w_county, year) %>%
          dplyr::summarise(totworkers = sum(S000)) %>%
          ungroup() 
  
        datcom <- rbind(datcom, dat)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }

## We write this data to the harddrive.
# write_csv(datcom, "./R/DATA-PROCESSED/ALL-COMMUTINGAREAS.csv")
datcom <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS.csv")
z <-  unique(coastal_counties$GEOID)

# The data is very large and is not computationally tractable as is. 
# Here, we build a helper function to truncate the data and produce a few summary statistics.
# The truncation is for any county within 100 miles of the coast and containing at least 1% of the commuters.
testdist <- function(x){dat2 <- datcom %>%
  group_by(h_county, w_county) %>%
  mutate(totworkers = as.numeric(totworkers)) %>%
  dplyr::summarise(totworkers = sum(totworkers, na.rm=T)) %>%
  group_by(h_county) %>%
  arrange(-totworkers) %>%
  mutate(csum = cumsum(totworkers), # calculating the cumulative sum of workers and the ratio for each county
       tot = sum(totworkers, na.rm=T),
       per = totworkers/tot) %>%
  filter(per >= x,
         h_county %in% z) %>%
  mutate(n = n(), # this generates the # of counties included
         allper = sum(per)) %>%
  ungroup()
# getting a county shapefile
counties<- get_decennial(geography = "county",
                     # state = this.state,
                     # county = cntyarea$CNTYID[which(cntyarea$state == this.state)],
                     variables = "P001001",
                     geometry=TRUE)
# We have to calculate the great circle distance and convert our county polygons to centroids.
sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

# This next set of code just prepares the data for great circle distance calculation.
o_counties<-counties %>%  filter(GEOID %in% dat2$h_county)
d_counties<-counties %>%  filter(GEOID %in% dat2$w_county)

o_counties<-  sfc_as_cols(o_counties, st_centroid(geometry))
d_counties<- sfc_as_cols(d_counties, st_centroid(geometry))
centroidsd <- st_centroid(d_counties)
z <- left_join(dat2, o_counties, by = c("h_county" = "GEOID"))
pts1 <- z %>% dplyr::select(x,y)
z <- left_join(dat2, d_counties, by = c("w_county" = "GEOID"))

pts2 <- z %>% dplyr::select(x,y) 
# Here we actually calculate the Great Circle Distance between every county.
dist <- geosphere::distHaversine(pts1, pts2)/1000

# We then truncate our data to only counties within 100 miles of the coastal county.
dat2 <- cbind(dat2, dist) %>% filter(as.numeric(dist) < 161) %>%
  group_by(h_county) %>%
  mutate(csum = cumsum(totworkers),
         # tot = sum(totworkers, na.rm=T),
         allper = sum(totworkers)/tot)
}

a<- testdist(0.01)
# write_csv(a, "./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv")