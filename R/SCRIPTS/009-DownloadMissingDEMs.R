## This script will:
##  1) Load in the underlying commuting areas
##  2) Find all of the counties associated with the commuting area-- including inbetween areas.
##  3) Download the OSM data and save it to the hard drive.

source("./R/SCRIPTS/000-Libraries.R")
source("./R/SCRIPTS/999-combineRaster.R")
#### Step 1: Loading in the underlying commuting areas.

# This will download a list of FIPS codes from online.
fipslist <- read_csv(file="https://raw.githubusercontent.com/kjhealy/fips-codes/master/county_fips_master.csv", 
                     col_names = TRUE) %>%
  mutate(GEOID = str_pad(fips, 5, pad = "0"),
         STATEID = substr(GEOID,1,2),
         CNTYID = substr(GEOID,3,5)) 

# Downloading a shapefile of all counties
allcounties <-  counties(cb=TRUE, year =2019)

# Loading in the commuting counties that contain at least 1% of all commuters
commutingareas <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv") %>%
  mutate(h_state = substr(h_county,1,2))

# These are all of the counties that contain roads.
files <- list.files("./R/DATA-RAW/ROADNETWORK/")
rdnetworks <- substr(files,2,6)
# Filtering the county list to only include those with roads.
targetcounties <- allcounties[which(allcounties$GEOID %in% rdnetworks),]
# This is the Countylist with a DEM from NOAA.
DEMCrosswalk <- read.csv("./R/DATA-RAW/DEMCrosswalk.csv") %>%
  mutate(GEOID = str_pad(GEOID,5,"left", "0"))
coastalcounties <- unique(commutingareas$h_county)
# These are the counties that don't have DEMs from NOAA
missing <- paste0("h",setdiff(coastalcounties, DEMCrosswalk$GEOID),".rds")

for(this.file in missing){
   # Setting up the workspace
  state = substr(this.file,2,3)
  cnty = substr(this.file,2,6)
  print(cnty)
  countyshape <- allcounties[which(allcounties$GEOID == cnty),]
  folder <- stri_rand_strings(1,10)
  ned <- get_ned(countyshape, res = "13",
                 folder)
  # # Unfortunately the get_ned() function saves the file everytime and must be deleted.
  unlink(paste0("EXTRACTIONS/", folder), recursive = TRUE)
  rl <- crop(ned,countyshape)
  rl <- mask(rl, countyshape)
  writeRaster(rl, paste0("./R/DATA-RAW/NOAA_SLR/h", cnty, ".tif"), overwrite=TRUE)
}  
  