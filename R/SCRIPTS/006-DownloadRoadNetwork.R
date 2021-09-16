## This script will:
##  1) Load in the underlying commuting areas
##  2) Find all of the counties associated with the commuting area-- including inbetween areas.
##  3) Download the OSM data and save it to the hard drive.

source("./R/SCRIPTS/000-Libraries.R")

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

# This is the function that will loop through target counties and download the OSM data.
GetRoadNetwork <- function(cntyst){
  
  ## Step 2: Finding all of the counties associated with the commuting area-- including inbetween areas.

  # Filtering only the target county: `cntyst`
  commutingarea <- commutingareas %>%
      filter(h_county == cntyst) 

  area <- allcounties %>%
    mutate(GEOID = paste0(STATEFP, COUNTYFP)) %>%
    filter(GEOID %in% commutingarea$w_county)

## Need to find all counties along the pathways
centroids <- st_centroid(area)
# Getting the coords of the centroids
c2 <-do.call(rbind, st_geometry(centroids)) %>% 
  # tibble::rownames_to_column(., "id")
  as_tibble(rownames=NA) %>% setNames(c("x_","y_")) %>%
  mutate(x2 = x_[which(grepl(cntyst, area$GEOID))],
         y2 = y_[which(grepl(cntyst, area$GEOID))])
st_segment = function(r){st_linestring(t(matrix(unlist(r), 2, 2)))}

# Converting the XYs to a line string then setting in the same CRS as the counties.
c2$geometry = st_sfc(sapply(1:nrow(c2), 
                            function(i){st_segment(c2[i,])},simplify=FALSE))
c2 <- st_sf(c2,
            crs = crs(allcounties))

# Finding the counties that intersect the lines.
inarea <- st_intersects(c2, allcounties)
inarea2 <- unique(allcounties[unlist(inarea),]$GEOID)
area2 <- area %>% filter(GEOID %in% centroids$GEOID)

appendareas <- function(cnty){
  case_when(
    cnty == "24003" ~ list(c("51510", "51600", "51610")),
    cnty %in% c("24005", "24019", "24035", "24510") ~ list(c("11001")),
    cnty == "24009" ~  list(c("24035", "51013", "51510", "51600", "51610")),
    cnty == "24015" ~list(c("11001", "24035", "24011", "24041")),
    cnty == "24017" ~ list(c("51600")),
    cnty == "24025" ~ list(c("11001", "24029", "24035", "24041")),
    cnty == "24029" ~ list(c("11001", "24011")),
    cnty == "24037" ~ list(c("51013", "51510", "51600", "51610")),
    cnty == c("24039", "24047") ~ list(c("10001", "24011")),
    cnty == "24041" ~ list(c("11001", "10001")),
    cnty == "24045" ~ list(c("10001")),
    cnty == "51001" ~ list(c("24017", "51033", "51036", "51057", "51073", "51095", "51099", "51159", "51193",
                             "51735", "51740", "51830")),
    cnty == "51057" ~ list(c("11001", "51013", "51036", "51127", "51510", "51600", "51610", "51683", "51685", "51830")),
    cnty == "51059" ~ list(c("51610", "51683", "51685")),
    cnty == "51073" ~ list(c("51097", "51101")),
    cnty == "51093" ~ list(c("51127", "51830")),
    cnty == "51099" ~ list(c("11001", "51600", "51610", "51683")),
    cnty %in% c("51103", "51133") ~ list(c("11001", "51013", "51036", "51095", "51510", "51600", "51610", "51683",
                             "51685", "51830")),
    cnty == "51119" ~ list(c("11001", "51013", "51510", "51600", "51610", "51683", "51685")),
    cnty == "51131" ~ list(c("51830")),
    cnty == "51153" ~ list(c("11001", "51013")),
    cnty == "51159" ~ list(c("11001", "51013", "51510", "51600", "51610", "51630", "51683", "51685")),
    cnty == "51179" ~ list(c("11001", "51099", "51600", "51610", "51685")),
    cnty == "51193" ~ list(c("11001", "51095", "51127", "51600", "51610", "51683", "51685", "51830")),
    cnty == c("51550", "51740") ~ list(c("51095", "51127", "51199", "51830")),
    cnty == c("51650", "51700") ~ list(c("51097", "51101", "51127")),
    cnty == "51710" ~ list(c("51127", "51199", "51830")),
    cnty == "51735" ~ list(c("51097", "51101", "51127", "51570", "51670", "51730")),
    cnty == "51800" ~ list(c("51095", "51127", "51199", "51570", "51670", "51730", "51830")),
    cnty == "51810" ~ list(c("51127", "51199", "51800", "51830")),
    )
}

inarea2 <- c(inarea2, unlist(appendareas(cntyst)))


# Creating the list to search OSM data over.
cntyarea <- fipslist[which(fipslist$GEOID %in% inarea2),] %>%
  mutate(county_name = case_when(
    fips == "34025" ~ "Monmouth",
    TRUE ~ county_name
  ),
  state_name = case_when(
    fips == "11001" ~ "",
    TRUE ~ state_name
  ))
cntyarea <- unique(cntyarea[,c(1:5,9,14:16)])

# These are the primary road types in the OSM data. Only `service` roads are excluded.
rdtypes <- c(
  "motorway",
  "motorway_link",
  "residential",
  "tertiary",
  "tertiary_link",
  "secondary",
  "secondary_link",
  "primary",
  "primary_link",
  "trunk",
  "trunk_link",
  "unclassified"
)


## Step 3: Downloading and saving the OSM data.
for(i in 1:nrow(cntyarea)){
    print(paste(cntyarea$GEOID[i]))
  filenam <- paste0("./R/DATA-RAW/ROADNETWORK/h", 
                    cntyarea$GEOID[i], 
                    ".rds")
  if(!file.exists(filenam)){
    for(j in 1:length(rdtypes)){
      print(paste(cntyarea$county_name[i], ", ", cntyarea$state_name[i], rdtypes[j]))
    a<- osmdata::opq(paste0(cntyarea$county_name[i], ", ", cntyarea$state_name[i]), timeout = 80) %>%
      osmdata::add_osm_feature(key = "highway", value = rdtypes[j]) %>%
      osmdata::osmdata_sf(quiet = TRUE) %>%
      osmdata::osm_poly2line()
    assign(paste0("BA",i), a)
    
    if(!exists("cc")){
      cc <- get(paste0("BA",i))
    } else {
      cc <- c(cc, get(paste0("BA",i)))
    write_rds(cc, filenam, compress = "gz")}}}
 }

}

targetcounties <- sort(unique(commutingareas$h_county))
tictoc::tic()
lapply(targetcounties, GetRoadNetwork)
tictoc::toc()