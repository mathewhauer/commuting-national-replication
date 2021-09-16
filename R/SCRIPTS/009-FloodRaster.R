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

files <- paste0("h",sort(DEMCrosswalk$GEOID),".rds")
# Removing the missing counties (01003, 13127, 13179) from the files loop
# files <- setdiff(files, missing)
# this.file <- files[91]
# this.file <- files[1]
# this.file <- files[408]
#"h06083
for(this.file in files){
   # Setting up the workspace
  state = substr(this.file,2,3)
  cnty = substr(this.file,2,6)
  print(cnty)
  DemToGet <- DEMCrosswalk[which(DEMCrosswalk$GEOID == cnty),]$DEMfile
  countyshape <- allcounties[which(allcounties$GEOID == cnty),]
  demname <- paste0("./R/DATA-RAW/DEMs/h", cnty, ".tif")
  if(!file.exists(demname)){
  if(length(DemToGet)>1){
  for(i in 1:length(DemToGet)){
    rl <- crop(raster(DemToGet[i]),countyshape)
  assign(paste0("r",i), rl)
  rm(rl)
  }
  x <- lapply(paste0("r", 1:length(DemToGet)), get)
   dem <- combine_rasters(x, ref_rast=r1)
   dem <- mask(dem, countyshape)
  }else{
    d <- raster(DemToGet)
    dem <- crop(d,countyshape)
    dem <- mask(dem, countyshape)
  }
  writeRaster(dem, paste0("./R/DATA-RAW/DEMs/h", cnty, ".tif"), overwrite=TRUE)
  rm(dem)
  # rm(d)
  # rm(x)
  gc(reset=TRUE)
  removeTmpFiles(0.25)
  }
}
  
  DEMlist <- list.files(
    path = "./R/DATA-RAW/NOAA_SLR/", 
    recursive=TRUE, 
    pattern = paste0("^",fipslist$state_abbr[which(fipslist$GEOID == cnty)]),
    full.names = TRUE)
  DEMlist <-  sort(DEMlist, decreasing = TRUE)
  dem <- raster(this.dem)
  for(this.dem in DemToGet){
    print(this.dem)
    
    # while(!exists("dem")){ 
      dem2 <- crop(raster(this.dem), countyshape)
      if(maxValue(dem2) <0){
        rm(dem2)
      }else{dem <- dem2
      resolution <- unlist(str_split(this.dem, pattern ="_"))
      resolution <- resolution[grepl("[0-9]m",resolution)]
      resolution <- as.numeric(substr(resolution,1,1))}
     }
  

  
 for(i in 0:5){
   print(i)
   d <- dem <= unique(tides[[as.character(paste0("bin",i))]])
   r0 <- clump(d, gaps=FALSE)
   clump9 = data.frame(freq(r0))
   # From NOAA: The value of 40 is based on the use of 10m grid cells; 1 acre = 4046.85m2, 
   # 4046.85 m2/100 m2 = 40.46; this value may change depending on the resolution of the DEM. 
   # We need to get the DEM resolution to calculate this value.
   clump9 = clump9[ ! clump9$count < (4046.85/resolution^2), ]
   clump9 = as.vector(clump9$value)
   r0[!r0 %in% clump9] = NA
   d <- mask(dem, r0)
  writeRaster(d, paste0("./R/DATA-RAW/DEMs/h", cnty,"_",i, ".tif"), overwrite=TRUE)
 }
}
 dem0 <- dem <= unique(tides$bin0)
 dem1 <- dem <= unique(tides$bin1)
 dem2 <- dem <= unique(tides$bin2)
 dem3 <- dem <= unique(tides$bin3)
 dem4 <- dem <= unique(tides$bin4)
 dem5 <- dem <= unique(tides$bin5)
 for(i in 0:5){
   r <- clump(get(paste0("dem",i)), gaps=FALSE)
   assign(paste0("r",i), r)
 }
 r0 <- clump(dem0, gaps=FALSE)
 r1 <- clump(dem1, gaps=FALSE)
 r2 <- clump(dem2, gaps=FALSE)
 r3 <- clump(dem3, gaps=FALSE)
 r4 <- clump(dem4, gaps=FALSE)
 r5 <- clump(dem5, gaps=FALSE)
 }
 clump9 = data.frame(freq(r3))
 clump9 = clump9[ ! clump9$count < 161, ]
 clump9 = as.vector(clump9$value)
 r3[!r3 %in% clump9] = NA
 # freq(r3)
 mapview(r3)
 ned3 <- mask(ned2, r3)
 ned2[r3]
  
  
    } else {
      dem <- merge((cc,readRDS(files[i]))}
  }
    
dem<-   lapply(DEMlist, raster)
crop(dem, countyshape)
  
}





fipslist <- read_csv(file="https://raw.githubusercontent.com/kjhealy/fips-codes/master/county_fips_master.csv", 
                     col_names = TRUE) %>%
  mutate(GEOID = str_pad(fips, 5, pad = "0"),
         STATEID = substr(GEOID,1,2),
         CNTYID = substr(GEOID,3,5)) 

allcounties <-  counties(cb=TRUE, year =2019)

commutingareas <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv") %>%
  mutate(h_state = substr(h_county,1,2))

cntyst <- "53035"
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

# Creating the list to search OSM data over.
cntyarea <- fipslist[which(fipslist$GEOID %in% inarea2),] %>%
  mutate(county_name = case_when(
    fips == "34025" ~ "Monmouth",
    TRUE ~ county_name
  ),
  state_name = case_when(
    fips == "11001" ~ "",
    TRUE ~ state_name
  ),
  filename = paste0("h", GEOID, ".rds"))
cntyarea <- unique(cntyarea[,c(1:5,9,14:17)])

for(this.county in unique(cntyarea$fips)){
  list.files()
  files <- paste0("./R/DATA-RAW/ROADNETWORK/", cntyarea$filename)
  raster("./R/DATA-RAW/NOAA_SLR/WA_SEW2_GCS_5m_NAVDm.tif")
}

area <- allcounties %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP)) %>%
  filter(GEOID %in% commutingarea$w_county)

# load raster in an R object called 'DEM'
DEM <- raster("./R/DATA-RAW/NOAA_SLR/WA_SEW2_GCS_5m_NAVDm.tif")
test <- st_as_sf(allcounties)
a <- st_intersects(test, DEM)
DEM2 <- raster("./R/DATA-RAW/NOAA_SLR/ALFL_A1_GCS_5m_NAVDm.tif")
DEM3 <- merge(DEM, DEM2)
areas <- allcounties$geometry[which(allcounties$GEOID %in% cntyarea$GEOID)]

mapview(DEM) +
  mapview(areas)

DEM <- raster("./R/DATA-RAW/NOAA_SLR/FL_JAX_1_GCS_5m_NAVD88m.tif")

mapview(DEM)
ned2 <- ned[ned<= 2.6]
ned2 <- crop(ned, rdnetwork2)
ned3 <- DEM <= unique(tides$bin5)
r3 <- clump(ned3, gaps=FALSE)
clump9 = data.frame(freq(r3))
clump9 = clump9[ ! clump9$count < 161, ]
clump9 = as.vector(clump9$value)
r3[!r3 %in% clump9] = NA
# freq(r3)
mapview(r3)
ned3 <- mask(ned2, r3)
ned2[r3]


mapview(ed2) +
mapview(rdnetwork2) +
  mapview(odpoints)
ned2 <- as.data.frame(crop(ned, rdnetwork2), xy=TRUE)

