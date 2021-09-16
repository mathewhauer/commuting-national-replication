source("./R/SCRIPTS/000-Libraries.R")
library(rvest)


# Generating a list of the DEM files underneath NOAA's SLR viewer
url <- "https://coast.noaa.gov/htdata/raster2/elevation/SLR_viewer_DEM_6230/"
pg <- read_html(url)
links <- html_attr(html_nodes(pg, "a"), "href") 
links[grep("*.tif", links)]
# Dropping the Channel Islands.
DEMslocs <- paste0(url, links[grep("*.tif", links)])[2:80]
# DEM <-DEMslocs[63]
i=33
for(i in 1:length(DEMslocs)){
  # if(!file.exists(filenam)){
  DEM <-DEMslocs[i]
  print(DEM)
filenam <- paste0("./R/DATA-RAW/NOAA_SLR/", 
                  tail(unlist(str_split(DEM, "/")), 1))
if(!file.exists(filenam)){
  download.file(DEM,
                destfile = filenam, method = "curl")
}
}
 
