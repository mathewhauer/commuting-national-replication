source("./R/SCRIPTS/000-Libraries.R")
library(fst)
library(terra)
# This will download a list of FIPS codes from online.
fipslist <- read_csv(file="https://raw.githubusercontent.com/kjhealy/fips-codes/master/county_fips_master.csv", 
                     col_names = TRUE) %>%
  mutate(GEOID = str_pad(fips, 5, pad = "0"),
         STATEID = substr(GEOID,1,2),
         CNTYID = substr(GEOID,3,5)) 

# 
allcounties <-  counties( year =2018)

commutingareas <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv") %>%
  mutate(h_state = substr(h_county,1,2)) 
  # filter(h_state %in% c("24", "51"))
# 
# urb <- urban_areas(cb=TRUE) %>%
#   filter()

shapeslist <- list.files( paste0("./R/DATA-RAW/LEVEES/LEVEES"), pattern = ".shp",recursive=TRUE,  full.names = TRUE)
levees <- rbindlist(lapply(shapeslist, function(x) st_read(x)[,"areaSqua", "geometry"])) %>%
  st_sf(.) %>%
  filter(!is.na(st_is_valid(.))) %>%
  filter(areaSqua >0) %>%
  st_transform(., crs(allcounties))

DEMCrosswalk <- read.csv("./R/DATA-RAW/DEMCrosswalk.csv") %>%
  mutate(GEOID = str_pad(GEOID,5,"left", "0"))

MakeRdsElevation <- function(cntyst){
  print(cntyst)
  #   cntyst <- "23029"
  #   cntyst <- "01003"
  #   cntyst <- "24009"
  # Generating the HW counties
  if(!file.exists(paste0("./R/DATA-PROCESSED/GRAPHS/h", cntyst,".fst"))){
    # print(cntyst)
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
        cnty == "06081" ~ list(c("06075", "06095", "06113")),
        cnty == "06087" ~ list(c("06075")),
        # cnty == "22045" ~ list(c("22089")),
        # cnty == "22109" ~ list(c("22095")),
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
        cnty == "37055" ~ list(c("37029", "51550", "51710")),
        cnty == "48057" ~ list(c("48391")),
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
        cnty == "53057" ~ list(c("53053")),
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
      ),
      filename = paste0("h", GEOID, ".rds"))
    cntyarea <- unique(cntyarea[,c(1:5,9,14:17)])
    ## These are helper functions to check.
    # plot(allcounties$geometry[which(allcounties$GEOID %in% cntyarea$GEOID)], col = "blue")
    # 
    # plot(area$geometry, add=TRUE, col = "white")
    # plot(c2$geometry, add = TRUE, col = "red")
    
    print("Getting Road Network")
    files <- paste0("./R/DATA-RAW/ROADNETWORK/", cntyarea$filename)
    for(i in 1:nrow(cntyarea)){
      print(files[i])
      if(!exists("cc")){
        cc <- readRDS(files[i])
      } else {
        cc <- c(cc,readRDS(files[i]))}
    }
    
    a <- cc$osm_lines
    rm(cc)
    gc(reset=TRUE)
    a <- st_transform(a, st_crs(allcounties))
    bridges <- data.frame(osm_id = a$osm_id[which(a$bridge== "yes")],
                          bridge = "yes")
    if(length(a$osm_id[which(a$tunnel== "yes")])==0){
      tunnels <- data.frame(osm_id = NA,
                            tunnel = "yes")
    } else{
      tunnels <- data.frame(osm_id = a$osm_id[which(a$tunnel== "yes")],
                            tunnel = "yes")
    }
    
    print("Getting Elevation Data")
    DemToGet <- unique(DEMCrosswalk[which(DEMCrosswalk$GEOID %in% cntyarea$GEOID),]$DEMfile)
    for(i in 1:length(DemToGet)){
      # rl <- raster(DemToGet[i])
      rl <- rast(DemToGet[i])
      assign(paste0("r",i), rl)
      rm(rl)
    }
    
    # folder <- stri_rand_strings(1,10)
    # ned <- get_ned(a, #res = "13",
    #                folder)
    # # Unfortunately the get_ned() function saves the file everytime and must be deleted.
    # unlink(paste0("EXTRACTIONS/", folder), recursive = TRUE)
    
    # Making the road network
    print("Weighting Street Network")
    graph2 <- read.fst(paste0("./R/DATA-PROCESSED/GRAPHS/h", cntyst, ".fst"))
    graph_connected <- graph2[graph2$component == 1, ]  
    
    pointCoords <- data.table() # setting up a blank datatable to hold the xy's for the nodes
    pointCoords$osm_id <- graph_connected$from_id # getting the ID for the 'from'
    pointCoords$lon <- graph_connected$from_lon
    pointCoords$lat <- graph_connected$from_lat
    
    pointCoords2 <- data.table() #setting up a blank datatable to hold the xy's
    # pointCoords2$osm_id <- graph_connected$to_id
    # pointCoords2$lon <- graph_connected$to_lon
    # pointCoords2$lat <- graph_connected$to_lat
    # Georeferencing the xy's of the nodes
    # Georeferencing the xy's of the nodes
    pointCoords <- st_as_sf(unique(rbind(pointCoords, pointCoords2)), 
                            coords = c("lon", "lat"), 
                            crs = st_crs(allcounties))
    p <- pointCoords
    # finding the points that are behind levees.
    inlevees <- st_intersects(pointCoords, levees)
    inlevees = lengths(inlevees) > 0
    inlevees <- as.data.table(cbind(pointCoords, inlevees))[,1:2]
    inleveeso <-inlevees %>% dplyr::rename(inleveeso = inlevees)
    inleveesd <-inlevees %>% dplyr::rename(inleveesd = inlevees)
    pointCoords<- do.call(rbind, st_geometry(pointCoords)) %>%
      as_tibble(rownames=NA) %>% setNames(c("x_","y_")) %>% cbind(osm_id = pointCoords$osm_id, .)
    
    # Getting the elevation data associated with each node
    print("Getting Elevation Data for each node")
    RasVal <- data.frame()
    for(i in 1:length(DemToGet)){
      print(i)
      # p <- as(pointCoords, "Spatial")
      # rl <- velox::velox(r1)
      rl <- as.data.table(cbind(pointCoords,terra::extract(get(paste0("r",i)), pointCoords[,c(2:3)])))
      # rl <- as.data.table(cbind(pointCoords,raster::extract(get(paste0("r",i)), pointCoords[,c(2:3)])))

      rl$z_ <- rl[,5]
      rl <- rl[!is.na(rl$z_),]
      rl <- rl %>% dplyr::select(osm_id,x_, y_, z_)
      RasVal <- rbind(RasVal, rl)
      # assign(paste0("rval", i), rl)
      # missingpoints <- rl[is.na(rl$`raster::extract(get(paste0("r", i)), pointCoords[, c(2:3)])`),]
    }
    
    # RasVal$z_ <- RasVal[,4]
    RasVal2 <- RasVal %>% dplyr::select(osm_id,x_, y_, z_)
    RasVal2 <- RasVal2[,list(z_= mean(z_), x_ = mean(x_), y_ = mean(y_)),osm_id]
    # z <- RasVal2[duplicated(RasVal2$osm_id),]
    pointCoords <- full_join(pointCoords, RasVal2, by = c("osm_id", "x_", "y_")) %>%
      # unique() %>%
      cbind(., inlevees[,2])
    # pointCoords2 <-  cbind(pointCoords, inlevees[,2]) %>%
    #   left_join(., RasVal2)
    
    
    # Rejoining the z value to the road network
    # Removing the DEMs from the workspace.
    rm(list= paste0("r", 1:length(DemToGet)))
    
    tides <- read_csv("./R/DATA-PROCESSED/tideheightbins_cnties.csv") %>%
      filter(GEOID %in% commutingarea$h_county)
    tides$bin0 = 0
    o_e <- pointCoords %>% dplyr::select(-x_, -y_, z_o = z_, inleveeso=inlevees)
    o_d <- pointCoords %>% dplyr::select(-x_, -y_, z_d = z_, inleveesd=inlevees)
    graph2  <- left_join(graph, o_e, by = c("from_id" = "osm_id"))
    graph2 <- left_join(graph2, o_d, by = c("to_id" = "osm_id"))
    
    graph2$kph2 = ((graph2$d) / graph2$time)/1000 *3600 # mean=22.1 This is the time field.
    graph2 <- graph2[which(graph2$time>0),]
    graph2$kph_dweighted = ((graph2$d_weighted) / graph2$time)/1000 *3600 # Fastest Time
    graph2$kph_tweighted = ((graph2$d) / graph2$time_weighted)/1000 *3600 # Slowest Time
    graph2$kph_weighted = ((graph2$d_weighted) / graph2$time_weighted)/1000 *3600 # mean=22.65
    
    # Joining up the bridges and the levees with the graph
    graph2 <- left_join(graph2, bridges, by = c("way_id" = "osm_id")) %>%
      left_join(., tunnels, by = c("way_id" = "osm_id"))
    # graph2 <- left_join(graph2, inleveeso,by = c("from_id" = "osm_id"))
    # graph2 <- left_join(graph2, inleveesd,by = c("to_id" = "osm_id"))
    graph2$bridge <- case_when(
      is.na(graph2$bridge) ~ "no",
      graph2$bridge == "yes" ~"yes"
    )
    graph2$tunnel <- case_when(
      is.na(graph2$tunnel) ~ "no",
      graph2$tunnel == "yes" ~"yes"
    )
    
    # Setting the elevations to 100m if they're inside a levee.
    graph2$z_o = case_when(
      graph2$inleveeso == TRUE ~ 100,
      graph2$inleveeso == FALSE ~ graph2$z_o
    )
    graph2$z_d = case_when(
      graph2$inleveesd == TRUE ~ 100,
      graph2$inleveesd == FALSE ~ graph2$z_d
    )
    graph2$z_m =  (graph2$z_o + graph2$z_d)/2
    if(is.na(graph2$z_m)){graph2$z_m == 100}
    graph2[is.na(graph2)] = 100
    
    write.fst(graph2, paste0("./R/DATA-PROCESSED/GRAPHS/h", cntyst, ".fst"), 100)
    rm(list=setdiff(ls(), l))
    gc(reset=TRUE)
  }}

  targetcounties <- sort(unique(commutingareas$h_county))
  # targetcounties <- sort(unique(commutingareas$h_county[which(!commutingareas$h_state %in% c("36"))]))
  # targetcounties <- targetcounties[which(!targetcounties %in% c("06037", "06059", "06111"))]
l <- ls()
l <- c(l, "l")

  tictoc::tic()
  lapply(targetcounties, MakeRdsElevation)
  tictoc::toc()