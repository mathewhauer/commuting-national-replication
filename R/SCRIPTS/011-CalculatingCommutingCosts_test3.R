source("./R/SCRIPTS/000-Libraries.R")
options(tigris_use_cache = TRUE)
# This will download a list of FIPS codes from online.
fipslist <- read_csv(file="https://raw.githubusercontent.com/kjhealy/fips-codes/master/county_fips_master.csv", 
                     col_names = TRUE) %>%
  mutate(GEOID = str_pad(fips, 5, pad = "0"),
         STATEID = substr(GEOID,1,2),
         CNTYID = substr(GEOID,3,5)) 

# 
allcounties <-  counties(cb=TRUE, year =2019)

commutingareas <- read_csv("./R/DATA-PROCESSED/ALL-COMMUTINGAREAS-1percent.csv") %>%
  mutate(h_state = substr(h_county,1,2))

shapeslist <- list.files( paste0("./R/DATA-RAW/LEVEES/LEVEES"), pattern = ".shp",recursive=TRUE,  full.names = TRUE)
levees <- rbindlist(lapply(shapeslist, function(x) st_read(x)[,"areaSqua", "geometry"])) %>%
  st_sf(.) %>%
  filter(!is.na(st_is_valid(.))) %>%
  filter(areaSqua >0) %>%
  st_transform(., crs(allcounties))

gettraveltimes <- function(cntyst){
  print(cntyst)
  #   cntyst <- "23029"
  #   cntyst <- "06041"
  #   cntyst <- "23013"
  # Generating the HW counties
  if(!file.exists(paste0("./R/DATA-PROCESSED/COMMUTINGCOSTS/h", cntyst,".rds"))){
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

    # plot(area$geometry, add=TRUE, col = "white")
    # plot(c2$geometry, add = TRUE, col = "red")
    
    ## getting the Tide Data
    tides <- read_csv("./R/DATA-PROCESSED/tideheightbins_cnties.csv") %>%
      filter(GEOID %in% commutingarea$h_county)
    tides$bin0 = 0
 
    graph2 <- read.fst(paste0("./R/DATA-PROCESSED/GRAPHS/h", cntyst, ".fst"))
    graph_dry <- graph2
    graph_1 <- graph2
    graph_2 <- graph2
    graph_3 <- graph2
    graph_4 <- graph2
    graph_5 <- graph2

    floodadjust <- function(thisgraph, bin){
      # Flood depth of 0.077m
      a <- get(thisgraph)
      a$i <- (unique(tides[[as.character(bin)]])-a$z_m)*1000
      a$kph_adjusted = case_when(
        a$i <= 0 ~ a$kph2,
        a$i > 300 ~ 1/.6,
        a$i > 0 ~ 0.0009 * a$i ^2 - 0.5529 * a$i + 86.9448)
      a$kph_adjusted = case_when(
        a$i <= 0 ~ a$kph_adjusted,
        a$i > 0 & a$kph_adjusted > a$kph2 ~ a$kph2,
        a$i > 0 & a$kph_adjusted <= a$kph2 ~ a$kph_adjusted)
      a$kph_adjusted = case_when(
        a$bridge == "yes" ~ a$kph2,
        a$tunnel == "yes" ~ a$kph2,
        a$bridge == "no" ~ a$kph_adjusted,
        a$tunnel == "no" ~ a$kph_adjusted)
      a$time <- a$d/(a$kph_adjusted/3600*1000)
      return(a)
    }

    graph_1 <- floodadjust("graph_1", "bin1")
    graph_2 <- floodadjust("graph_2", "bin2")
    graph_3 <- floodadjust("graph_3", "bin3")
    graph_4 <- floodadjust("graph_4", "bin4")
    graph_5 <- floodadjust("graph_5", "bin5")
    graph_dry <- floodadjust("graph_dry", "bin0")
    # graph_dry$time <- graph_dry$d/(graph_dry$kph2/3600*1000)
    
    getcbgs <-function(this.state){
      cbgs<- get_decennial(geography = "block group",
                           state = this.state,
                           # county = cntyarea$CNTYID[which(cntyarea$state_abbr == this.state)],
                           county = as.character(cntyarea$county_name[which(cntyarea$state_abbr == this.state)]),
                           variables = "P001001",
                           geometry=TRUE)
      # cbgs<- block_groups(year=2010, cb=TRUE, state = this.state, county = cntyarea$CNTYID[which(cntyarea$state == this.state)])
      cbgs = cbgs[!st_is_empty(cbgs),,drop=FALSE]
    }
    
    getblks <- function(this.state){
      blocks<- get_decennial(geography = "block",
                             state = this.state,
                             # county = as.character(cntyarea$county_name[which(cntyarea$state_abbr == this.state)]),
                             county = as.character(cntyarea$county_name[which(cntyarea$state_abbr == this.state)]),
                             variables = "P001001",
                             geometry=TRUE)
      blocks = blocks[!st_is_empty(blocks),,drop=FALSE]
    }
    cbgs <- map_df(unique(cntyarea$state_abbr), getcbgs)
    blocks <- map_df(unique(cntyarea$state_abbr), getblks)
    
    blocks$blkgrp = substr(blocks$GEOID,1,12)
    
    centroids <- st_centroid(blocks)
    
    # Calculating the weighted mean center of the CBGs based on the block data.
    centroids <-  do.call(rbind, st_geometry(centroids)) %>% 
      as_tibble() %>% setNames(c("lon","lat")) %>% cbind(P001001 = centroids$value, GEOID = centroids$blkgrp) %>%
      group_by(GEOID) %>%
      dplyr::summarise(lon = sum(lon*P001001)/sum(P001001),
                       lat =  sum(lat*P001001)/sum(P001001)) %>%
      mutate(cntyst = substr(GEOID,1,5)) 
    
    # Finding the centroids in the CBG data.
    print("7/9  Finding the centroids in the CBG data.")
    # Converting the X/Y/ID data into a spatial object that has the same Coordinate Reference System (CRS) as the CBGs.
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
      pointCoords <- st_as_sf(unique(rbind(pointCoords, pointCoords2)),
                              coords = c("lon", "lat"),
                              crs = st_crs(blocks))
    
    coords <- st_as_sf(pointCoords,
                       coords = c("x_", "y_"),
                       crs = crs(blocks))

    nodes_coords <-  do.call(rbind, st_geometry(coords)) %>%
      as_tibble() %>% setNames(c("lon","lat"))

    weighted_coords <-  cbind(centroids$lon, centroids$lat) %>%
      as_tibble() %>% setNames(c("lon","lat"))


    # Joining up the unique ID to the centroids object.
    centroids$h_county = substr(centroids$GEOID,1,5)
    centroids$type = "Weighted Mean"
    
    # 0 population block groups return NaN. We replace those with the geographic center.
    centroids_cbgs <- st_centroid(arrange(cbgs[which(cbgs$GEOID %in% centroids[which(is.nan(centroids$lon)),]$GEOID),], GEOID))
    geometric_coords <-  do.call(rbind, st_geometry(centroids_cbgs)) %>%
      as_tibble() %>% setNames(c("lon","lat"))
    
    tryCatch({
      centroids$lon[which(centroids$GEOID %in% centroids[which(is.nan(centroids$lon)),]$GEOID)] <- geometric_coords$lon
      centroids$lat[which(centroids$GEOID %in% centroids[which(is.nan(centroids$lat)),]$GEOID)] <- geometric_coords$lat
    },
    error=function(e)centroids <<- na.omit(centroids))
    
    
    centroids <- na.omit(centroids)
    weighted_coords <-  cbind(centroids$lon, centroids$lat) %>%
      as_tibble() %>% setNames(c("lon","lat"))
    coords <- st_as_sf(pointCoords,
                       coords = c("x_", "y_"),
                       crs = crs(allcounties))
    nodes_coords <-  do.call(rbind, st_geometry(coords)) %>%
      as_tibble() %>% setNames(c("lon","lat"))
    ptsmean <- ann(as.matrix(nodes_coords), as.matrix(weighted_coords), k=2)
    ptsmean <- ann(as.matrix(nodes_coords), as.matrix(weighted_coords), k=1)
    centroids$nodes <- coords$osm_id[ptsmean$knnIndexDist[,1]]
       from<- centroids$nodes[which(centroids$cntyst==cntyst)]
    to <- centroids$nodes

    locs <- as.data.frame(centroids[,c("GEOID", "nodes", "lat", "lon")])
    set.seed(1)
    gettimes <- function(graph_df, varname){
      set.seed(1)
      melt(dodgr_times(graph_df, from = from, to = to, shortest = FALSE)/60)  %>%
        dplyr::rename(!!quo_name(varname) := value)
    }
    
    print("8/9 Getting Travel Times.")
    times_dry <- gettimes(graph_1, "cost_dry")
    # nrow(times_dry[!complete.cases(times_dry),])
    times_1 <- gettimes(graph_1, "cost_1")
    times_2 <- gettimes(graph_2, "cost_2")
    times_3 <- gettimes(graph_3, "cost_3")
    times_4 <- gettimes(graph_4, "cost_4")
    times_5 <- gettimes(graph_5, "cost_5")
    
      times <- cbind(times_dry, times_1, times_2, times_3, times_4, times_5)
    times <- times[,c(1:2,3,6,9,12,15,18)]
    
    times$Var1 = as.character(times$Var1)
    times$Var2 = as.character(times$Var2)
    times2<- left_join(times, locs, by = c(Var1 ="nodes")) %>%
      na.omit %>%
      dplyr::rename("h_blkgrp" = "GEOID",
                    "h_x" = "lat",
                    "h_y" = "lon") %>%
      left_join(., locs, by = c(Var2 ="nodes")) %>%
      dplyr::rename("w_blkgrp" = "GEOID",
                    "w_x" = "lat",
                    "w_y" = "lon")
    times2$h_county = substr(times2$h_blkgrp,1,5)
    times2$w_county = substr(times2$w_blkgrp,1,5)
    times2 <- times2[which(!times2$h_blkgrp == times2$w_blkgrp),]
    
    print("Cleaning the workspace")
    rm(list= setdiff(ls(), c("times2", "tides", "cntyst", "fipslist", "allcounties", "commutingareas", "levees",
                             "gettraveltimes", "targetcounties", "clusters")))

    gc(reset=TRUE)
    print("9/9 Gathering the Commuting Costs.")
    h <- readRDS(paste0("./R/DATA-PROCESSED/COMMUTERS/h", cntyst,".RDS"))
       h <- as.data.table(h)
    times2$pairs <- paste(times2$h_blkgrp, times2$w_blkgrp)
    times2 <- as.data.table(times2)
    times2 <- times2[times2$pairs %in% h$pairs,c(3:16)]
    
    times2 <- na.omit(times2)
    times2[h, on = c('h_blkgrp', 'w_blkgrp'), `:=` (c2002 = i._2002,
                                                c2003 = i._2003,
                                                c2004 = i._2004,
                                                c2005 = i._2005,
                                                c2006 = i._2006,
                                                c2007 = i._2007,
                                                c2008 = i._2008,
                                                c2009 = i._2009,
                                                c2010 = i._2010,
                                                c2011 = i._2011,
                                                c2012 = i._2012,
                                                c2013 = i._2013,
                                                c2014 = i._2014,
                                                c2015 = i._2015,
                                                c2016 = i._2016,
                                                c2017 = i._2017)
       ]
    
    times2[is.na(times2)] <-0
    
    z3<-melt(times2, #id.vars = c("cost_dry", "cost_1", "cost_2", "cost_3", "cost_4", "cost_5", "h_blkgrp", "w_blkgrp", "h_county",
             #               "w_county"),
             measure.vars = paste0("c",2002:2017), variable.name = "year", value.name = "commuters" )
    z3$year = as.numeric(substr(z3$year,2,5))
    z3 <- as.data.table(z3)
    z3[, scenario:="historic"]
    z2060 <- z3[year %in% c(2015:2017), .(commuters = sum(commuters)/3, 
                                          cost_dry = mean(cost_dry),
                                          cost_1 = mean(cost_1),
                                          cost_2 = mean(cost_2),
                                          cost_3 = mean(cost_3),
                                          cost_4 = mean(cost_4),
                                          cost_5 = mean(cost_5),
                                          year  = 2060),
                by=list(h_blkgrp,w_blkgrp, h_county, w_county, h_x, h_y, w_x, w_y)] 
    z2060 <- rbind(z2060 %>% mutate(scenario="low"), 
                   z2060 %>% mutate(scenario="intermediate"), 
                   z2060 %>% mutate(scenario="extreme"))
    z2060 <- z2060[which(z2060$commuters >0),]
    
    z3 <- z3[which(z3$commuters >0),]
    z3 <- rbind(z3, z2060)
    
    tides2 <- as.data.table(tides[,c(14:20)])
    tides2[is.na(tides2)] <- 0
    z3[tides2, on = c('year', 'scenario'), `:=` (Days1 = i.1,
                                                 Days2 = i.2,
                                                 Days3 = i.3,
                                                 Days4 = i.4,
                                                 Days5 = i.5)]
    z3$totalcost <- (z3$cost_1 - z3$cost_dry) * z3$Days1 +
      (z3$cost_2 - z3$cost_dry) * z3$Days2 +
      (z3$cost_3 - z3$cost_dry) * z3$Days3 +
      (z3$cost_4 - z3$cost_dry) * z3$Days4 +
      (z3$cost_5 - z3$cost_dry) * z3$Days5
    z3$totdays <- z3$Days1 + z3$Days2 + z3$Days3 + z3$Days4 +z3$Days5
    
    
    z3$totalcostperday <- z3$totalcost / z3$totdays
    
    z3$avebase <- (2 * 250 * z3$totalcostperday) * z3$commuters
    z3$percommuter <- z3$totalcostperday / z3$commuters
    z3 <- z3[which(z3$commuters >0),]
    
    saveRDS(z3, file = paste0("./R/DATA-PROCESSED/COMMUTINGCOSTS/h",cntyst,".rds"))
    # write.csv(z3, file=gzfile(paste0("./R/DATA-PROCESSED/COMMUTINGCOSTS/h",cntyst,".csv.gz")))
    rm(list= setdiff(ls(), c("cntyst", "fipslist", "allcounties", "commutingareas", "levees",
                             "gettraveltimes", "targetcounties", "clusters")))
    gc(reset=TRUE)
    
  }
}

targetcounties <- sort(unique(commutingareas$h_county))
targetcounties <- sort(unique(commutingareas$h_county[which(!commutingareas$h_state %in% c("36"))]))
targetcounties <- targetcounties[which(!targetcounties %in% c("06037", "06059", "06111"))]

tictoc::tic()
lapply(targetcounties, gettraveltimes)
tictoc::toc()

# lapply(targetcounties, gettraveltimes)
# # z4 <- gettraveltimes("23013")


