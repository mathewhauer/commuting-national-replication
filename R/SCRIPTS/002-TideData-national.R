###------Tide Data -----
## @knitr TideDat
source("./R/SCRIPTS/000-Libraries.R")
######      Data Load      ######
# Loading in the tribble for the tide station IDs.
tide_stations <-   tribble(
  ~station_id,
  8410140,  8411060,  8413320,  8418150,  8419317,  8423898,  8443970,  8447386,  8447435,  8447930,  
  8449130,  8452660,  8452944,  8454000,  8454049,  8461490,  8465705,  8467150,  8510560,  8516945,  
  8518750,  8519483,  8531680,  8534720,  8536110,  8537121,  8539094,  8545240,  8546252,  8548989,
  8551762,  8551910,  8555889,  8557380,  8570283,  8571421,  8571892,  8573364,  8573927,  8574680,
  8575512,  8577330,  8594900,  8631044,  8632200,  8635027,  8635750,  8636580,  8637689,  8638610,
  8638863,  8639348,  8651370,  8652587,  8654467,  8656483,  8658120,  8658163,  8661070,  8662245,
  8665530,  8670870,  8720030,  8720218,  8720219,  8720226,  8720357,  8720503,  8720625,  8721604,
  8722670,  8723214,  8723970,  8724580,  8725110,  8725520,  8726384,  8726520,  8726607,  8726667,
  8726724,  8727520,  8728690,  8729108,  8729210,  8729840,  8732828,  8735180,  8735391,  8735523,
  8736897,  8737048,  8737138,  8738043,  8739803,  8740166,  8741041,  8741533,  8747437,  8760721,
  8760922,  8761305,  8761724,  8761927,  8762075,  8762483,  8764044,  8764227,  8764314,  8766072,
  8767816,  8767961,  8768094,  8770475,  8770520,  8770570,  8770613,  8770733,  8770777,  8770808,
  8770971,  8771013,  8771341,  8771450,  8771486,  8771972,  8772447,  8772985,  8773037,  8773146,
  8773259,  8773701,  8773767,  8774230,  8774513,  8774770,  8775237,  8775296,  8775792,  8775870,
  8779280,  8779748,  8779770,  9410170,  9410230,  9410660,  9410840,  9411340,  9411406,  9412110,
  9413450,  9414290,  9414523,  9414575,  9414750,  9414863,  9414958,  9415020,  9415102,  9415144,
  9416841,  9418767,  9419750,  9431647,  9432780,  9435380,  9437540,  9439040,  9439099,  9439201,
  9440083,  9440422,  9440569,  9440581,  9440910,  9441102,  9442396,  9443090,  9444090,  9444900,
  9446484,  9447130,  9449424,  9449880
  
)

# Converting the tribble into a list
mylist <- unlist(list(tide_stations))


##########################################################################################################
###   Defining the data clean function.
###   This function filter's the tidal datums to only select the Higher-High Water (HH),
###   then calculates the percentile of each water level from the empirical distribution,
###   and finally converts the percentile into days in a year.
###   It also converts the lat/lon into numeric values.
##########################################################################################################
cleandat = function(data){
  data %>% 
    # filter(data.ty == "HH") %>%
    group_by(metadata.id) %>%
    arrange(desc(data.v)) %>%
    getanID(id.vars = "metadata.id") %>%
    mutate(percentile = .id/max(.id),
           num_days = percentile*365,
           lat = as.numeric(as.character(metadata.lat)),
           lon = as.numeric(as.character(metadata.lon)))
}

######      Setting up the parallel processing      ######

# This will set the number of parallel cores to n - 1.
parallelStartSocket(detectCores() - 1)
# We also have to load the libraries we need onto each core.
parallelLibrary("rnoaa")
# This is the function used to gather the NOAA tide data for the previous five years.
getdat = function(x){
  tryCatch({print(x)
    prod <- 'hourly_height'
    X2019 <- data.frame(coops_search(begin_date=20190101, end_date=20191231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2018 <- data.frame(coops_search(begin_date=20180101, end_date=20181231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2017 <- data.frame(coops_search(begin_date=20170101, end_date=20171231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2016 <- data.frame(coops_search(begin_date=20160101, end_date=20161231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2015 <- data.frame(coops_search(begin_date=20150101, end_date=20151231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2014 <- data.frame(coops_search(begin_date=20140101, end_date=20141231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2013 <- data.frame(coops_search(begin_date=20130101, end_date=20131231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2012 <- data.frame(coops_search(begin_date=20120101, end_date=20121231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2011 <- data.frame(coops_search(begin_date=20110101, end_date=20111231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2010 <- data.frame(coops_search(begin_date=20100101, end_date=20101231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2009 <- data.frame(coops_search(begin_date=20090101, end_date=20091231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2008 <- data.frame(coops_search(begin_date=20080101, end_date=20081231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2007 <- data.frame(coops_search(begin_date=20070101, end_date=20071231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2006 <- data.frame(coops_search(begin_date=20060101, end_date=20061231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2005 <- data.frame(coops_search(begin_date=20050101, end_date=20051231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2004 <- data.frame(coops_search(begin_date=20040101, end_date=20041231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2003 <- data.frame(coops_search(begin_date=20030101, end_date=20031231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    X2002 <- data.frame(coops_search(begin_date=20020101, end_date=20021231, station_name=x, datum='NAVD', product=prod, time_zone = "gmt", units = "metric"))
    
    MHHW<- do.call("rbind", list(X2019,X2018, X2017, X2016, X2015, X2013, X2014, X2012, X2011, X2010, X2009, X2008,
                                 X2007, X2006, X2005, X2004, X2003, X2002))
    return(MHHW)
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

mylist2 <- "8410140" # This is the station for Miami

# This is the actual parallel function. getdat is the Function, mylist is the list of tide-stations to apply the function to.
# All of the data is saved as a list in the object d.
d = parallelMap(getdat, mylist)

# The list needs to be 1) cleaned up, and 2) converted to a dataframe.
# Here, I call the function cleandat on the do.call function to bind the list into a dataframe.
MHHW <- cleandat(do.call(rbind.data.frame, d))

# Finally we need to stop the parallelization.
parallelStop()

MHHW2 <- MHHW %>%
  dplyr::select(metadata.id:data.v)
head(MHHW2)



# Converting the tide data to a format useable by the tsoutliers package.

MHHW2$data.t <- lubridate::with_tz(lubridate::ymd_hms(MHHW2$data.t, tz = "GMT"),  "America/New_York")
MHHW2$date <- lubridate::as_date(MHHW2$data.t)
MHHW2$time <- format(as.POSIXct(MHHW2$data.t), "%H:%M:%S")
MHHW2 <- MHHW2[which(between(MHHW2$time,"05:00:00", "20:00:00")),]
MHHW2 <- MHHW2[which(!weekdays(MHHW2$date) %in% c("Saturday", "Sunday")),]
MHHW2 <- MHHW2 %>%
  group_by(date, metadata.id) %>%
  filter(data.v == max(data.v),
         date >= "2002-01-01") %>%
  ungroup()  

# Sorting the tide data.
MHHW2 <- arrange(MHHW2, metadata.id, data.t)
# Saving the output to the hard drive.
# write_csv(MHHW2, "R/DATA-PROCESSED/tideheights.csv")
MHHW2 <- read_csv("R/DATA-PROCESSED/tideheights.csv")
mylist2 <- unique(MHHW2$metadata.id)

## Creates a function to control for outliers
returnadjusted <- function(stationid){
  tryCatch({
# Running the actual tsoutliers package

    # Filtering the tide data.
    a<-MHHW2[which(MHHW2$metadata.id== stationid),]
    # Setting the tide data as a time series
    z <- ts(a$data.v)
    set.seed(1)
    # Running the actual tsoutliers call.
    outliers2 <- tsoutliers::tso(z, types = c("AO","LS","TC"),cval = 10, maxit.iloop=10)
    # extracting the adjusted value.
    z2 <- outliers2$yadj
    # Combining the raw tide data (a) with the tso adjusted data (z2)
    tideheights2 <- cbind(a, as.data.frame(z2)) %>%
      dplyr::select(everything(), yadj = x)
    
    # Creating a new object to hold the tide bin heights.
    z3 <- data.frame(metadata.id = stationid, 
                     # binval =quantile(tideheights2$yadj, c(0.5, 0.75, .9, 0.95, 1)), 
                     binval = quantile(tideheights2$yadj, c(0.5, 0.90, 0.98, 0.995, 0.999)), 
                     binnum = paste0("bin", rep(seq(1:5)))) %>%
      pivot_wider(names_from = binnum, values_from = binval)
    
    tideheights3 <- tideheights2 %>%
      mutate(year = substr(date,1,4),
             scenario = "historic",
             bin = case_when(
        yadj <= quantile(yadj, 0.90) ~ 1,
        yadj <= quantile(yadj, 0.98) ~ 2,
        yadj <= quantile(yadj, 0.995) ~ 3,
        yadj <= quantile(yadj, 0.999) ~ 4,
        yadj > quantile(yadj, 0.999) ~ 5,
      ))  %>%
      group_by(metadata.id, metadata.name, metadata.lat, metadata.lon, year, scenario, bin) %>%
      dplyr::summarise(count = n()) %>%
        pivot_wider(names_from = bin, values_from = count) %>%  left_join(., z3)
    
    tideheights2060 <- tideheights2 %>%
        mutate(low = yadj + 0.19,
        intermediate = yadj + 0.45,
       extreme = yadj + 0.9,
       year = "2060") %>%
      pivot_longer(low:extreme, 
                   names_to = "scenario",
                   values_to = "Height") %>%
      mutate(bin = case_when(
        Height <= z3$bin1 ~ 1,
        Height <= z3$bin2 ~ 2,
        Height <= z3$bin3 ~ 3,
        Height <= z3$bin4 ~ 4,
        Height > z3$bin4 ~ 5)
      ) %>%
      group_by(metadata.id, metadata.name, metadata.lat, metadata.lon, year, scenario, bin) %>%
      dplyr::summarise(count = n()) %>%
      mutate(count = count/sum(count) * 250) %>%
      pivot_wider(names_from = bin, 
                  values_from = count,
                  values_fill = 0) %>%  left_join(., z3)
    tideheights3 <- rbind(tideheights3, tideheights2060)
    tideheights3$bin0 <- 0

return(tideheights3)
  }
, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

}

outputs <- map_df(mylist2, returnadjusted)
# write_csv(outputs, "./R/DATA-PROCESSED/tideheightbins.csv")
# Saving the tsoutliers to the hard drive.

# outputs <- read_csv("./R/DATA-PROCESSED/tideheightbins.csv")

## Getting ready to merge the stations with the nearest county.

targets<- read.csv("./R/DATA-RAW/countylist.csv") %>%
  mutate(GEOID = as.character(x),
         GEOID = str_pad(GEOID,5, "left", "0"))
# # County shapefile that includes only coastal states
coastal_counties <- counties( year = 2019, cb = TRUE) %>%
  filter(GEOID %in% targets$GEOID)

# Finding the centroids of the coastal counties
centroids <- st_centroid(coastal_counties)
# graph_tc <- dodgr_contract_graph(graph)
# Extracting the X/Y and unique ID from the connected graph. Uses a contracted graph for speed.
coord<-unique(data.frame(x = outputs$metadata.lon, 
                         y = outputs$metadata.lat, 
                         id = outputs$metadata.id))

# Converting the X/Y/ID data into a spatial object that has the same Coordinate Reference System (CRS) as the CBGs.
coords <- st_as_sf(coord, 
                   coords = c("x", "y"), 
                   crs = crs(coastal_counties))

foo1 <-  do.call(rbind, st_geometry(coords)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

foo2 <-  do.call(rbind, st_geometry(centroids)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

# Creating a new object called nodes that finds the minimum distance between each centroid and each unique node.
nodes<- knn.out <- ann(as.matrix(foo1), as.matrix(foo2), k=1)

# Joining up the unique ID to the centroids object.
centroids$metadata.id <- coords$id[knn.out$knnIndexDist[,1]]
centroids2 <- left_join(centroids, outputs)

# write_csv(centroids2, "./R/DATA-PROCESSED/tideheightbins_cnties.csv")