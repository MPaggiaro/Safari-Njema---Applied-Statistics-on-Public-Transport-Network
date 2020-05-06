###

# 1) data exploration:


# reading data (e.g. day 51)
trips_71 <- read.table("local_data/Trips/WithPosInfo/trips_71.txt", header=T)
trips_72<- read.table("local_data/Trips/WithPosInfo/trips_72.txt", header=T)
trips_73 <- read.table("local_data/Trips/WithPosInfo/trips_73.txt", header=T)
trips_74<- read.table("local_data/Trips/WithPosInfo/trips_74.txt", header=T)

# select number of trips per hour:
# first of all, whole Maputo.
# Later, we'll take a precise zone.

#add coloumn tracks

library(leaflet)
library(sf)
library(rgdal)
library(sp)
library(plyr)


#########################################################################################
# including map

my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") , 
  layer="Maputo5distr",
  verbose=FALSE
)

leaflet(my_map) %>%addTiles() %>%
  addPolylines(color = "#444444", weight = 1)

############################################################################################
# departures

dep_data <- data.frame( Longitude=trips_71$dep_lng, Latitude=trips_71$dep_lat )


coordinates(dep_data) <- ~ Longitude + Latitude
proj4string(dep_data) <- proj4string(my_map)

district <- over(dep_data,my_map)
district <- district[,c(2,1)]


df_trips_district <- cbind(trips_71, c(district))
names(df_trips_district)[names(df_trips_district) == "Zone_ID"] <- "dep_ID"
names(df_trips_district)[names(df_trips_district) == "name"] <- "dep_zone"


############################################################################################
# arrivals

arr_data <- data.frame( Longitude=trips_71$arr_lng, Latitude=trips_71$arr_lat )


coordinates(arr_data) <- ~ Longitude + Latitude
proj4string(arr_data) <- proj4string(my_map)

district <- over(arr_data,my_map)
head(district)
district <- district[,c(2,1)]


df_trips_district <- cbind(df_trips_district, c(district))
names(df_trips_district)[names(df_trips_district) == "Zone_ID"] <- "arr_ID"
names(df_trips_district)[names(df_trips_district) == "name"] <- "arr_zone"

## count for the hours
head(df_trips_district)
hour_data_center <- df_trips_district[df_trips_district$dep_ID==3 || df_trips_district$arr_ID==3,c(10,16)]
hours_arr_count <- count(hour_data_center,"arr_hour")
hours_dep_count <- count(hour_data_center,"dep_hour")


# plots of the traffic by hour:
  x11()
hours_arr_count <- is.numeric(hours_arr_count)
hist(hour_data_center$dep_hour,breaks = 24)


# for some other days (similar):
source("trips_tmpfiles/functions.R", encoding = "UTF-8")

trips_72 <- add_zones_labels(trips_72,my_map)
trips_73 <- add_zones_labels(trips_73,my_map)
trips_74 <- add_zones_labels(trips_74,my_map)

hour_72 <- trips_72[trips_72$dep_ID==3 || trips_72$arr_ID==3,c(10,16)]
x11()
hist(hour_72$dep_hour,breaks = 24)

hour_73 <- trips_73[trips_73$dep_ID==3 || trips_73$arr_ID==3,c(10,16)]
x11()
hist(hour_73$dep_hour,breaks = 24)
hour_74 <- trips_74[trips_74$dep_ID==3 || trips_74$arr_ID==3,c(10,16)]
x11()
hist(hour_74$dep_hour,breaks = 24)


# very different one:

trips_100 <- read.table("local_data/Trips/WithPosInfo/trips_100.txt", header=T)

trips_100 <- add_zones_labels(trips_100,my_map)
hour_100 <- trips_100[trips_100$dep_ID==3 || trips_100$arr_ID==3,c(10,16)]
x11()
hist(hour_100$dep_hour,breaks = 24)

count_100 =count(trips_100,"dep_hour")
plot(count_100$dep_hour, count_100$freq)

