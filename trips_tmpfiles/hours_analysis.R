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


# select fasce orarie:

