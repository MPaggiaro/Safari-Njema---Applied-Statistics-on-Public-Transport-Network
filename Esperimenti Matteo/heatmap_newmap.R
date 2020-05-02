################################################
#
# Heatmap of the departures/arrival zones of a given day.
#
# Author: MP. Date: 26/04/2020
#
#################################################

# currentwd -> già lo sai

# installation of packages:
# install.packages("dplyr")
# install.packages("plyr")
# install.packages("raster")
# install.packages("sp")
# install.packages("rgdal")

# uploading some graphical libraries:
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(plyr)
library(sp)
library(raster)
library(rgdal)


# reading data (e.g. day 51)
trips <- read.table("local_data/Trips/WithPosInfo/trips_51.txt", header=T)
head(trips)

my_map <- readOGR( 
  dsn= paste0(getwd(),"/DistrictMap") , 
  layer="HomogenousDivisionOK",
  verbose=FALSE
)

map_2 <- st_read("DistrictMap/HomogenousDivisionOK.shp")

# departures <- data.frame( Longitude=trips$dep_east, Latitude=trips$dep_north )
# coordinates(departures) <- ~Longitude + Latitude
# 
# arrivals <- data.frame( Longitude=trips$arr_east, Latitude=trips$arr_north )
# coordinates(arrivals) <- ~Longitude + Latitude
# 
# 
# map_2 <- left_join(map_2,trips)

# selection of dep/arr zones:
arr_dep <- data.frame(depName=trips$depName, arrName=trips$arrName)

dep_count <- count(arr_dep, "depName")
arr_count <- count(arr_dep, "arrName")

# rinomino variabili
names(dep_count)[names(dep_count) == "freq"] <- "freq_dep"
names(arr_count)[names(arr_count) == "freq"] <- "freq_arr"
names(dep_count)[names(dep_count) == "depName"] <- "fid"
names(arr_count)[names(arr_count) == "arrName"] <- "fid"


map_2 <-left_join(map_2,dep_count)
map_2 <-left_join(map_2,arr_count)

x11()
qtm(map_2, "freq_dep")
x11()
qtm(map_2, "freq_arr")
