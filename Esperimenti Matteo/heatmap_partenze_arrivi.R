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

# options (really useful ?)
#options(scipen = 999)

# reading data (e.g. day 51)
trips <- read.table("local_data/Trips/WithPosInfo/trips_51.txt", header=T)
head(trips)

# reading the map (of Pit):

my_map <- readOGR( 
  dsn= paste0(getwd(),"/DistrictMap") , 
  layer="Maputo_Quartieri",
  verbose=FALSE
)

# rough counter of zones of the original dataset:
# depZones <- count(trips, vars="depName")
# arrZones <- count(trips, vars="arrName")
# BIG PROBLEM: numbers of zones are not the same in the dataset and
# in Pit's map!


# let's take the coordinates of arrival/departure of each point in the
# dataset trips. E.g. departures:
departures <- data.frame( Longitude=trips$dep_lng, Latitude=trips$dep_lat )
coordinates(departures) <- ~Longitude + Latitude

arrivals <- data.frame( Longitude=trips$arr_lng, Latitude=trips$arr_lat )
coordinates(arrivals) <- ~Longitude + Latitude

proj4string(departures) <- proj4string(my_map)
res_dep <- over(departures,my_map)
#table(res_dep$id)

proj4string(arrivals) <- proj4string(my_map)
res_arr <- over(arrivals,my_map)


# aggregate data over zones:
dep_count <- count(res_dep, "id")
arr_count <- count(res_arr, "id")

# last step: plot of a heatmap showing number of arrivals/departures per zone:
# first, read the map data in a different format:
map_new_format <- st_read("DistrictMap/Maputo_Quartieri.shp")
head(map_new_format)
map_new_format$id <- as.factor(map_new_format$id)
dep_count$id <- as.factor(dep_count$id)
map_and_data <- left_join(map_new_format,dep_count)

# final result: heat map.
qtm(map_and_data, "freq")

# I'll add later arrivals, same process ;)