source("script/import_data.R", encoding = "UTF-8")

source("script/utility.R", encoding = "UTF-8")


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


trip_to_tracks = function(trip_row, df_tracks)
{
  # Prende una riga del df trips e restituisce tutti i tracks associati 
  return(df_tracks[which(df_tracks$day == trip_row$day & df_tracks$journey_id == trip_row$journey_id),])
}


#mappa mascaretti
my_map <- readOGR( 
  dsn= paste0(getwd(),"/DistrictMap") , 
  layer="HomogenousDivisionOK",
  verbose=FALSE
)

#aggiungo tutti i distretti al tracks

df_tracks_districts <- df_tracks 
dat <- data.frame( Longitude=df_tracks_districts$lng, Latitude=df_tracks_districts$lat )

coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)

district_tracks <- over(dat,my_map)


df_tracks_districts <- cbind(df_tracks_districts, c(district_tracks))
colnames(df_tracks_districts)[19] <- c("district_id")
colnames(df_tracks_districts)[20] <- c("district_name")

## cos'è un viaggio lungo?

hist(df_trips_pos$distance[which(df_trips_pos$distance < 50000)], breaks = 200)


mean(df_trips_pos$distance)
sqrt(var(df_trips_pos$distance))
max(df_trips_pos$distance)

v = df_trips_pos$distance/df_trips_pos$duration
quantile(v, c(0,0.2))*3.6 # il 20% più lento va a meno di 5.2km/h

#estraggo i trips più lenti 
slow_trips = df_trips_pos[which(v<5.1583/3.6),]
slow_tracks = NULL

a = df_tracks$day*100000+df_tracks$journey_id
b = slow_trips$day*100000+slow_trips$journey_id
slow_tracks = df_tracks[  a%in%b, ]



# aggiungo i distretti a slow_tracks (perchè non l ho fatto prima)



df_tracks_districts <- slow_tracks
dat <- data.frame( Longitude=df_tracks_districts$lng, Latitude=df_tracks_districts$lat )


coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)

district <- over(dat,my_map)



slow_tracks_districts <- cbind(slow_tracks, c(district))
colnames(slow_tracks_districts)[19] <- c("district_id")
colnames(slow_tracks_districts)[20] <- c("district_name")

#plot slow
map_new_format <- st_read("DistrictMap/HomogenousDivisionOK.shp")
head(map_new_format)
par(mar=c(0,0,0,0))
plot(my_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1 )
count_districts <- count(district, "id")
map_new_format$cat <- as.factor(map_new_format$cat)
count_districts$cat <- as.factor(count_districts$id)
map_and_data <- left_join(map_new_format,count_districts)

qtm(map_and_data, "freq")

#plot all

map_new_format <- st_read("DistrictMap/HomogenousDivisionOK.shp")
head(map_new_format)
par(mar=c(0,0,0,0))
plot(my_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1 )
count_districts <- count(district_track, "id")
map_new_format$cat <- as.factor(map_new_format$cat)
count_districts$cat <- as.factor(count_districts$id)
map_and_data <- left_join(map_new_format,count_districts)

qtm(map_and_data, "freq")
