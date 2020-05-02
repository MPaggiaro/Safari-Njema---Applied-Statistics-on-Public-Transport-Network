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
district_map <- readOGR( 
  dsn= paste0(getwd(),"/DistrictMap") , 
  layer="HomogenousDivisionOK",
  verbose=FALSE
)

#aggiungo tutti i distretti al tracks

points <- data.frame( east=df_tracks$Easting, north=df_tracks$Northing )
coordinates(points) <- ~ east + north
proj4string(points) <- proj4string(district_map)

district <- over(points,district_map)

#add the columns with the district id
df_tracks_districts <- cbind(df_tracks, c(district))
colnames(df_tracks_districts)[colnames(df_tracks_districts)=="fid"] <- "district_id"
head(df_tracks_districts)

## cos'è un viaggio lento?

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
slow_tracks = df_tracks_districts[  a%in%b, ]

points <- data.frame( east=slow_tracks$Easting, north=slow_tracks$Northing )
coordinates(points) <- ~ east + north
proj4string(points) <- proj4string(district_map)
district_slow <- over(points,district_map)

x11()

#plot slow 
map_new_format <- st_read("DistrictMap/HomogenousDivisionOK.shp")
head(map_new_format)
plot(district_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1 )
count_districts <- count(district_slow, "cat")
map_new_format$cat <- as.factor(map_new_format$cat)
count_districts$cat<- as.factor(count_districts$district_id)
map_and_data <- left_join(map_new_format,count_districts)
qtm(map_and_data, "freq")

#plot all
x11()
map_new_format <- st_read("DistrictMap/HomogenousDivisionOK.shp")
head(map_new_format)
par(mar=c(0,0,0,0))
plot(district_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1 )
count_districts <- count(district, "cat")
map_new_format$cat <- as.factor(map_new_format$cat)
count_districts$cat<- as.factor(count_districts$district_id)
map_and_data <- left_join(map_new_format,count_districts)

qtm(map_and_data, "freq")


# per il distretto 5
distr_id=11
all_districts = sort(unique(df_tracks_districts$district_id[!is.na(df_tracks_districts$district_id)]))
m <- empty_list <- vector(mode = "list")
tutti = NULL
for (distr_id in all_districts)
{
  ids = df_tracks_districts[which(df_tracks_districts$district_id == distr_id),]
  ids = ids$day*100000+ids$journey_id
  ids = unique(ids)
  trips_ids = df_trips_pos$day*100000+df_trips_pos$journey_id
  harambe = df_trips_pos[ trips_ids %in% ids, ]
  row_k = harambe$distance / harambe$duration
  m[[distr_id]] = row_k
  tutti = append(tutti,row_k)
}

tutti_gaussiani = tan(tutti)
qqnorm(tutti_gaussiani)
qqline(tutti_gaussiani)
tutti_gaussiani_hist = tutti_gaussiani[tutti_gaussiani>-10 & tutti_gaussiani<10]
hist(tutti_gaussiani_hist,breaks=200)
mu_tot = mean(tutti_gaussiani)
var_tot = var(tutti_gaussiani)