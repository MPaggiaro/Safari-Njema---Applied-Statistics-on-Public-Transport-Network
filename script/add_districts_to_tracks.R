#add coloumn tracks

library(leaflet)
library(sf)
library(rgdal)
library(sp)


#########################################################################################
my_map <- readOGR( 
  dsn= paste0(getwd(),"/DistrictMap") , 
  layer="Maputo_Quartieri",
  verbose=FALSE
)

df_tracks_districts<-df_tracks
dat <- data.frame( Longitude=df_tracks_districts$lng, Latitude=df_tracks_districts$lat )


coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)

district <- over(dat,my_map)



df_tracks_districts <- cbind(df_tracks, c(district))
colnames(df_tracks_districts)[19] <- c("district_id")
colnames(df_tracks_districts)[20] <- c("district_name")


############################################################################################
# Usando i distretti nello shapefile di Mascaretti

district_map<- readOGR( 
  dsn= paste0(getwd(),"/DistrictMap") , 
  layer="HomogenousDivisionOK",
  verbose=FALSE
)



points <- data.frame( east=df_tracks$Easting, north=df_tracks$Northing )
coordinates(points) <- ~ east + north
proj4string(points) <- proj4string(district_map)

district <- over(points,district_map)

#add the columns with the district id
df_tracks_districts <- cbind(df_tracks, c(district))
colnames(df_tracks_districts)[colnames(df_tracks_districts)=="fid"] <- "district_id"
head(df_tracks_districts)



