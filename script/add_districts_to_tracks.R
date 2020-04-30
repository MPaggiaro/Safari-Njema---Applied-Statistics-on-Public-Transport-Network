#add coloumn tracks

library(leaflet)
library(sf)
library(rgdal)
library(sp)



my_map <- readOGR( 
  dsn= paste0(getwd(),"/DistrictMap") , 
  layer="Maputo_Quartieri",
  verbose=FALSE
)


dat <- data.frame( Longitude=df_tracks_districts$lng, Latitude=df_tracks_districts$lat )


coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)

district <- over(dat,my_map)



df_tracks_districts <- cbind(df_tracks, c(district))
colnames(df_tracks_districts)[19] <- c("district_id")
colnames(df_tracks_districts)[20] <- c("district_name")

