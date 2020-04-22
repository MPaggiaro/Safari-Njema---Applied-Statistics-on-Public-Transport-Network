#setwd("/Users/pietro/Documents/appstat_project")
#rm(list = ls())

## loading

library(leaflet)
library(sf)
library(rgdal)
library(sp)

if (!exists("df_tracks")){
  df_tracks <- read.table("local_data/df_tracks.txt")
}

## parametri
JRNY_ID <- 15

## extracting points

# ho tristemente scoperto che il journey_id non è univoco... Quindi per
# ora prendo solo il primo viaggio che ha journey_id = JRNY_ID

df_tmp <- df_tracks[df_tracks$journey_id == JRNY_ID,]

if (dim(df_tmp)[1] != 0){
  i <- 1
  df1 <- df_tmp[1,]
  while ( i != dim(df_tmp)[1] && df_tmp$record_id[i+1] > df_tmp$record_id[i] ){
    df1 <- rbind(df1, df_tmp[i+1,])
    i = i+1
  }
}else{
  stop("Empty selection")
}
  

####  MAPUTO MAP ####

## reading the map
my_map <- readOGR( 
  dsn= paste0(getwd(),"/DistrictMap/") , 
  layer="Maputo_Quartieri",
  verbose=FALSE
)

## plotting the map
par(mar=c(0,0,0,0))
plot(my_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1 )


dat <- data.frame( Longitude=df1$lng, Latitude=df1$lat )

# dat <- data.frame( Longitude=df_tracks$lng[1:100], Latitude=df_tracks$lat[1:100], 
#                     Name = c("P1", "P2", etc... ) )


coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)


# cercare dove sono i punti
over(dat,my_map)

# plottare i punti
points(df1$lat ~ df1$lng, col = "red", cex = 1)


## EXTRA
# come selezionare un solo poligono della mappa
Central_A <- my_map[ my_map@data$District == "Central_A", ]


####

