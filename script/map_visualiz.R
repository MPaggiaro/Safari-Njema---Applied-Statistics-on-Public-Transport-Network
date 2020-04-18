#setwd("/Users/pietro/Documents/appstat_project")
#rm(list = ls())

### Settate la vostra wd ###

library(leaflet)

## Plottare punti di partenza dei primi 10 trips, senza doppioni ##

df_trips_pos <- read.table("local_data/df_trips_pos.txt")
head(df_trips_pos)

df <- data.frame( dep_lat = df_trips_pos$dep_lat, dep_lng = df_trips_pos$dep_lng,
                  depName = df_trips_pos$depName)[1:10,]
df <- df[!duplicated(df$depName),]

leaflet(data = df) %>% addTiles() %>%
  addCircleMarkers(~df$dep_lng, ~df$dep_lat, label = ~as.character(df$depName))

## Plottare tracks per utente ##

df_tracks <- read.table("local_data/df_tracks.txt")
head(df_tracks)

#journey1

df1 <- data.frame( lat = df_tracks$lat[1:8], lng = df_tracks$lng[1:8],
                  pointcode = seq(1,8))

leaflet(data = df1) %>% addTiles() %>%
  addCircleMarkers(~lng, ~lat, label = ~as.character(pointcode))

#journey2 #TODO: lavorare in automatico con journey id

df2 <- data.frame( lat = df_tracks$lat[9:83], lng = df_tracks$lng[9:83],
                   pointcode = seq(1,75))

leaflet(data = df2) %>% addTiles() %>%
  addCircleMarkers(~lng, ~lat, label = ~as.character(pointcode), radius=3)

# Se caricate troppa roba e vi si impalla il Viewer, 
# usate la scopettina per cancellare tutto