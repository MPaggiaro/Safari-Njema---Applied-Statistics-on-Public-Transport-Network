##############################################################################################
#
# Utility2.R : funzioni utili da combinare con Utility.R 
#
##############################################################################################

library(leaflet)
library(rgdal)

#Maputo city administrative area (province) - in Shapefile
maputoProvinces_admin_areas<-readOGR("Shapefile/Moz.shp")

#Maputo city administrative area (city) - in Shapefile
maputoCity_admin_areas<-readOGR("Shapefile/MOZ5_boundaries.shp")

#Maputo Quartieri (Shapefile fatto da Pit) - in District Map
maputoCityDistricts<-readOGR("DistrictMap/Maputo_Quartieri.shp")

#Funzioni 

#Plot dei punti di partenza in rosso, di arrivo in blu e una linea che connette arrivo e partenza. 
#NB: Passare un dataset piccolo affichè sia significativo (consiglio di NON provare con df_trips completo, ci mette un sacco per niente)

plot_connections = function(df_pos){
  
  
  m <-leaflet(data = df_pos) %>% addTiles(options = providerTileOptions(minZoom = 11)) %>%
    addCircleMarkers(~df_pos$dep_lng, ~df_pos$dep_lat, radius = .2, color = 'red')%>%
    addCircleMarkers(~df_pos$arr_lng, ~df_pos$arr_lat, radius = .2, color = 'blue')%>%
    addPolylines(data=maputoCity_admin_areas, color = "grey", weight = 2, smoothFactor = 1, opacity = 1.0, fillOpacity = 0.1)%>%
    addPolylines(data=maputoCityDistricts, color = "grey", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.1)
    
  for(i in 1:nrow(df_pos)){
    
    m <- addPolylines(m, lat = c(df_pos$dep_lat[i], df_pos$arr_lat[i]), 
                      lng = c(df_pos$dep_lng[i], df_pos$arr_lng[i]), weight = .3, smoothFactor = 0.1, opacity = 1.0, fillOpacity = 0.1 )
  } 
  
  m 
  
}


#Plot dei punti di partenza dall'area N in rosso, di arrivo in blu e una linea che connette arrivo e partenza. 
#INPUT:  df_pos = dataset trips WITH pos 
#        N= nome luogo di partenza, i.e. depName=N (N integer !!)

plot_connections_fromDepName = function(df_pos, N){
  
  #controlla che N esista in df
  if (! is.element(N, levels(factor(df_trips_pos$depName)))) {
    stop("Departure location does not exists in the given datset")
  }
  
  #Partiziona il dataseta seconda del luogo di partenza
  df_pos<-df_pos[df_pos$depName == N , ]
  
  m <-leaflet(data = df_pos) %>% addTiles(options = providerTileOptions(minZoom = 11)) %>%
    addCircleMarkers(~df_pos$dep_lng, ~df_pos$dep_lat, radius = 3, color = 'red')%>%
    addCircleMarkers(~df_pos$arr_lng, ~df_pos$arr_lat, radius = .2, color = 'blue')%>%
    addPolylines(data=maputoCity_admin_areas, color = "grey", weight = 2, smoothFactor = 1, opacity = 1.0, fillOpacity = 0.1)%>%
    addPolylines(data=maputoCityDistricts, color = "grey", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.1)
  
  for(i in 1:nrow(df_pos)){
    
    m <- addPolylines(m, lat = c(df_pos$dep_lat[i], df_pos$arr_lat[i]), 
                      lng = c(df_pos$dep_lng[i], df_pos$arr_lng[i]), weight = .3, smoothFactor = 0.1, opacity = 1.0, fillOpacity = 0.1 )
  } 
  
  m 
  
}



#Plot dei punti di arrivo nell'area N in blue, di partenza in rosso e una linea che connette arrivo e partenza. 
#INPUT:  df_pos = dataset trips WITH pos 
#        N= nome luogo di partenza, i.e. depName=N (N integer !!)

plot_connections_toArrName = function(df_pos, N){
  
  #controlla che N esista in df
  if (! is.element(N, levels(factor(df_trips_pos$arrName)))) {
    stop("Departure location does not exists in the given datset")
  }
  
  #Partiziona il dataseta seconda del luogo di partenza
  df_pos<-df_pos[df_pos$arrName == N , ]
  
  m <-leaflet(data = df_pos) %>% addTiles(options = providerTileOptions(minZoom = 11)) %>%
    addCircleMarkers(~df_pos$dep_lng, ~df_pos$dep_lat, radius = .2, color = 'red')%>%
    addCircleMarkers(~df_pos$arr_lng, ~df_pos$arr_lat, radius = 3, color = 'blue')%>%
    addPolylines(data=maputoCity_admin_areas, color = "grey", weight = 2, smoothFactor = 1, opacity = 1.0, fillOpacity = 0.1)%>%
    addPolylines(data=maputoCityDistricts, color = "grey", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.1)
  
  for(i in 1:nrow(df_pos)){
    
    m <- addPolylines(m, lat = c(df_pos$dep_lat[i], df_pos$arr_lat[i]), 
                      lng = c(df_pos$dep_lng[i], df_pos$arr_lng[i]), color='#e34a33' , weight = .3, smoothFactor = 0.1, opacity = 1.0, fillOpacity = 0.1 )
  } 
  
  m 
  
}


#Plot dei punti di PARTENZA con cluster e per fascia oraria
#INPUT:  df_pos = dataset trips 
#        from.H = ora di inizio  della fascia orario presa in considerazione
#        to.H   = ora di fine della fascia orario presa in considerazione

plot_DeparturePoints_Byhours<-function(df_pos, from.H, to.H){
  
  title <- tags$p(tags$style("p {color:  black; font-size:13px}"),
                     tags$b(paste0("Departure points from h: ", from.H , " to h:", to.H)))
  
  
  dep<- df_pos[(df_pos$dep_hour >= from.H) & (df_trips_pos$dep_hour < to.H), ]
  
  leaflet(data = dep) %>% addTiles(options = providerTileOptions(minZoom = 11)) %>%
    addCircleMarkers(~dep$dep_lng, ~dep$dep_lat, radius = .1,
                     clusterOptions = markerClusterOptions(freezeAtZoom = 11, removeOutsideVisibleBounds = TRUE))%>%
    addPolylines(data=maputoCity_admin_areas, color = "grey", weight = 2, smoothFactor = 1, opacity = 1.0, fillOpacity = 0.1)%>%
    addPolylines(data=maputoCityDistricts, color = "grey", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.1)%>%
    addControl(title, position = "bottomright" )
}





#Plot dei punti di ARRIVO con cluster e per fascia oraria
#INPUT:  df_pos = dataset trips 
#        from.H = ora di inizio  della fascia orario presa in considerazione
#        to.H   = ora di fine della fascia orario presa in considerazione

plot_ArrivalPoints_Byhours<-function(df_pos, from.H, to.H){
  
  title <- tags$p(tags$style("p {color:  black; font-size:13px}"),
                  tags$b(paste0("Arrival points from h: ", from.H , " to h:", to.H)))
  
  
  aux<- df_pos[(df_pos$arr_hour >= from.H) & (df_trips_pos$arr_hour < to.H), ]
  
  leaflet(data = aux) %>% addTiles(options = providerTileOptions(minZoom = 11)) %>%
    addCircleMarkers(~aux$dep_lng, ~aux$dep_lat, radius = .1,
                     clusterOptions = markerClusterOptions(freezeAtZoom = 11, removeOutsideVisibleBounds = TRUE))%>%
    addPolylines(data=maputoCity_admin_areas, color = "grey", weight = 2, smoothFactor = 1, opacity = 1.0, fillOpacity = 0.1)%>%
    addPolylines(data=maputoCityDistricts, color = "grey", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.1)%>%
    addControl(title, position = "bottomright" )
}



#Plot in sequenza dei punti di partenza con cluster e per fascia oraria (dt=1 ora)

#NOT WORKING ???? (non capisco perchè)
plot_DeparturePoints_EachHour<-function(df_pos){
  
  for (i in 1:24){  plot_DeparturePoints_Byhours(df_pos, i, i-1)}
}





