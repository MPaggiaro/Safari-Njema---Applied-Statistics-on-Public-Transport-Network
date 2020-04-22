##############################################################################################
#
# Utility.R: Contiene tutte le FUNZIONI utili da chiamare in da life
#
##############################################################################################

# Librerie usate
library(leaflet)
library(magrittr)


# Funzioni utili 
full_import_data = function(){
  source(file = "script/import_data.R")
}


cut_dataset_by_id = function(df, cut_size, seed=0){
  # Taglia df selezionando random un numero di utenti pari a cut_size (si può impostare il seed del random sampling)
  max_id = max(df$id) # numero totale di utenti
  set.seed(seed) # setto il seed (default = 0)
  selected_users = sample(1:max_id, cut_size, replace=F) # selezione random degli utenti senza doppioni
  df_cutted = df[ df$id %in% selected_users, ] # taglio il dataset
  return(df_cutted)
}


plot_starting_points = function(df_pos){
  
  ## Plottare punti di partenza dei primi 10 trips, senza doppioni ##
  
  df = data.frame( dep_lat = df_pos$dep_lat, dep_lng = df_pos$dep_lng,
                    depName = df_pos$depName) #creo il dataframe da stampare
  df <- df[!duplicated(df$depName),]
  
  leaflet(data = df) %>% addTiles() %>%
    addProviderTiles(providers$OpenStreetMap)  %>% 
    addMarkers(~df$dep_lng, ~df$dep_lat, label = ~as.character(df$depName), clusterOptions = markerClusterOptions())
}

plot_arrival_points = function(df_pos){
  
  ## Plottare punti di partenza dei primi 10 trips, senza doppioni ##
  
  df = data.frame( arr_lat = df_pos$arr_lat, arr_lng = df_pos$arr_lng,
                   arrName = df_pos$arrName) #creo il dataframe da stampare
  df <- df[!duplicated(df$arrName),]
  
  leaflet(data = df) %>% addTiles() %>%
    addProviderTiles(providers$OpenStreetMap)  %>% 
    addMarkers(~df$arr_lng, ~df$arr_lat, label = ~as.character(df$arrName), clusterOptions = markerClusterOptions())
}

plot_connections = function(df_pos){
  
  ## NOT WORKING YET
  
  df = data.frame(group = c("dep", "arr"),
                      lat = c(df_pos$dep_lat, df_pos$arr_lat),
                      lng = c(df_pos$dep_lng, df_pos$arr_lng))
  leaflet()%>%
    addTiles() %>%
    addPolylines(data = df, lng = ~lng, lat = ~lat, group = ~group)
}




