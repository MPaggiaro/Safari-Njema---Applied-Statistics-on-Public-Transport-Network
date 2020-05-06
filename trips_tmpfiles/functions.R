#utility functions for tmp files


# my_map <- readOGR(
#   dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") ,
#   layer="Maputo5distr",
#   verbose=FALSE
# )

#INPUT : 
#- DATASET of type TRIPS
#- shapefile with the desires region, with data organized as in Maputo5distr.shp

add_zones_labels<- function (df_trips_pos, my_map){

  # departures
  dep_data <- data.frame( Longitude=df_trips_pos$dep_lng, Latitude=df_trips_pos$dep_lat )


  coordinates(dep_data) <- ~ Longitude + Latitude
  proj4string(dep_data) <- proj4string(my_map)

  district <- over(dep_data,my_map)
  district <- district[,c(2,1)]


  df_trips_district <- cbind(df_trips_pos, c(district))
  names(df_trips_district)[names(df_trips_district) == "Zone_ID"] <- "dep_ID"
  names(df_trips_district)[names(df_trips_district) == "name"] <- "dep_zone"

  # arrivals

   arr_data <- data.frame( Longitude=df_trips_pos$arr_lng, Latitude=df_trips_pos$arr_lat )
   coordinates(arr_data) <- ~ Longitude + Latitude
   proj4string(arr_data) <- proj4string(my_map)

   district <- over(arr_data,my_map)
   head(district)
   district <- district[,c(2,1)]

  df_trips_district <- cbind(df_trips_district, c(district))
  names(df_trips_district)[names(df_trips_district) == "Zone_ID"] <- "arr_ID"
  names(df_trips_district)[names(df_trips_district) == "name"] <- "arr_zone"
  
  return(df_trips_district)

}


