#add coloumn tracks

library(leaflet)
library(sf)
library(rgdal)
library(sp)


#########################################################################################
my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/export") , 
  layer="export2",
  verbose=FALSE
)

dep_data <- data.frame( Longitude=df_trips_pos$dep_lng, Latitude=df_trips_pos$dep_lat )


coordinates(dep_data) <- ~ Longitude + Latitude
proj4string(dep_data) <- proj4string(my_map)

district <- over(dep_data,my_map)
head(district)
district <- district[,c(1,5)]


df_trips_district <- cbind(df_trips_pos, c(district))
names(df_trips_district)[names(df_trips_district) == "X_id"] <- "dep_ID"
names(df_trips_district)[names(df_trips_district) == "name"] <- "dep_zone"


############################################################################################
# arrivals

arr_data <- data.frame( Longitude=df_trips_pos$arr_lng, Latitude=df_trips_pos$arr_lat )


coordinates(arr_data) <- ~ Longitude + Latitude
proj4string(arr_data) <- proj4string(my_map)

district <- over(arr_data,my_map)
head(district)
district <- district[,c(1,5)]


df_trips_district <- cbind(df_trips_district, c(district))
names(df_trips_district)[names(df_trips_district) == "X_id"] <- "arr_ID"
names(df_trips_district)[names(df_trips_district) == "name"] <- "arr_zone"


#### step 2: cleaning the dataset

df_trips_district <- df_trips_district[!is.na(df_trips_district$dep_ID),]
df_trips_district <- df_trips_district[!is.na(df_trips_district$arr_ID),]


#### Step 3: counter.

