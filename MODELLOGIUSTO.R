library(leaflet)
library(sf)
library(rgdal)
library(sp)

#loading dataframes
if (!exists("df_tracks")){
  df_tracks <- read.table("local_data/df_tracks.txt", header=T)
}

#loading dataframe
if (!exists("df_trips_pos")){
  df_trips_pos <- read.table("local_data/df_trips_pos.txt", header=T)
}

#loading map
my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") , 
  layer="Maputo5distr",
  verbose=FALSE
)



## plotting the map
par(mar=c(0,0,0,0))
plot(my_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1 )


dat <- data.frame( Longitude=df_trips_pos$dep_lng, Latitude=df_trips_pos$dep_lat )
coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)

# cercare dove sono i punti
localize_df <- over(dat,my_map)
elems <- df_trips_pos[which(localize_df$Zone_ID == 3 ),]


hom_div_areas<- readOGR( 
  dsn= paste0(getwd(),"/HomogenousDivisionOK") , 
  layer="HomogenousDivisionOK",
  verbose=FALSE
)

points <- data.frame( east=elems$arr_east, north=elems$arr_north )
coordinates(points) <- ~ east + north
proj4string(points) <- proj4string(hom_div_areas)

district <- over(points,district_map)
df_trips_districts <- cbind(elems, c(district))
df_new = df_trips_districts[ which(df_trips_districts$fid == 33), ]

#max( df_trips_districts[(!is.na(df_trips_districts$fid)),22] )

df_new = df_new[, c(3,10,16,17,18,19,20,21)]

df_new_archive <- df_new

# 3 - 12
# 12 - 19
# 19 - 3


#h2 <- ifelse( df_new$dep_hour >= 13 & df_new$dep_hour <= 19, 1, 0)
#h3 <- ifelse( df_new$dep_hour >= 20 & df_new$dep_hour <= 3, 1, 0)

w1 <- ifelse( df_new$day%%7 == 4 | df_new$day%%7 == 5, 1, 0) #dummy WEEKEND

attach(df_new)

fit = lm( duration ~  h1 + h2 + w1 + distance + h1:distance + h2:distance + w1:distance + I(distance^2) )#+ I(h1*distance^2) + I(h2*distance^2) + I(w1*distance^2) )
summary(fit)

A <- rbind(c(0,1,0,0,0,0,0,0,0), c(0,0,1,0,0,0,0,0,0))
b <- c(0,0)
linearHypothesis(fit,A,b)

#A <- rbind(c(0,0,0,1,0,0,0,0,0), c(0,0,0,0,0,0,0,0,1))
#b <- c(0,0)
#linearHypothesis(fit,A,b)

A <- rbind(c(0,0,0,1,0,0,0,0,0))
b <- c(0)
linearHypothesis(fit,A,b)

fit2 = lm( duration ~ w1 + distance + I(distance^2) )#+ I(h1*distance^2) + I(h2*distance^2) + I(w1*distance^2) )
summary(fit2)

########################################################################

trip_to_tracks = function(trip_row, df_tracks)
{
  # Prende una riga del df trips e restituisce tutti i tracks associati 
  return(df_tracks[which(df_tracks$day == trip_row$day & df_tracks$journey_id == trip_row$journey_id),])
}

max_list <- NULL
stop_dur <- NULL
avg_speed <- NULL
for ( i in (1:dim(df_new)[1]) ){
  tmp <- trip_to_tracks(df_new[i,], df_tracks )
  max_list[i] <- max(tmp$stop_count)
  stop_dur[i] <- 0
  if(dim(tmp)[1] > 1){
    for( j in (1: (dim(tmp)[1]-1) ) ){
      if ( tmp$stop_duration[j+1] == 0 ){
        stop_dur[i] <- stop_dur[i] +  tmp$stop_duration[j]
      }
    }
    stop_dur[i] <- stop_dur[i] +  tmp$stop_duration[ dim(tmp)[1] ]
  }
  avg_speed[i] <- mean(tmp$speed)
}

lento <- ifelse( avg_speed<=2.5, 1, 0 )
dummy_stop <- ifelse( max_list<=4, 1, 0 )
#hol <- ifelse(df_new$day>dd,1,0)

detach(df_new)

h <- ifelse( df_new$dep_hour < 6 , 1, 0)
df_new <- cbind(df_new_archive, n_stop = dummy_stop, d_stop = stop_dur, slow = lento, h = h, hol = hol)

attach(df_new)

# BEST PER ORA
#fit3 = lm( duration ~ log(distance) + n_stop + d_stop + I(log(distance)*n_stop) + 
#             d_stop:n_stop + I(n_stop^2) )
#summary(fit3)

fit3 = lm( duration ~ distance + n_stop + d_stop + slow + distance:n_stop + 
                         d_stop:n_stop + distance:d_stop + distance:slow + n_stop:slow)

fit3 = lm( duration ~ distance + h + n_stop + d_stop + distance:n_stop + 
             d_stop:n_stop + distance:d_stop + distance:slow + h:distance)

print ( summary(fit3) )

z0 = data.frame(distance = 5000, n_stop = 0, d_stop=3000, h = 1, slow = 0)
Pred <- predict(fit3, z0, interval ='prediction', level = 0.95)
Conf <- predict(fit3, z0, interval ='confidence', level = 0.95)
