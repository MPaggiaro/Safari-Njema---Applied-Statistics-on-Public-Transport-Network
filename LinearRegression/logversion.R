library(leaflet)
library(sf)
library(rgdal)
library(sp)
library(car)

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
  dsn= paste0(getwd(),"/DistrictMap") , 
  layer="HomogenousDivisionOK",
  verbose=FALSE
)

points <- data.frame( east=elems$arr_east, north=elems$arr_north )
coordinates(points) <- ~ east + north
proj4string(points) <- proj4string(hom_div_areas)

district <- over(points,hom_div_areas)
df_trips_districts <- cbind(elems, c(district))
df_new = df_trips_districts[ which(df_trips_districts$fid == 33), ]

#max( df_trips_districts[(!is.na(df_trips_districts$fid)),22] )

df_new = df_new[, c(3,10,16,17,18,19,20,21)]

anoon <- ifelse( df_new$dep_hour >= 13 & df_new$dep_hour <= 19, 1, 0)
morning <- ifelse( df_new$dep_hour >= 4 & df_new$dep_hour < 13, 1, 0)

we <- ifelse( df_new$day%%7 == 4 | df_new$day%%7 == 5, 1, 0) #dummy WEEKEND

df_new$distance <- df_new$distance/1000
df_new$duration <- df_new$duration/60

speed <- df_new$distance/(df_new$duration/60)
df_new <- cbind(df_new, speed,anoon, morning, we)
graphics.off()
plot(speed)
df_new <- df_new[which(speed < 60),]
# linear model:
log.duration <- log(df_new$duration)
log.distance <- log(df_new$distance)
fm <- lm(log.duration ~ log.distance + df_new$anoon + df_new$morning + 
           df_new$we + log.distance:df_new$anoon +
           log.distance:df_new$morning + log.distance:df_new$we)
summary(fm)

# diagnostics:
par(mfrow=c(2,2))
plot(fm)
shapiro.test(residuals(log.lm))

# we need to reduce the model:

# first test: weekend influences?
A <- rbind(c(0,0,0,0,1,0,0,0), c(0,0,0,0,0,0,0,1))
b <- c(0,0)
linearHypothesis(fm, A, b)

# second test: morning?
A <- rbind(c(0,0,0,1,0,0,0,0), c(0,0,0,0,0,0,1,0))
b <- c(0,0)
linearHypothesis(fm, A, b)
# doesn't influence!
# reduce morning!
fm2 <- lm(log.duration ~ log.distance + df_new$anoon + df_new$we 
          + log.distance:df_new$anoon + log.distance:df_new$we)
summary(fm2)

# third test: afternoon?
A <- rbind(c(0,0,1,0,0,0), c(0,0,0,0,1,0))
b <- c(0,0)
linearHypothesis(fm2, A, b)
# doesn't influence!
# let's reduce again.
fm3 <- lm(log.duration ~ log.distance + + df_new$we + log.distance:df_new$we)
summary(fm3)

# again, weekend?
A <- rbind(c(0,0,1,0), c(0,0,0,1))
b <- c(0,0)
linearHypothesis(fm3, A, b)
# we cannot reject with 1% significancy. Maybe collinearity?

# With the chosen model, we can predict.