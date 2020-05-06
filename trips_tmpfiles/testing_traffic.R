#
tracks <- read.table("local_data/Tracks/tracks_71.txt", header=T)

# run import_data

# including map

my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") , 
  layer="Maputo5distr",
  verbose=FALSE
)


#df_tracks_districts<-df_tracks
dat <- data.frame( Longitude=df_tracks$lng, Latitude=df_tracks$lat )


coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)

district <- over(dat,my_map)

df_tracks <- cbind(df_tracks, c(district))

# selection of zone 3:
tracks_3 <- df_tracks[df_tracks$Zone_ID == 3,]

# over the days, count:
tracks_3_night <- tracks_3[tracks_3$hour <= 5,]
tracks_3_morning <- tracks_3[tracks_3$hour >=6 & tracks_3$hour <=9,]

N = 180-30
morning = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_m_daily <- tracks_3_morning[tracks_3_morning$day==i,]
  tracks_3_m_daily <- tracks_3_m_daily[!duplicated(tracks_3_morning$id),]
  morning[i-30] <- dim(tracks_3_m_daily)[1]
}