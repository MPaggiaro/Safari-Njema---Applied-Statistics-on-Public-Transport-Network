library(leaflet)
library(sp)
library(rgdal)

# import data
df_tracks <- read.table("local_data/df_tracks.txt", header=T)

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
tracks_3 <- tracks_3[!is.na(tracks_3$Zone_ID),]

# over the days, count:
tracks_3_night <- tracks_3[tracks_3$hour <= 5,]
tracks_3_morning <- tracks_3[tracks_3$hour >=6 & tracks_3$hour <=9,]
tracks_3_midday <-tracks_3[tracks_3$hour >=10 & tracks_3$hour <=14,]
tracks_3_anoon <-tracks_3[tracks_3$hour >=15 & tracks_3$hour <=18,]
tracks_3_evening <-tracks_3[tracks_3$hour >=19 & tracks_3$hour <=24,]



N = 180-30
morning = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_m_daily <- tracks_3_morning[tracks_3_morning$day==i,]
  tracks_3_m_daily <- tracks_3_m_daily[!duplicated(tracks_3_m_daily$id),]
  morning[i-30] <- dim(tracks_3_m_daily)[1]
}

hist(morning, breaks = 20)
hist ( (morning - mean(morning)) /sd(morning), breaks = 20 )
plot(morning)

night = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_n_daily <- tracks_3_night[tracks_3_night$day==i,]
  tracks_3_n_daily <- tracks_3_n_daily[!duplicated(tracks_3_n_daily$id),]
  night[i-30] <- dim(tracks_3_n_daily)[1]
}

# hist(night, breaks = 20)
# hist ( (night - mean(night)) /sd(night), breaks = 20 )
# plot(night[1:50])

morning = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_m_daily <- tracks_3_morning[tracks_3_morning$day==i,]
  tracks_3_m_daily <- tracks_3_m_daily[!duplicated(tracks_3_m_daily$id),]
  morning[i-30] <- dim(tracks_3_m_daily)[1]
}
###

midday = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_n_daily <- tracks_3_midday[tracks_3_midday$day==i,]
  tracks_3_n_daily <- tracks_3_n_daily[!duplicated(tracks_3_n_daily$id),]
  midday[i-30] <- dim(tracks_3_n_daily)[1]
}


anoon = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_n_daily <- tracks_3_anoon[tracks_3_anoon$day==i,]
  tracks_3_n_daily <- tracks_3_n_daily[!duplicated(tracks_3_n_daily$id),]
  anoon[i-30] <- dim(tracks_3_n_daily)[1]
}


evening = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_n_daily <- tracks_3_evening[tracks_3_evening$day==i,]
  tracks_3_n_daily <- tracks_3_n_daily[!duplicated(tracks_3_n_daily$id),]
  evening[i-30] <- dim(tracks_3_n_daily)[1]
}

total = night + morning + midday + anoon + evening

# frequencies over the total day traffic:
fr_night = night/total
fr_morning = morning/total
fr_midday = midday/total
fr_anoon = anoon/total
fr_evening = evening/total

# saving the data in a dataset:
traffic.time.slots <- data.frame(night,fr_night,morning,fr_morning,midday,fr_midday,anoon,fr_anoon,evening,fr_evening,total)


