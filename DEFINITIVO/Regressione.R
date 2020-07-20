library(leaflet)
library(sf)
library(rgdal)
library(sp)
library(car)
rm(list=ls())

## ---------------- ##
## INITIAL LOADINGS ##
## ---------------- ##

{
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

df_new = df_new[, c(3,10,16,17,18,19,20,21)]

df_new_archive <- df_new

}

# ---------------- #
# COMPUTING N_STOP

trip_to_tracks = function(trip_row, df_tracks)
{
  # Prende una riga del df trips e restituisce tutti i tracks associati 
  return(df_tracks[which(df_tracks$day == trip_row$day & df_tracks$journey_id == trip_row$journey_id),])
}

max_list <- NULL
#numero di stop

avg_speed <- NULL
#velocità media


for ( i in (1:dim(df_new)[1]) ){
  
  tmp <- trip_to_tracks(df_new[i,], df_tracks )
  aux<- tmp$stop_here[2: dim(tmp)[1] ] - tmp$stop_here[1: (dim(tmp)[1]-1) ]
  max_list[i] <- length(which(aux >0)) + tmp$stop_here[1]
  
  avg_speed[i] <- mean(tmp$speed)
}

max_list -> n_stop

#dummy sulla velocità: 2.5 m/s ~ 10 km/h
lento <- ifelse( avg_speed<=2.5, 1, 0 )

# dummy persona che si è fermata molte volte
dummy_stop <- ifelse( max_list<=4, 1, 0 )


detach(df_new)


## ------------------------------------ ##
## ---------- MODELLO FINALE ---------- ##
## ------------------------------------ ##

attach(df_new)

#w1 <- ifelse( df_new$day%%7 == 4 | df_new$day%%7 == 5, 1, 0) #dummy WEEKEND
we_s <- ifelse(df_new$day%%7 == 5, 1, 0) #dummy WEEKEND -sunday
df_new<-cbind(df_new[,1:8], n_stop = dummy_stop, we_s, slow = lento)

logduration <- log(duration)
logdistance <- log(distance)


#fit_log = lm( logduration ~ logdistance + I(cos((dep_hour+12)*2*pi/24 ) +1) + n_stop + d_stop + we + logdistance:n_stop + 
#d_stop:n_stop + logdistance:d_stop +  logdistance:slow + we:logdistance)

#summary(fit_log) 


fit_log = lm( logduration ~ logdistance + I(cos((dep_hour+12)*2*pi/24 ) +1) + we_s + df_new$n_stop + df_new$slow + we_s:logdistance)
summary(fit_log) 

# first test: weekend (Sunday) influences?
A <- rbind(c(0,0,0,1,0,0,0), c(0,0,0,0,0,0,1))
b <- c(0,0)
linearHypothesis(fit_log, A, b)
#no , pval=0.1766    # RIMUOVO IMPATTO WEEKEND


# CONFRONTO: conviene tenere LOGD^2 ??

fit_log = lm( logduration ~ logdistance  + I(cos((dep_hour+12)*2*pi/24 ) +1) + df_new$n_stop  + df_new$slow)
summary(fit_log) #R2adj0.539

fit_log = lm( logduration ~ logdistance  + I(logdistance^2)+ I(cos((dep_hour+12)*2*pi/24 ) +1) + df_new$n_stop  + df_new$slow)
summary(fit_log) 
#R2adj0.5419

# (teniamo logd^2)

# diagnosis
par(mfrow=c(2,2))
plot(fit_log)
shapiro.test(residuals(fit_log))


## ------------------------------------------------------ ## 
## Insert the result from cluster (instead of slow)

df_tragitto_clustered <- read.table("DEFINITIVO/my_df_tragitto_clustered.txt")

p_walk <-NULL
p_chapas<-NULL

for ( i in (1:dim(df_new)[1]) ){
  #per ogni riga (cioè un trip), ne calcolo il trip id
  trip_id_temp<- df_new$day[i] * 100000 + df_new$journey_id[i] 
  #selezione il tragitto con il clustered dei segmenti
  temp<-df_tragitto_clustered[df_tragitto_clustered$trip_id == trip_id_temp, ]
  #calcolo percentualke di viaggio fatta a piedi
  
  n_walk<- length(which(temp$clust3 == 'walk'))
  p_walk[i]<- n_walk/length(temp$clust3)
  n_chapas<- length(which(temp$clust3 == 'chapas'))
  p_chapas[i]<- n_chapas/length(temp$clust3)
 
}

df_new<- cbind(df_new[,1:11], p_walk, p_chapas) #percentage of trip in walk/chapas



fit_log = lm( logduration ~ logdistance  + I(logdistance^2)+ I(cos((dep_hour+12)*2*pi/24 ) +1) +
              df_new$n_stop + df_new$slow  + p_walk + p_walk:logdistance  )
summary(fit_log) 


# first test: p_walk influences?
A <- rbind(c(0,0,0,0,0,0,0,1), c(0, 0,0,0,0,0,1,0))
b <- c(0,0)
linearHypothesis(fit_log, A, b)
#pval 2.2e-16 *** : si 


#Tolgo slow perchè, secondo me, non ha senso nell'interpretazione del modello
fit_log = lm( logduration ~ logdistance  + I(logdistance^2)+ I(cos((dep_hour+12)*2*pi/24 ) +1) + n_stop + p_walk  )
summary(fit_log) # R2ajd : 0.57 


#Seleziono anche quanta % viaggio vorrei fare con la chapas
fit_log = lm( logduration ~ logdistance  + I(logdistance^2)+ I(cos((dep_hour+12)*2*pi/24 ) +1) + n_stop + p_walk + p_chapas  )
summary(fit_log) # R2ajd : 0.5779


# diagnosis
par(mfrow=c(2,2))
plot(fit_log)
shapiro.test(residuals(fit_log))
#OK


## ------------------------------------------------------ ## 
#Predizione: 
z0 <- data.frame(logdistance = log(5000), n_stop = 0, dep_hour=8, p_walk=0.1, p_chapas=0.9)
Pred <- predict(fit_log, z0, interval ='prediction', level = 0.95)
Conf <- predict(fit_log, z0, interval ='confidence', level = 0.95)

#In the original space: viaggio in minuti (???)

Pred
Conf
exp(Pred)/60
exp(Conf)/60


###########################################################
###########################################################