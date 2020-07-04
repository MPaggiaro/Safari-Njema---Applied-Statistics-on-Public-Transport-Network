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

df_new_archive <- df_new

# 3 - 12
# 12 - 19
# 19 - 3


#h2 <- ifelse( df_new$dep_hour >= 13 & df_new$dep_hour <= 19, 1, 0)
#h3 <- ifelse( df_new$dep_hour >= 20 & df_new$dep_hour <= 3, 1, 0)

w1 <- ifelse( df_new$day%%7 == 4 | df_new$day%%7 == 5, 1, 0) #dummy WEEKEND

attach(df_new)

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
df_new <- cbind(df_new_archive, n_stop = dummy_stop, d_stop = stop_dur, slow = lento, h = h)

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

# diagnosis
par(mfrow=c(2,2))
plot(fit3)
# strange behaviour.

z0 <- data.frame(distance = 5000, n_stop = 0, d_stop=3000, h = 1, slow = 0)
Pred <- predict(fit3, z0, interval ='prediction', level = 0.95)
Conf <- predict(fit3, z0, interval ='confidence', level = 0.95)

# graphical representation:
# conversions:
# distance: kilometers.
distance <- distance/1000

# duration: minutes.
duration <- duration/60
dev.off()

plot(distance, duration)

# next model:
logduration <- log(duration)
logdistance <- log(distance)

# plot of the logs:
plot(logdistance, logduration, col="white")
lento <- as.factor(lento)
levels(lento)
points(logdistance[which(lento==1)], logduration[which(lento==1)], col="blue")
points(logdistance[which(lento==0)], logduration[which(lento==0)], col="green3")
legend('bottomright',c('Lenti', 'Veloci'), lty=1,col =c('blue', 'green'))

# clear but trivial interpretation.
# Let's try with weekends:
plot(logdistance, logduration, col="black")
w1 <- as.factor(w1)
levels(w1)
points(logdistance[which(w1==1)], logduration[which(w1==1)], col="blue")
points(logdistance[which(w1==0)], logduration[which(w1==0)], col="green3")
legend('bottomright',c('Weekend', 'Weekdays'), lty=1,col =c('blue', 'green'))
# weekend and weekdays hanno la stessa distribuzione distance vs duration
# Interesting!

# simple fitting:
simple <- lm(logduration ~ logdistance)
summary(simple)

# diagnostics:
par(mfrow=c(2,2))
plot(simple)

shapiro.test(residuals(simple))
# results:

graphics.off()
plot(logdistance, logduration, col="black")
coef.log= simple$coef
abline(coef.log[1],coef.log[2], lwd=2,col='red')

X.new.log <- data.frame(logdistance = seq(min(logdistance), max(logdistance), len=100))

IC.log <-predict(simple ,X.new.log,interval="confidence",level=0.95)
matplot(X.new.log,IC.log,add=T,type='l',col=c('black','blue','blue'),lwd=2,lty=2)

IP.log <-predict(simple ,X.new.log,interval="prediction",level=0.95)
matplot(X.new.log,IP.log,add=T,type='l',col=c('black','green','green'),lwd=2,lty=2)

legend('bottomright', legend=c('regression line','confidence intervals','prediction intervals'),
       col=c('black','blue','green'), lwd=2, cex=0.85)


# plot of the original variables:
plot(distance, duration, lwd=1)
IC <- exp(IC.log)
IP <- exp(IP.log)
X.new <- exp(X.new.log)
matplot(X.new,IC,add=T,type='l',col=c('black','blue','blue'),lwd=2,lty=2)
matplot(X.new,IP,add=T,type='l',col=c('black','green','green'),lwd=2,lty=2)
legend('bottomright', legend=c('regression line','confidence intervals','prediction intervals'),
       col=c('black','blue','green'), lwd=2, cex=0.85)

# let's try with hours of the day:

# we need to select the actual trips from center to airport, not just
# walkarounds.
# distance center - airport: 10 km.
# Let's cut the trips longer than 15kms
distance_short <- distance[which(distance <= 15)]
duration_short <- duration[which(distance <= 15)]  

df_cut <- df_new[which(distance <= 15),]

plot(distance_short, duration_short)  
plot(log(distance_short), log(duration_short))
     
log.lm <- lm(log(duration_short) ~ log(distance_short))
summary(log.lm)
coef.log= log.lm$coef
abline(coef.log[1],coef.log[2], lwd=2,col='red')

# diagnostics:
par(mfrow=c(2,2))
plot(log.lm)
shapiro.test(residuals(log.lm))

# strange values: very fast!
df_cut[607,]
df_cut[626,]

df_cut$distance <- df_cut$distance/1000
df_cut$duration <- df_cut$duration/60

# let's compute the average speed:
speed <- df_cut$distance/(df_cut$duration/60)
graphics.off()
plot(speed)
# let's select the people moving with regular cars:

head(df_cut)
df_cut <- cbind(df_cut, speed)
head(df_cut)
df_cut2 <- df_cut[which(speed < 80),]

# plot
dev.off()
plot(log(df_cut$distance), log(df_cut$duration))
df_cut2$speed
df_cut2[which(df_cut2$speed<5.5),]
