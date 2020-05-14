#source("script/import_data.R", encoding = "UTF-8")

#source("script/utility.R", encoding = "UTF-8")


library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(plyr)
library(sp)
library(raster)
library(rgdal)


trip_to_tracks = function(trip_row, df_tracks)
{
  # Prende una riga del df trips e restituisce tutti i tracks associati 
  return(df_tracks[which(df_tracks$day == trip_row$day & df_tracks$journey_id == trip_row$journey_id),])
}


#mappa mascaretti
district_map <- readOGR( 
  dsn= paste0(getwd(),"/DistrictMap") , 
  layer="HomogenousDivisionOK",
  verbose=FALSE
)

#aggiungo tutti i distretti al tracks

points <- data.frame( east=df_tracks$Easting, north=df_tracks$Northing )
coordinates(points) <- ~ east + north
proj4string(points) <- proj4string(district_map)

district <- over(points,district_map)

#add the columns with the district id
df_tracks_districts <- cbind(df_tracks, c(district))
colnames(df_tracks_districts)[colnames(df_tracks_districts)=="fid"] <- "district_id"
head(df_tracks_districts)

## cos'è un viaggio lento?

hist(df_trips_pos$distance[which(df_trips_pos$distance < 50000)], breaks = 200)


mean(df_trips_pos$distance)
sqrt(var(df_trips_pos$distance))
max(df_trips_pos$distance)

v = df_trips_pos$distance/df_trips_pos$duration
quantile(v, c(0,0.2))*3.6 # il 20% più lento va a meno di 5.2km/h

#estraggo i trips più lenti 
slow_trips = df_trips_pos[which(v<5.1583/3.6),]
slow_tracks = NULL

a = df_tracks$day*100000+df_tracks$journey_id
b = slow_trips$day*100000+slow_trips$journey_id
slow_tracks = df_tracks_districts[  a%in%b, ]

points <- data.frame( east=slow_tracks$Easting, north=slow_tracks$Northing )
coordinates(points) <- ~ east + north
proj4string(points) <- proj4string(district_map)
district_slow <- over(points,district_map)

x11()

#plot slow 
map_new_format <- st_read("DistrictMap/HomogenousDivisionOK.shp")
head(map_new_format)
plot(district_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1 )
count_districts <- count(district_slow, "cat")
map_new_format$cat <- as.factor(map_new_format$cat)
count_districts$cat<- as.factor(count_districts$district_id)
map_and_data <- left_join(map_new_format,count_districts)
qtm(map_and_data, "freq")

#plot all
x11()
map_new_format <- st_read("DistrictMap/HomogenousDivisionOK.shp")
head(map_new_format)
par(mar=c(0,0,0,0))
plot(district_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1 )
count_districts <- count(district, "cat")
map_new_format$cat <- as.factor(map_new_format$cat)
count_districts$cat<- as.factor(count_districts$district_id)
map_and_data <- left_join(map_new_format,count_districts)

qtm(map_and_data, "freq")


all_districts = sort(unique(df_tracks_districts$district_id[!is.na(df_tracks_districts$district_id)]))
speed <- empty_list <- vector(mode = "list")
tutti = NULL
for (distr_id in all_districts)
{
  ids = df_tracks_districts[which(df_tracks_districts$district_id == distr_id),]
  ids = ids$day*100000+ids$journey_id
  ids = unique(ids)
  trips_ids = df_trips_pos$day*100000+df_trips_pos$journey_id
  harambe = df_trips_pos[ trips_ids %in% ids, ]
  row_k = harambe$distance / harambe$duration
  speed[[distr_id]] = row_k
  tutti = append(tutti,row_k)
}

tutti_gaussiani = tan(4*tutti)
qqnorm(tutti_gaussiani)
qqline(tutti_gaussiani)
tutti_gaussiani_hist = tutti_gaussiani[tutti_gaussiani>-10 & tutti_gaussiani<10]
hist(tutti_gaussiani_hist,breaks=200)
mu_tot = mean(tutti_gaussiani)
var_tot = var(tutti_gaussiani)


##### Ve la butto lì, what if la nostra distribuzione è una t-Student? #####
# con le normali, le code sono troppo magre... # yesss

district_distribution = function (speed_row){
tutti_gaussiani = tan(speed_row)
tutti_gaussiani_hist = tutti_gaussiani[tutti_gaussiani>-10 & tutti_gaussiani<10]
h <- hist(tutti_gaussiani_hist, breaks=200, density = 10,
          col = "lightgray", xlab = "Accuracy", main = "Overall")

xfit <- seq(min(tutti_gaussiani_hist), max(tutti_gaussiani_hist), length = 40) 


yfit <- dt(xfit, df=1) 
yfit <- yfit * diff(h$mids[1:2]) * length(tutti_gaussiani_hist)
lines(xfit, yfit, col = "red", lwd = 2)
return(shapiro.test(speed_row))
}
 #ex
district_distribution(speed[[20]])


#test sulla media della velocità media nei distretti
#vengono 15k occorrenze di diverso comportamento, è ragionevole perchè basta per esempio
#che siano due diversi tipi di comportamento (e.g. 150 per ognuno) e si hanno 30k distretti di comportamento diverso
#andrà rifatto tutto sui distretti agglomerati ----- son diverse le varianze non sappiamo bene come fare

speed_clean = empty_list
speed_tutti = NULL
id = NULL
j=1
for (i in 1:405){
  if ( lengths(speed)[i]>1 ) {
  speed_clean[[j]] = speed[[i]]
  speed_tutti = append(speed_tutti, speed_clean[[j]])
  id = append(id,rep(j,lengths(speed)[i]))
  j=j+1}
}

data <- data.frame(speed_tutti, id)
bartlett.test(speed_clean)
id<-as.factor(id)
attach(data)
summary(data)


fit <- aov( speed_tutti~ id )

summary(fit)

n       <- length(id)      # total number of obs.
ng      <- table(id)       # number of obs. in each group
treat   <- levels(id)      # levels of the treatment
g       <- length(treat)    # number of levels (i.e., of groups)


k <- g*(g-1)/2

Media   <- mean(speed_tutti)
Mediag  <- tapply(speed_tutti, id, mean)

SSres <- sum(residuals(fit)^2)

S <- SSres/(n-g)

alpha=0.05

ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
  }
}

Auni <- matrix(0,g,g)
for(i in 1:g) {
  for(j in i:g) {
    Auni[i,j] <- Mediag[i]-Mediag[j] + qt(1-alpha/2, n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] ) )}
  for(j in 1:i) {
    Auni[i,j] <- Mediag[j]-Mediag[i] - qt(1-alpha/2, n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] ) )}
  Auni[i,i]     <- 0
}
P <- matrix(0,g,g)
for(i in 1:g) {
  for(j in i:g) {
    P[i,j] <- (1-pt(abs((Mediag[i]-Mediag[j]) / sqrt( S * ( 1/ng[i] + 1/ng[j] ) ) ), n-g))*2}
  for(j in 1:i) {
    P[i,j] <- (1-pt(abs((Mediag[i]-Mediag[j]) / sqrt( S * ( 1/ng[i] + 1/ng[j] ) ) ), n-g))*2}
  P[i,i]     <- 0
}

p=NULL
for (i in 1:321){
  for (j in 1:321){
    if (j>i){
      p =append(p,P[i,j])
    }

  }
}

p.bonf <- p.adjust(p, 'bonf') 

alpha = 0.05
p.bonf_alpha <-p.bonf[which(p.bonf<alpha)]
alpha = 0.01
p.bonf_alpha <-p.bonf[which(p.bonf<alpha)]


######################################################################################################################

find_stopping_perc = function(fascia_oraria, df_tracks, df_trips){
  
  df_tracks_fo = df_tracks[which(df_tracks$hour>= fascia_oraria[1] & df_tracks$hour<=fascia_oraria[2]),];
  
  a = df_tracks_fo$day*100000+df_tracks_fo$journey_id
  b = df_trips$day*100000+df_trips$journey_id
  df_trips = df_trips[ b%in%a, ]
  
  
  stopping_perc=NULL
  for(i in 1:dim(df_trips)[1])
  {
    linked_tracks = trip_to_tracks(df_trips[i,], df_tracks_fo)
    n_stop = dim(linked_tracks[which(linked_tracks$stop_here == 1 ),])[1]
    dist = df_trips[i,]$distance
    stopping_perc = append(stopping_perc, n_stop/dist)
  }
  
  return(stopping_perc)
}


orari = list( c(0,5), c(6,9), c(10,14), c(15,18), c(19,24) )

stperc_05 = find_stopping_perc(orari[[1]], df_tracks, df_trips_pos)
stperc_69 = find_stopping_perc(orari[[2]], df_tracks, df_trips_pos)
stperc_1014 = find_stopping_perc(orari[[3]], df_tracks, df_trips_pos)
stperc_1518 = find_stopping_perc(orari[[4]], df_tracks, df_trips_pos)
stperc_1924 = find_stopping_perc(orari[[5]], df_tracks, df_trips_pos)


log_stperc_05  =log(stperc_05[ stperc_05 < quantile(stperc_05,0.95)[[1]] ])
log_stperc_69  =log(stperc_05[ stperc_69 < quantile(stperc_69,0.95)[[2]] ])
log_stperc_1014  =log(stperc_05[ stperc_1014 < quantile(stperc_1014,0.95)[[3]] ])
log_stperc_1518  =log(stperc_05[ stperc_1518 < quantile(stperc_1518,0.95)[[4]] ])
log_stperc_1924  =log(stperc_05[ stperc_1924 < quantile(stperc_1924,0.95)[[5]] ])

list(log_stperc_05,log_stperc_69,log_stperc_1014,log_stperc_1518,log_stperc_1924)





























