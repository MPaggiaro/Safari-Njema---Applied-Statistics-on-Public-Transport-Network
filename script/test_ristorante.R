### ORARIO PRANZO - ZONE 3 ####

#density_grid partenze centro orario

matxMax <- function(mtx)
{
  colmn <- which(mtx == max(mtx)) %/% nrow(mtx) + 1
  row <- which(mtx == max(mtx)) %% nrow(mtx)
  return( matrix(c(row, colmn), 1))
}


maxCords <- matxMax(density_grid)
maxCords
density_grid[maxCords]

threshold= quantile(density_grid,.8)


indx<-which(density_grid > threshold, arr.ind = TRUE)



# funzione per trovare le coordineate di un un quadratino sulla matrice rotated_map
squareCoords = function(i,j,gridmap,max_N,min_N,max_E,min_E,n){
  N_step = (max_N - min_N)/n
  E_step = (max_E - min_E)/n
  
  N_min_square = max_N-N_step*(i-1)
  N_max_square = max_N-N_step*i
  E_min_square = min_E+E_step*(j-1)
  E_max_square = min_E+E_step*j
  
  NO=data.frame(E=E_min_square,N=N_max_square)
  NE=data.frame(E=E_max_square,N=N_max_square)
  SO=data.frame(E=E_min_square,N=N_min_square)
  SE=data.frame(E=E_max_square,N=N_min_square)
  tutti=rbind(NO,NE,SO,SE)
  sputm <- SpatialPoints(tutti, proj4string=CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs+ellps+WGS84+towgs84=0,0,0"))  
  spgeo <- data.frame(spTransform(sputm, CRS("+proj=longlat +datum=WGS84")))
  longlat = 
    colnames(spgeo)=c("lng","lat")
  return(spgeo)
  
}


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

# trasformo in CRS
#my_map1 <- spTransform(my_map, 
#                       CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m 
#                        +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## plotting the map
par(mar=c(0,0,0,0))
plot(my_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1 )


dat <- data.frame( Longitude=df_tracks$lng, Latitude=df_tracks$lat )
coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)




# cercare dove sono i punti
localize_df <- over(dat,my_map) #tutti i punti che sono in df_track sonon localizzati nei 5 distretti

n<-20

#prendo solo i rilevamenti delle PARTENZE del dataframe in una delle 5 zone
#toglie i NA

elems <- df_tracks[which((localize_df$Zone_ID ==3)),]

elems <- elems[which( elems$hour >11 & elems$hour <=14 ),]



# plottare i punti
# points(elems$lat ~ elems$lng, col = "red", cex = 1.5)

# defining the extrema of area

# defining the extrema of area
max_N <- max(elems$Northing) 
min_N <- min(elems$Northing) 
max_E <- max(elems$Easting) 
min_E <- min(elems$Easting) 

# dividing the rectangle just found in a grid

grid_h <- n #number of intervals in horizontal (i.e. number of columns in the grid)
grid_v <- n #number of intervals in vertical (i.e. number of rows in the grid)

vec_v <- seq(min_E, max_E+0.1, length.out = grid_h+1) #ordino da sinistra a destra
vec_h <- seq(max_N, min_N-0.1, length.out = grid_v+1) #ordino dall'alto in basso
# + 0.1 è per debuggare alcuni casi criticissimi del codice sotto

# a questo punto ho una coppia di vettori di coordinate che identificano un rettangolino
# della griglia: in particolare ogni rettangolino è identificato dalla coordinata del
# suo vertice in alto a sinistra

#creo griglia vuota da fillare con il numero di rilevamenti in ogni rettangolino
density_grid <- matrix(0L, nrow = grid_v, ncol=grid_h, )

df_coord <- data.frame(n_riga = rep(0,dim(elems)[1]), n_colonna = rep(0,dim(elems)[1]) )

# ci mette una ventina di secondi
n_riga <- rep( 0,dim(elems)[1] )
n_colonna <- rep( 0,dim(elems)[1] )

for ( i in 1:dim(elems)[1] ) {
  #seleziono il primo "parallelo" t.c. il mio punto si trova sotto
  AAA <- which(vec_h < elems$Northing[i] )[1] -1 
  #prendo il primo parallelo strettamente sotto e torno indietro di 1
  
  #seleziono il primo "meridiano" t.c. il mio punto si trova a destra
  BBB <- which(vec_v > elems$Easting[i])[1] -1
  #prendo il primo parallelo strettamente a destra e torno indietro di 1
  
  #colloco il punto nel quadratino corrispondente e aumento il counter
  density_grid[AAA,BBB] = density_grid[AAA,BBB] + 1
  
  #salvo le coordinate nella matrice per uso futuro
  n_riga[i] = AAA
  n_colonna[i] = BBB
}

#CHECK
which(is.na(n_colonna))
which(is.na(n_riga))


#identifying the zone on the map


max_lat <- elems[elems$Northing == max_N,]$lat[1]
min_lat <- elems[elems$Northing == min_N,]$lat[1]
max_lng <- elems[elems$Easting == max_E,]$lng[1]
min_lng <- elems[elems$Easting == min_E,]$lng[1]



vertici <- data.frame( my_lat = c(max_lat,max_lat,min_lat, min_lat), 
                       my_lng = c(max_lng,min_lng,max_lng, min_lng)
)

df <- data.frame( my_lat = vertici$my_lat, my_lng = vertici$my_lng)


# vettori contenenti i vertici dei quadratini
vec_min_lng = NULL
vec_max_lng = NULL
vec_min_lat = NULL
vec_max_lat = NULL
palette = NULL

# riempio i vettori e creo la palette di colori
pal=colorRampPalette(colors = c("palegreen","yellow", "orange","orangered" ,"red", "red4"))(max(density_grid)+1)
for(i in 1:n){
  for(j in 1:n){
    sqcoord = squareCoords(i,j ,density_grid,max_N,min_N,max_E,min_E,n)
    vec_min_lng = c(vec_min_lng, min(sqcoord$lng))
    vec_max_lng = c(vec_max_lng, max(sqcoord$lng))
    vec_min_lat = c(vec_min_lat, min(sqcoord$lat))
    vec_max_lat = c(vec_max_lat, max(sqcoord$lat))
    palette = c(palette, pal[density_grid[i,j]+1])
  }
}

# piazzo i rettangolini sulla mappa per creare una sorta di heatmap
mappa = leaflet(data = df) %>%addTiles()%>%addProviderTiles(providers$OpenStreetMap)%>%addRectangles(lng1 = vec_min_lng, lng2 = vec_max_lng, lat1 = vec_min_lat, lat2 = vec_max_lat, color = palette)
mappa%>%addCircleMarkers(~df$my_lng, ~df$my_lat)



################################### AREE #########################################

coord<- squareCoords(9,5, density_grid,max_N,min_N,max_E,min_E,n )
coord<-rbind(coord, squareCoords(9,6, density_grid,max_N,min_N,max_E,min_E,n ) )
coord<-rbind(coord, squareCoords(11,5, density_grid,max_N,min_N,max_E,min_E,n ) )
coord<-rbind(coord, squareCoords(11,6, density_grid,max_N,min_N,max_E,min_E,n ) )

NO<- c(lat=max(coord$lat), lng=min(coord$lng))
NE<- c(lat=max(coord$lat), lng=max(coord$lng))
SO<- c(lat=min(coord$lat), lng=min(coord$lng))
SE<- c(lat=min(coord$lat), lng=max(coord$lng))

area1<- t(data.frame(NO, NE, SO, SE))
area1<-data.frame(area1)

# piazzo i rettangolini sulla mappa per creare una sorta di heatmap

# piazzo i rettangolini sulla mappa per creare una sorta di heatmap
mappa = leaflet(data = df) %>%addTiles()%>%addProviderTiles(providers$OpenStreetMap)%>%addRectangles(lng1 = vec_min_lng, lng2 = vec_max_lng, lat1 = vec_min_lat, lat2 = vec_max_lat, color = palette)
mappa%>%addCircleMarkers(~area1$lng, ~area1$lat)



coord2<- squareCoords(15,12, density_grid,max_N,min_N,max_E,min_E,n )
coord2<-rbind(coord2, squareCoords(15,13, density_grid,max_N,min_N,max_E,min_E,n ) )
coord2<-rbind(coord2, squareCoords(17,12, density_grid,max_N,min_N,max_E,min_E,n ) )
coord2<-rbind(coord2, squareCoords(17,13, density_grid,max_N,min_N,max_E,min_E,n ) )

NO<- c(lat=max(coord2$lat), lng=min(coord2$lng))
NE<- c(lat=max(coord2$lat), lng=max(coord2$lng))
SO<- c(lat=min(coord2$lat), lng=min(coord2$lng))
SE<- c(lat=min(coord2$lat), lng=max(coord2$lng))


area2<- t(data.frame(NO, NE, SO, SE))
area2<-data.frame(area2)

# piazzo i rettangolini sulla mappa per creare una sorta di heatmap
mappa = leaflet(data = df) %>%addTiles()%>%addProviderTiles(providers$OpenStreetMap)%>%addRectangles(lng1 = vec_min_lng, lng2 = vec_max_lng, lat1 = vec_min_lat, lat2 = vec_max_lat, color = palette)
mappa%>%addCircleMarkers(~area2$lng, ~area2$lat)



coord3.1<- squareCoords(14,4, density_grid,max_N,min_N,max_E,min_E,n )
coord3.1<-rbind(coord3.1, squareCoords(14,5, density_grid,max_N,min_N,max_E,min_E,n ) )
coord3.2<-squareCoords(15,5, density_grid,max_N,min_N,max_E,min_E,n ) 
coord3.2<-rbind(coord3.2, squareCoords(15,6, density_grid,max_N,min_N,max_E,min_E,n ) )
coord3.3<-squareCoords(16,6, density_grid,max_N,min_N,max_E,min_E,n ) 
coord3.3<-rbind(coord3.3, squareCoords(16,7, density_grid,max_N,min_N,max_E,min_E,n ) )


NO<- c(lat=max(coord3.1$lat), lng=min(coord3.1$lng))
NE<- c(lat=max(coord3.1$lat), lng=max(coord3.1$lng))
SO<- c(lat=min(coord3.1$lat), lng=min(coord3.1$lng))
SE<- c(lat=min(coord3.1$lat), lng=max(coord3.1$lng))


area3.1<- t(data.frame(NO, NE, SO, SE))
area3.1<- data.frame(area3.1)


# piazzo i rettangolini sulla mappa per creare una sorta di heatmap
mappa = leaflet(data = df) %>%addTiles()%>%addProviderTiles(providers$OpenStreetMap)%>%addRectangles(lng1 = vec_min_lng, lng2 = vec_max_lng, lat1 = vec_min_lat, lat2 = vec_max_lat, color = palette)
mappa%>%addCircleMarkers(~area3.1$lng, ~area3.1$lat)


NO<- c(lat=max(coord3.2$lat), lng=min(coord3.2$lng))
NE<- c(lat=max(coord3.2$lat), lng=max(coord3.2$lng))
SO<- c(lat=min(coord3.2$lat), lng=min(coord3.2$lng))
SE<- c(lat=min(coord3.2$lat), lng=max(coord3.2$lng))


area3.2<- t(data.frame(NO, NE, SO, SE))
area3.2<- data.frame(area3.2)


# piazzo i rettangolini sulla mappa per creare una sorta di heatmap
mappa = leaflet(data = df) %>%addTiles()%>%addProviderTiles(providers$OpenStreetMap)%>%addRectangles(lng1 = vec_min_lng, lng2 = vec_max_lng, lat1 = vec_min_lat, lat2 = vec_max_lat, color = palette)
mappa%>%addCircleMarkers(~area3.2$lng, ~area3.2$lat)



NO<- c(lat=max(coord3.3$lat), lng=min(coord3.3$lng))
NE<- c(lat=max(coord3.3$lat), lng=max(coord3.3$lng))
SO<- c(lat=min(coord3.3$lat), lng=min(coord3.3$lng))
SE<- c(lat=min(coord3.3$lat), lng=max(coord3.3$lng))


area3.3<- t(data.frame(NO, NE, SO, SE))
area3.3<- data.frame(area3.3)


# piazzo i rettangolini sulla mappa per creare una sorta di heatmap
mappa = leaflet(data = df) %>%addTiles()%>%addProviderTiles(providers$OpenStreetMap)%>%addRectangles(lng1 = vec_min_lng, lng2 = vec_max_lng, lat1 = vec_min_lat, lat2 = vec_max_lat, color = palette)
mappa%>%addCircleMarkers(~area3.3$lng, ~area3.3$lat)


################## selezione track ################################

inQuadratone = function(point_lat, point_lng, area){
  minLat = min(area$lat)
  maxLat = max(area$lat)
  minLng = min(area$lng)
  maxLng = max(area$lng)
  
  return(point_lat>minLat & point_lat<maxLat & point_lng>minLng & point_lng<maxLng);
}


in3Quadratini = function(point_lat,point_lng, quad1,quad2,quad3){
  
  return(inQuadratone(point_lat,point_lng,quad1)| inQuadratone(point_lat,point_lng,quad2)| inQuadratone(point_lat,point_lng,quad3)
  )
}


df_tracks_area1 = elems[ which(inQuadratone(elems$lat, elems$lng, area1 )), ]
df_tracks_area2 = elems[ which(inQuadratone(elems$lat, elems$lng, area2 )), ]
df_tracks_area3 = elems[ which(in3Quadratini(elems$lat, elems$lng, area3.1, area3.2, area3.3 )), ]


############### dataset per test #####################################


mediaStopDay = function(day, df){
  
  df_day = df[which(df$day == day), ];
  sum = 0
  n = dim(df_day)[1]
  count=0
  
  for(i in 1:(n-1)){
    
    if(df_day$stop_duration[i] > df_day$stop_duration[i+1] | (df_day$id[i]!= df_day$id[i+1] & !(df_day$stop_duration[i]==0)) ){
      count=count+1
      sum = sum+df_day$stop_duration[i]
    }
    
}
  
  return(sum/count)
  
}


vecMedieStop = function(df){
  res = NULL
  
  for(i in unique(df$day)){
    res = c(res,mediaStopDay(i,df))
  }
  
  return(res)
  
}



vec1<- vecMedieStop(df_tracks_area1)
vec2<- vecMedieStop(df_tracks_area2)
vec3<- vecMedieStop(df_tracks_area3)
vec3[ which(is.nan(vec3))] =0

count1<-rep(0, 150)
count2<-rep(0, 150)
count3<-rep(0, 150)
for (i in 1:150){
  
  count1[i]<- length(unique(df_tracks_area1[which(df_tracks_area1$day == (i+30)),]$id))
  count2[i]<- length(unique(df_tracks_area2[which(df_tracks_area2$day == (i+30)),]$id))
  count3[i]<- length(unique(df_tracks_area3[which(df_tracks_area3$day == (i+30)),]$id))

}

v1<- cbind(media_stop= vec1, num_stop=count1)
v2<- cbind(media_stop= vec2, num_stop=count2)
v3<- cbind(media_stop= vec3, num_stop=count3)

######## TEST ####################################

library(car)
load("/Users/maddalenalischetti/Desktop/Applied Stat/Lab 5 - 16042020/mcshapiro.test.RData")

#### t - test univariato #####
# we compute the sample of differences
D <- vec1-vec2
D

#plot the diff vs H0 (graphical representation)
x11()
plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35') #mu0


### T2 Hotelling Test 
# H0: delta == delta.0 vs H1: delta != delta.0
# with delta.0=c(0,0) #the two labs do same analysis

#ASS: Test the Gaussian assumption (on D!)
shapiro.test(D) #multivariate differences in Rp space
hist(D)

#sotto assumzione di TCL: 

t.test(D, alternative = "two.sided" )
t.test(D, alternative = "less" )
#---> sono uguali


D <- vec2-vec3
D
t.test(D, alternative = "two.sided" )
t.test(D, alternative = "less" )



#########  Dati accoppiati #############



# we compute the sample of differences

D <- v1-v2

# D<- v1-v3
# D<- v2-v3

#plot the diff vs H0 (graphical representation)
x11()
plot(D, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35') #mu0


### T2 Hotelling Test 
# H0: delta == delta.0 vs H1: delta != delta.0
# with delta.0=c(0,0) #the two labs do same analysis

#ASS: Test the Gaussian assumption (on D!)
mcshapiro.test(D) #multivariate differences in Rp space

#Settings
n <- dim(D)[1]  
p <- dim(D)[2]  

D.mean   <- colMeans(D)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(0,0) #adjust dim

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

#test
D.T2 < cfr.fisher 

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P


#If dim==2 - plot the ellipsoidal conf region
x11()
plot(D,  pch=1, main='Dataset of the Differences',ylim=c(-15,60))
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)

#plot add mu0
points(delta.0[1], delta.0[2], pch=16, col='red', cex=1.5)
abline(h=delta.0[1], v=delta.0[2], col='grey35')


# Now, let's communicate our results to the client.
# Let's build confidence intervals for linear combination of the
# components of the mean vector

### Simultanouse T2 intervals

IC.T2.1comp <- c( D.mean[1]-sqrt(cfr.fisher*D.cov[1,1]/n) , D.mean[1], D.mean[1]+sqrt(cfr.fisher*D.cov[1,1]/n) )
IC.T2.2comp  <- c( D.mean[2]-sqrt(cfr.fisher*D.cov[2,2]/n) , D.mean[2], D.mean[2]+sqrt(cfr.fisher*D.cov[2,2]/n) )

# la variabile cause del rifiuto è il numero di stop in v1 - v2
# sono entrambe in v1 - v3, e v2 - v3


# Siccome le assunzioni sulla gaussiniatà falliscono, facciamo i test non parametri sulle medie dei vettori D: 

boxplot(D)
matplot(t(D), type='l', lty=1)

# center of simmetry under H0
mu0      <- c(0, 0)

# Computing a proper test statistic
# (i.e., squared distance between the sample mean vector and the hypothesized center of simmetry)
x.mean   <- colMeans(D)
n <- dim(D)[1]
p <- dim(D)[2]

T20 <- as.numeric((x.mean-mu0) %*% (x.mean-mu0) )

# Selecting a proper likelihood-invariant strategy
# (i.e., data point reflections)
# We are assuming that under H0 the data distribution is simmetric

# number of possible data point reflections 
2^n

# number of different values of the test statistic 
2^n/2

# Estimating the permutational distribution under H0
B <- 100000 
T2 <- numeric(B) 


for(perm in 1:B){
  # In this case we use changes of signs in place of permutations
  
  # Permuted dataset
  signs.perm <- rbinom(n, 1, 0.5)*2 - 1
  hum_perm <- mu0 + (D - mu0) * matrix(signs.perm,nrow=n,ncol=p,byrow=FALSE)
  x.mean_perm <- colMeans(hum_perm)
  T2[perm]  <- (x.mean_perm-mu0)  %*% (x.mean_perm-mu0) 
}

# plotting the permutational distribution under H0
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

# p-value
p_val <- sum(T2>=T20)/B
p_val 



# v1 - v2 :
# p_val 
# [1] 0.30499


# v1-v3
#  p_val 
# [1] 5e-05


# v2 - v3 
# p_val 
# [1] 0


########### Seleziono area 1 #############

# piazzo i rettangolini sulla mappa per creare una sorta di heatmap
mappa = leaflet(data = df) %>%addTiles()%>%addProviderTiles(providers$OpenStreetMap)%>%addRectangles(lng1 = vec_min_lng, lng2 = vec_max_lng, lat1 = vec_min_lat, lat2 = vec_max_lat, color = palette)
mappa%>%addCircleMarkers(~area1$lng, ~area1$lat)








