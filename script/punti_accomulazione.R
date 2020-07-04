# Chapas?


#setwd("/Users/pietro/Documents/appstat_project")
#rm(list = ls()) #cleaning environment

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

###### PARTENZA: ###### 
{
dat <- data.frame( Longitude=df_trips_pos$dep_lng, Latitude=df_trips_pos$dep_lat )
coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)




# cercare dove sono i punti
localize_df <- over(dat,my_map) #tutti i punti che sono in df_track sonon localizzati nei 5 distretti

n<-20

#prendo solo i rilevamenti delle PARTENZE del dataframe in una delle 5 zone
#toglie i NA

elems <- df_trips_pos[which(!is.na(localize_df$Zone_ID)),]

elems <- df_trips_pos[which( localize_df$Zone_ID == 3 ),]#<--- VARIAZIONE SOLO UNA ZONA

elems <- elems[which( elems$dep_hour > 11 & elems$dep_hour <= 14 ),]

# plottare i punti
# points(elems$lat ~ elems$lng, col = "red", cex = 1.5)

# defining the extrema of area
max_N <- max(elems$dep_north) 
min_N <- min(elems$dep_north) 
max_E <- max(elems$dep_east) 
min_E <- min(elems$dep_east) 


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
  AAA <- which(vec_h < elems$dep_north[i] )[1] -1 
  #prendo il primo parallelo strettamente sotto e torno indietro di 1
  
  #seleziono il primo "meridiano" t.c. il mio punto si trova a destra
  BBB <- which(vec_v > elems$dep_east[i])[1] -1
  #prendo il primo parallelo strettamente a destra e torno indietro di 1
  
  #colloco il punto nel quadratino corrispondente e aumento il counter
  density_grid[AAA,BBB] = density_grid[AAA,BBB] + 1
  
  #salvo le coordinate nella matrice per uso futuro
  n_riga[i] = AAA
  n_colonna[i] = BBB
}






#identifying the zone on the map

max_lat <- elems[elems$dep_north == max_N,]$dep_lat[1]
min_lat <- elems[elems$dep_north == min_N,]$dep_lat[1]
max_lng <- elems[elems$dep_east == max_E,]$dep_lng[1]
min_lng <- elems[elems$dep_east== min_E,]$dep_lng[1]

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
}

######### ARRIVI  ######
{
  
#Punti di partenza: 

dat <- data.frame( Longitude=df_trips_pos$arr_lng, Latitude=df_trips_pos$arr_lat )
coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)

n<-20


# cercare dove sono i punti
localize_df <- over(dat,my_map) #tutti i punti che sono in df_track sonon localizzati nei 5 distretti



#prendo solo i rilevamenti delle PARTENZE del dataframe in una delle 5 zone
#toglie i NA

elems <- df_trips_pos[which(!is.na(localize_df$Zone_ID)),]
elems <- df_trips_pos[which( localize_df$Zone_ID == 3 ),]
elems<- elems[which( elems$dep_hour > 11 & elems$dep_hour <= 14 ),]


# plottare i punti
# points(elems$lat ~ elems$lng, col = "red", cex = 1.5)

# defining the extrema of area
max_N <- max(elems$arr_north) 
min_N <- min(elems$arr_north) 
max_E <- max(elems$arr_east) 
min_E <- min(elems$arr_east) 


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
  AAA <- which(vec_h < elems$dep_north[i] )[1] -1 
  #prendo il primo parallelo strettamente sotto e torno indietro di 1
  
  #seleziono il primo "meridiano" t.c. il mio punto si trova a destra
  BBB <- which(vec_v > elems$dep_east[i])[1] -1
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

max_lat <- elems[elems$arr_north == max_N,]$arr_lat[1]
min_lat <- elems[elems$arr_north == min_N,]$arr_lat[1]
max_lng <- elems[elems$arr_east == max_E,]$arr_lng[1]
min_lng <- elems[elems$arr_east== min_E,]$arr_lng[1]

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


}


#######  RISTORANTE #####
{

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
}


