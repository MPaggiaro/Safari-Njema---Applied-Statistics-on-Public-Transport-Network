# inferenza geostatistica:

# first try: just a single day.
## Clear the workspace

rm(list=ls())

## Load spatial packages

library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(leaflet)
library(sf)
library(rgdal)
library(sp)
library(ggplot2)
library(raster)
library(dplyr)

library(cppRouting) # DA INSTALLARE

# Let's analyze a first data:
# reading data (e.g. day 51)
tracks <- NULL
for (i in 51:51){
  filename <- paste0("local_data/Tracks/tracks_", i, ".txt")
  tmpfil <- read.table(filename, header=T)
  #tmpfil <- tmpfil[which((tmpfil$hour <= 10) & (tmpfil$hour >= 7)),]
  tracks <- rbind(tracks, tmpfil)
}

# head(tracks)
# plot(tracks$speed)

# simple variogram:
# Let's build the dataset.
# very skewed!

# hist(tracks$speed)
# hist(log(tracks$speed))
# plot(log(tracks$speed))

# should we clean data and evaluate only the moving points?
tracks_moving <- tracks[which(tracks$stop_here==0),]

# hist(log(tracks_moving$speed))
# plot(log(tracks_moving$speed))

# Now they seem very fit and well-shaped. Let's provide a variogram.

coordinates(tracks_moving) <- c('lng', 'lat')

# Estimation of the variogram:
svgm <- variogram(log(speed) ~ 1, tracks_moving)
# plot(variogram(log(speed) ~ 1, tracks_moving, 
#                alpha = c(0,45,90,135)), main = 'Sample Variogram',pch=19)
# #zonal anisotropy:
# plot(svgm, pch=19)

v<-fit.variogram(svgm, vgm(0.7, "Sph", 0.05, 0.6))
# plot(svgm, v, pch=19)

# Let's build a grid:
#loading map
my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") , 
  layer="Maputo5distr",
  verbose=FALSE
)

# plot all together:
# x11()
# plot(my_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1, usePolypath=FALSE)
# plot(tracks_moving, add=TRUE)

my_map <- st_as_sf(my_map)
grid <- my_map %>% 
  st_make_grid(cellsize = 0.004, what = "centers") %>% # grid of points
  st_intersection(my_map) 
x11()
ggplot() +
  geom_sf(data = my_map) +
  geom_sf(data = grid)

# 
##
###


#grid <- SpatialPointsDataFrame(grid)
# st_write(grid,"gridtry.txt", layer_options = "GEOMETRY=AS_XY")
grid_save <- grid
grid <- do.call(rbind, st_geometry(grid)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
grid2 <- read.csv("grid.csv", header = T)
grid2 <- as.data.frame(grid2)
grid2 <- SpatialPointsDataFrame(grid, data = grid2)
#colnames(grid2)<-c('lng','lat')
head(grid2)
#coordinates(grid2)<- c('lng', 'lat')
grid2 <- as(grid2, 'SpatialPixelsDataFrame')

# prediction:
g.tr <- gstat(formula = log(speed) ~ 1, data = tracks_moving, model = v)
summary(g.tr)

# Ordinary Krieging:
my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") , 
  layer="Maputo5distr",
  verbose=FALSE
)
lz.ok <- predict(g.tr, grid2, BLUE = FALSE)

x11()
spplot(lz.ok)

# plot all together:

### -------------------------------- ###
# Shortest Path (symmetric weights)
### -------------------------------- ###


# BUILDING DATAFRAME

sz <- dim(grid)[1] #number of nodes (points in the grid)
df <- data.frame(lz.ok@coords)
colnames(df) <- c("lon", "lat")
df <- cbind(df[,1:2], vel = exp(lz.ok$var1.pred), idx = 1:sz)


# df$lon <- round(df$lon, 6)
# df$lat <- round(df$lat, 6)

dlat <- unique(df$lat[1:100])[2] - unique(df$lat[1:100])[1] #stupid way to compute delta lat
dlon <- unique(df$lon[1:100])[2] - unique(df$lon[1:100])[1] #stupid way to compute delta lon

arc <- NULL

# LUNGHINO...
for (i in 1:(sz-1)){
  for (j in (i+1):sz){
    #same longitude and 1 unit difference in latitude 
    if ( (abs(df[i,1] - df[j,1]) < 1e-10) & ( abs( abs(df[i,2] - df[j,2]) - dlat) < 1e-10) ){ #avoid rounding errors
      arc <- rbind(arc, c(i,j,df[i,3]/2 + df[j,3]/2) )
    }
    #same latitude and 1 unit difference in longitude
    if ( ( abs( abs(df[i,1] - df[j,1]) - dlon) < 1e-10) & ( abs( df[i,2] - df[j,2] ) < 1e-10) ){ #avoid rounding errors
      arc <- rbind(arc, c(i,j,df[i,3]/2 + df[j,3]/2) )
    }
  }
}

colnames(arc) <- c("node1", "node2", "avg_val")
arc

arc_new <- arc
arc_new[,3] <- 1/arc[,3] #use inverse of speed (i.e. more or less "time", since the grid is equispaced)



graph1 <- makegraph(arc_new, directed = FALSE)
sp_nodes <- get_path_pair(graph1, from = 811, to = 487, algorithm = "Dijkstra") #shortest path
sp_idx <- sort( as.numeric(sp_nodes[[1]]) )



sp_grid <- st_as_sf(grid_save[sp_idx])

x11()
ggplot() +
  geom_sf(data = grid_save) +
  geom_sf(data = sp_grid, col="red")




### -------------------------------- #####
# Shortest Path ("exiting" weights)
### -------------------------------- ###

# 
# # BUILDING DATAFRAME
# sz <- dim(df)[1] #number of nodes (points in the grid)
# df <- data.frame(lz.ok@coords)
# colnames(df) <- c("lon", "lat")
# df <- cbind(df[,1:2], vel = lz.ok$var1.pred, idx = 1:sz)
# 
# # df$lon <- round(df$lon, 6)
# # df$lat <- round(df$lat, 6)
# 
# dlat <- unique(df$lat[1:100])[2] - unique(df$lat[1:100])[1] #stupid way to compute delta lat
# dlon <- unique(df$lon[1:100])[2] - unique(df$lon[1:100])[1] #stupid way to compute delta lon
# 
# arc <- NULL
# 
# # LUNGHINO...
# for (i in 1:sz){
#   for (j in 1:sz){
#     #same longitude and 1 unit difference in latitude 
#     if ( (abs(df[i,1] - df[j,1]) < 1e-10) & ( abs( abs(df[i,2] - df[j,2]) - dlat) < 1e-10) ){ #avoid rounding errors
#       arc <- rbind(arc, c(i,j,df[i,3]) )
#     }
#     #same latitude and 1 unit difference in longitude
#     if ( ( abs( abs(df[i,1] - df[j,1]) - dlon) < 1e-10) & ( abs( df[i,2] - df[j,2] ) < 1e-10) ){ #avoid rounding errors
#       arc <- rbind(arc, c(i,j,df[i,3]) )
#     }
#   }
# }
# 
# colnames(arc) <- c("node1", "node2", "avg_val")
# arc
# 
# arc_new <- arc
# arc_new[,3] <- 1/arc[,3] #use inverse of speed (i.e. more or less "time", since the grid is equispaced)
# 
# library(cppRouting) # DA INSTALLARE
# {
#   graph1 <- makegraph(arc_new, directed = FALSE)
#   sp_nodes <- get_path_pair(graph1, from = 377, to = 459, algorithm = "Dijkstra") #shortest path
#   sp_idx <- sort( as.numeric(sp_nodes[[1]]) )
#   
#   sp_grid <- st_as_sf(grid[sp_idx])
#   
#   x11()
#   ggplot() +
#     geom_sf(data = grid) +
#     geom_sf(data = sp_grid, col="green")
# }
# 


## -------------------- ##
#find nearest point

tmp <- do.call(rbind, st_geometry(grid_save)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

tmp <- cbind(tmp[,1:2], idx = 1:dim(tmp)[1])
x0 <- data.frame(x =  32.588, y = -25.974)


mindist <- 1e18
minidx <- 0

for (ii in 1:dim(tmp)[1]){
  newdist <- sum( (x0 - tmp[ii,1:2])^2 )
  if (newdist < mindist){
    mindist <- newdist
    minidx <-ii
  }
}
minidx



#find neighbors
my_idx <- 479
{
vec <- rep(0,4) #N S E W

for (i in 1:874){
  if ( (abs(df[my_idx,1] - df[i,1]) < 1e-10) & ( abs( abs(df[my_idx,2] - df[i,2]) - dlat) < 1e-10)
       & ( df[my_idx,2] < df[i,2] ) ){ # NORTH
      vec[1] <- i
  }
  
  if ( (abs(df[my_idx,1] - df[i,1]) < 1e-10) & ( abs( abs(df[my_idx,2] - df[i,2]) - dlat) < 1e-10)
       & ( df[my_idx,2] > df[i,2] ) ){ # SOUTH
    vec[2] <- i
  }
  
  if ( ( abs( abs(df[my_idx,1] - df[i,1]) - dlon) < 1e-10) & ( abs( df[my_idx,2] - df[i,2] ) < 1e-10) 
       &  ( df[my_idx,1] < df[i,1] ) ){ #EAST
    vec[3] <- i
  }
  
  if ( ( abs( abs(df[my_idx,1] - df[i,1]) - dlon) < 1e-10) & ( abs( df[my_idx,2] - df[i,2] ) < 1e-10) 
       & ( df[my_idx,1] > df[i,1] ) ){ #WEST
    vec[4] <- i
  }
}

}

# ORDINE: 377, 376, 375, 781, 766, 751, 736, 329, 311, 705, 292, 690, 272, 252, 233, 651, 214
# 639, 628, 177, 159, 140, 139, 121, 102, 85, 68, 53, 54, 38, 23, 12, 11, 10, 9, 8, 3, 2
# 424, 423, 410, 397, 387, 518, 384, 382, 500, 499, 488, 477, 467, 459

new_idx <- c(377, 376, 375, 781, 766, 751, 736, 329, 311, 705, 292, 690, 272, 252, 233, 651, 214,
             639, 628, 177, 159, 140, 139, 121, 102, 85, 68, 53, 54, 38, 23, 12, 11, 10, 9, 8, 3, 2,
             424, 423, 410, 397, 387, 518, 384, 382, 500, 499, 488, 477, 467,459)

# ORDINE: 811, 810, 809, 797, 782, 767, 752, 737, 722, 707, 692, 678, 665, 653, 641, 630, 621, 613,
# 612, 605, 598, 592, 586, 580, 574, 570, 565, 564, 559, 874, 553, 552, 547, 546, 540, 857, 850,
# 841, 842, 834, 835, 836, 831, 832, 497, 498, 487, 488, 489, 478, 479, 469, 470, 471, 463

# USA inizio = 811, fine = 487
new_idx <- c(811, 810, 809, 797, 782, 767, 752, 737, 722, 707, 692, 678, 665, 653, 641, 630, 621, 613,
             612, 605, 598, 592, 586, 580, 574, 570, 565, 564, 559, 874, 553, 552, 547, 546, 540, 857, 850,
             841, 842, 834, 835, 836, 831, 832, 497, 498, 487, 488, 489, 478, 479, 469, 470, 471, 463)

new_grid <- st_as_sf(grid_save[new_idx])

spot_idx <- c()
spot_grid <- st_as_sf(grid_save[spot_idx])

x11()
ggplot() +
  geom_sf(data = grid_save) +
  geom_sf(data = new_grid, col="orange") + 
  geom_sf(data = sp_grid, col="green")
  

