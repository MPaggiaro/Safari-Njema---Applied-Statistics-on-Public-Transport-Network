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

graphics.off()

# Let's analyze a first data:
# reading data (e.g. day 51)
tracks <- read.table("local_data/Tracks/tracks_51.txt", header=T)
head(tracks)
plot(tracks$speed)

# simple variogram:
# Let's build the dataset.
# very skewed!
hist(tracks$speed)
hist(log(tracks$speed))
plot(log(tracks$speed))

# should we clean data and evaluate only the moving points?
tracks_moving <- tracks[which(tracks$stop_here==0),]
hist(log(tracks_moving$speed))
plot(log(tracks_moving$speed))
# Now they seem very fit and well-shaped. Let's provide a variogram.

coordinates(tracks_moving) <- c('lng', 'lat')

# Estimation of the variogram:
svgm <- variogram(log(speed) ~ 1, tracks_moving)
plot(variogram(log(speed) ~ 1, tracks_moving, 
               alpha = c(0,45,90,135)), main = 'Sample Variogram',pch=19)
# zonal anisotropy:
plot(svgm, pch=19)

v<-fit.variogram(svgm, vgm(0.7, "Sph", 0.05, 0.6))
plot(svgm, v, pch=19)

# Let's build a grid:
#loading map
my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") , 
  layer="Maputo5distr",
  verbose=FALSE
)

# plot all together:
plot(my_map, col="#f2f2f2", bg="skyblue", lwd=0.25, border=1 )
plot(tracks_moving, add=TRUE)

my_map <- st_as_sf(my_map)
grid <- my_map %>% 
  st_make_grid(cellsize = 0.004, what = "centers") %>% # grid of points
  st_intersection(my_map)    
ggplot() +
  geom_sf(data = my_map) +
  geom_sf(data = grid)

grid <- as.data.frame(grid)
grid <- SpatialPointsDataFrame(grid)
st_write(grid,"gridtry.txt", layer_options = "GEOMETRY=AS_XY")
grid2 <- read.csv("grid.csv", header = T)
colnames(grid2)<-c('lng','lat')
head(grid2)
coordinates(grid2)<- c('lng', 'lat')
#grid2 <- as(grid2, 'SpatialPixelsDataFrame')

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
spplot(lz.ok[,1])

# plot all together:

