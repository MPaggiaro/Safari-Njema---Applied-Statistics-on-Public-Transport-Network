# inferenza geostatistica:

# first try: just a single day.
## Clear the workspace

rm(list=ls())

## Load spatial packages

library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

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
