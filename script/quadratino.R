squareCoords = function(i,j,gridmap,max_N,min_N,max_E,min_E,n){
  N_step = (max_N - min_N)/n
  E_step = (max_E - min_E)/n

  N_min_square = min_N+N_step*(i-1)
  N_max_square = min_N+N_step*i
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

mm=rotated_map
which(mm == max(mm), arr.ind = TRUE)
mario = squareCoords(i=13,j=6,gridmap = mm, max_N,min_N,max_E,min_E,n=40)

mario2 = squareCoords(i=0,j=0,gridmap = mm, max_N,min_N,max_E,min_E,n=40)
leaflet(data = df) %>% addTiles() %>%
  addRectangles(lng1 =min(mario$lng), lng2 = max(mario$lng), lat1 = min(mario$lat), lat2 = max(mario$lat))%>%
  addRectangles(lng1 =min(mario2$lng), lng2 = max(mario2$lng), lat1 = min(mario2$lat), lat2 = max(mario2$lat))
