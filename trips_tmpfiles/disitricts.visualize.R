
#plot quartieri per capire quali aggregare 

library(leaflet)
library(rgdal)


hom_div_areas<- readOGR( 
  dsn= paste0(getwd(),"/HomogenousDivisionOK") , 
  layer="HomogenousDivisionOK",
  verbose=FALSE
)

levels(hom_div_areas$Pattern_No)
levels(hom_div_areas$Pattern)


all.districts <- spTransform(hom_div_areas, CRS("+proj=longlat +datum=WGS84"))

leaflet(all.districts) %>%addTiles() %>%
  addPolylines(color = "#444444", weight = 1, smoothFactor = 0.5,
               opacity = 1.0, fillOpacity = 0.5)


#City center classified districts

city_center.idx<- which(all.districts $Pattern ==1)
city_center<- all.districts[city_center.idx,]


leaflet(city_center) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))
 
#18-quartieri_pianificati_bassa_densit�

quartieri.idx<- which(all.districts$Pattern ==10)
quartieri<- all.districts[quartieri.idx,]

quartieri$Pattern_No


leaflet(quartieri) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))%>%
  addControl(title, position = "bottomright" )


#2-quartieri_informali

quartieri.idx<- which(all.districts$Pattern ==2)
quartieri<- all.districts[quartieri.idx,]

quartieri$Pattern_No

leaflet(quartieri) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))
 

#3-quartieri_suburbani
quartieri.idx<- which(all.districts$Pattern ==3)
quartieri<- all.districts[quartieri.idx,]

quartieri$Pattern_No

leaflet(quartieri) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))



# 4-quartieri_non_pianificati_suburbani
quartieri.idx<- which(all.districts$Pattern ==4)
quartieri<- all.districts[quartieri.idx,]

quartieri$Pattern_No

leaflet(quartieri) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))


#9-quartieri_pianificati_suburbani
quartieri.idx<- which(all.districts$Pattern ==5)
quartieri<- all.districts[quartieri.idx,]

quartieri$Pattern_No

leaflet(quartieri) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))

#10-quartieri_pianificati-blocchi-quadrati 
quartieri.idx<- which(all.districts$Pattern ==6)
quartieri<- all.districts[quartieri.idx,]

quartieri$Pattern_No

leaflet(quartieri) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))


#11-zona_di_completamento
quartieri.idx<- which(all.districts$Pattern == 7)
quartieri<- all.districts[quartieri.idx,]

quartieri$Pattern_No

leaflet(quartieri) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))


#15-zona_prevalentemente_rurale
quartieri.idx<- which(all.districts$Pattern == 8)
quartieri<- all.districts[quartieri.idx,]

quartieri$Pattern_No

leaflet(quartieri) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))



#16-quartieri_non_pianificati_bassa_densità

quartieri.idx<- which(all.districts$Pattern == 9)
quartieri<- all.districts[quartieri.idx,]

quartieri$Pattern_No

leaflet(quartieri) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))


