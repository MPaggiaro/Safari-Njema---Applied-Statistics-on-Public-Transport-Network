
library(leaflet)
library(rgdal)

library(plyr)


#Change CRS convention and plot

hom_div_areas<- readOGR( 
  dsn= paste0(getwd(),"/HomogenousDivisionOK") , 
  layer="HomogenousDivisionOK",
  verbose=FALSE
)

all.districts <- spTransform(hom_div_areas, CRS("+proj=longlat +datum=WGS84"))

city_center.idx<- which(all.districts $Pattern ==1)
city_center<- all.districts[city_center.idx,]
head(city_center)

leaflet(city_center) %>%addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3, label = ~fid, labelOptions = labelOptions(noHide=T))

# PROVA: study distribution in CityCenter Area in a fixed day

city_center$fid
df_city_center_flows<-df_trips_pos[df_trips_pos$depName %in% city_center$fid & df_trips_pos$arrName %in% city_center$fid , ]
n<- length(city_center$fid)

#partenza righe/ arrivo colonne
d.city_center_flows<-matrix(0, n,  n) 
colnames(d.city_center_flows)<-city_center$fid
rownames(d.city_center_flows)<-city_center$fid

#select a day (50)

df_city_center_flows.50<-df_city_center_flows[df_city_center_flows$day == 50, ]

for (i in city_center$fid){
   arr<- df_city_center_flows.50[df_city_center_flows.50$depName == i, grep("arrName", colnames(df_trips))]
   f<- count(arr)
     for (j in f$x){
      d.city_center_flows[paste0(i),paste0(j)]<-f[f$x==j,2]
     }
  }

d.city_center_flows.50<-d.city_center_flows
d.city_center_flows.50

#day 51
d.city_center_flows<-matrix(0, n,  n) 
colnames(d.city_center_flows)<-city_center$fid
rownames(d.city_center_flows)<-city_center$fid


df_city_center_flows.51<-df_city_center_flows[df_city_center_flows$day == 51, ]

for (i in city_center$fid){
  arr<- df_city_center_flows.51[df_city_center_flows.51$depName == i, grep("arrName", colnames(df_trips))]
  f<- count(arr)
  for (j in f$x){
    d.city_center_flows[paste0(i),paste0(j)]<-f[f$x==j,2]
  }
}

d.city_center_flows.51<-d.city_center_flows
d.city_center_flows.51

chisq.test(as.vector(d.city_center_flows.50), as.vector(d.city_center_flows.51)) #INESATTA

