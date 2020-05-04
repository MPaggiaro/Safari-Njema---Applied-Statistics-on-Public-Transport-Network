#rm(list = ls())

if (!exists("df_trips_pos")){
  df_trips_pos <- read.table("local_data/df_trips_pos.txt")
}

#add coloumn tracks

library(leaflet)
library(sf)
library(rgdal)
library(sp)
library(plyr)


#########################################################################################
# including map

my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") , 
  layer="Maputo5distr",
  verbose=FALSE
)

leaflet(my_map) %>%addTiles() %>%
  addPolylines(color = "#444444", weight = 1)

############################################################################################
# departures

dep_data <- data.frame( Longitude=df_trips_pos$dep_lng, Latitude=df_trips_pos$dep_lat )


coordinates(dep_data) <- ~ Longitude + Latitude
proj4string(dep_data) <- proj4string(my_map)

district <- over(dep_data,my_map)
district <- district[,c(2,1)]


df_trips_district <- cbind(df_trips_pos, c(district))
names(df_trips_district)[names(df_trips_district) == "Zone_ID"] <- "dep_ID"
names(df_trips_district)[names(df_trips_district) == "name"] <- "dep_zone"


############################################################################################
# arrivals

arr_data <- data.frame( Longitude=df_trips_pos$arr_lng, Latitude=df_trips_pos$arr_lat )


coordinates(arr_data) <- ~ Longitude + Latitude
proj4string(arr_data) <- proj4string(my_map)

district <- over(arr_data,my_map)
head(district)
district <- district[,c(2,1)]


df_trips_district <- cbind(df_trips_district, c(district))
names(df_trips_district)[names(df_trips_district) == "Zone_ID"] <- "arr_ID"
names(df_trips_district)[names(df_trips_district) == "name"] <- "arr_zone"


#### step 2: cleaning the dataset

df_trips_district <- df_trips_district[!is.na(df_trips_district$dep_ID),]
df_trips_district <- df_trips_district[!is.na(df_trips_district$arr_ID),]


#### Step 3: counter.

zones<- my_map$Zone_ID
exist_na <- (length(which(is.na(df_trips_district$dep_ID))) > 0) ||
  (length(which(is.na(df_trips_district$arr_ID))) > 0) #modo idiota per checkare se abbiamo tenuto i NA
n<-length(zones) + exist_na

#v1 <- NULL
#v2 <- NULL

# for ( i in seq(1, dim(df_trips_district)[1]) ){
#   
#   if ( is.na(df_trips_district$dep_ID[i]) ){ 
#     v1[i] <- 6 # set na to 6
#   }else{
#     if(df_trips_district$dep_ID[i] == "relation/3348585"){ v1[i] <- 1 }
#     if(df_trips_district$dep_ID[i] == "relation/3348586"){ v1[i] <- 2 }
#     if(df_trips_district$dep_ID[i] == "relation/3348587"){ v1[i] <- 3 }
#     if(df_trips_district$dep_ID[i] == "relation/3348588"){ v1[i] <- 4 }
#     if(df_trips_district$dep_ID[i] == "relation/3348589"){ v1[i] <- 5 }
#   }
#   
#   if ( is.na(df_trips_district$arr_ID[i]) ){ 
#     v2[i] <- 6 # set na to 6
#   }else{
#     if(df_trips_district$arr_ID[i] == "relation/3348585"){ v2[i] <- 1 }
#     if(df_trips_district$arr_ID[i] == "relation/3348586"){ v2[i] <- 2 }
#     if(df_trips_district$arr_ID[i] == "relation/3348587"){ v2[i] <- 3 }
#     if(df_trips_district$arr_ID[i] == "relation/3348588"){ v2[i] <- 4 }
#     if(df_trips_district$arr_ID[i] == "relation/3348589"){ v2[i] <- 5 }
#   }
#   
# }

#df_trips_district <- cbind(df_trips_district,v1,v2)

#df_trips_district$dep_ID <- NULL
#df_trips_district$arr_ID <- NULL

#names(df_trips_district)[names(df_trips_district) == "v1"] <- "dep_ID"
#names(df_trips_district)[names(df_trips_district) == "v2"] <- "arr_ID"

#### Step 4: testing.

#############################################################################
# MATRIX FORM:::

d.flows <- matrix(0, n,  n) 
colnames(d.flows) <- c(1:n)
rownames(d.flows) <- c(1:n)

#select a day (50)

df_trips_district.50<-df_trips_district[df_trips_district$day == 50, ]
for (i in c(1:n)){
  arr<- df_trips_district.50[df_trips_district.50$dep_ID == i, grep("arr_ID", colnames(df_trips_district))]
  f<- count(arr)
  for (j in f$x){
    d.flows[paste0(i),paste0(j)]<-f[f$x==j,2]
  }
}
d.flows.50 <- d.flows
d.flows.50

#select a day (51)

df_trips_district.50<-df_trips_district[df_trips_district$day == 51, ]
for (i in c(1:n)){
  arr<- df_trips_district.50[df_trips_district.50$dep_ID == i, grep("arr_ID", colnames(df_trips_district))]
  f<- count(arr)
  for (j in f$x){
    d.flows[paste0(i),paste0(j)]<-f[f$x==j,2]
  }
}
d.flows.51 <- d.flows
d.flows.51

#### Step 4: CHi^2 test

chisq.test( as.vector(d.flows.50), as.vector(d.flows.51) )

#############################################################################
# ALTERNATIVE::: Only flows exiting from zone 3

N = 7 #weekly

baseday = 71 #scelta ad-hoc (merc, 13 marzo 2019) -> NB: poi nel ciclo partiamo da baseday+1 !!!

pval_mtrx <- matrix(0, N,  N) 

for ( i in seq(1,N-1) ){
  df_dist_1 <- df_trips_district[df_trips_district$dep_ID == 3 & df_trips_district$day == baseday + i, 
                                 grep("arr_ID", colnames(df_trips_district))]
  for ( j in seq(i+1,N) ){
    df_dist_2 <- df_trips_district[df_trips_district$dep_ID == 3 & df_trips_district$day == baseday + j, 
                      grep("arr_ID", colnames(df_trips_district))]

    df_pearson <- cbind(day1 = count(df_dist_1)[,2],day2 = count(df_dist_2)[,2])
    #print(df_pearson) 
    result <- chisq.test(df_pearson) 
    pval_mtrx[i,j] <- result$p.value # collecting p_values
  }
}

pval_mtrx
