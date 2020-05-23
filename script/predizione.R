#predizione

#import data and functions
source("script/import_data.R", encoding = "UTF-8")
source("script/utility.R", encoding = "UTF-8")

library(leaflet)
library(sp)
library(rgdal)

#including map

my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") , 
  layer="Maputo5distr",
  verbose=FALSE
)

#df_tracks_districts<-df_tracks
dat <- data.frame( Longitude=df_tracks$lng, Latitude=df_tracks$lat )


coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(my_map)

district <- over(dat,my_map)

df_tracks <- cbind(df_tracks, c(district))


#trips partenza
dat_2 <- data.frame( Longitude=df_trips_no_pos$dep_lng, Latitude=df_trips_no_pos$dep_lat )


coordinates(dat_2) <- ~ Longitude + Latitude
proj4string(dat_2) <- proj4string(my_map)

district <- over(dat_2,my_map)

df_trips_no_pos <- cbind(df_trips_no_pos, c(district))

#trips arrivo
dat_3 <- data.frame( Longitude=df_trips_no_pos$arr_lng, Latitude=df_trips_no_pos$arr_lat )


coordinates(dat_3) <- ~ Longitude + Latitude
proj4string(dat_3) <- proj4string(my_map)

district <- over(dat_3,my_map)

df_trips_no_pos <- cbind(df_trips_no_pos, c(district))



#features di interesse
df_trips_no_pos <- subset( df_trips_no_pos, select = -device_type )
df_trips_no_pos <- subset( df_trips_no_pos, select = -journey_id )
df_trips_no_pos <- subset( df_trips_no_pos, select = -records )
df_trips_no_pos <- subset( df_trips_no_pos, select = -dep_east )
df_trips_no_pos <- subset( df_trips_no_pos, select = -dep_north )
df_trips_no_pos <- subset( df_trips_no_pos, select = -arr_east )
df_trips_no_pos <- subset( df_trips_no_pos, select = -arr_north )
df_trips_no_pos <- subset( df_trips_no_pos, select = -arr_timestamp )
df_trips_no_pos <- subset( df_trips_no_pos, select = -day)
df_trips_no_pos <- subset( df_trips_no_pos, select = -name )
df_trips_no_pos <- subset( df_trips_no_pos, select = -name.1 )
df_trips_no_pos <- subset( df_trips_no_pos, select = -dep_timestamp )



#voglio predire l arrivo conoscendo la partenza quindi tolgo anche l informazione sull arrivo 
df_trips_no_pos <- subset( df_trips_no_pos, select = -arr_lat)
df_trips_no_pos <- subset( df_trips_no_pos, select = -arr_lng)

#elimino dal dataset i viaggi di cui non conosco distretti di partenza arrivo
df_trips_no_pos = df_trips_no_pos[which(!is.na(df_trips_no_pos$Zone_ID) & !is.na(df_trips_no_pos$Zone_ID.1)),]


#spezzo il dataset in due parti, userò la prima per il training e la seconda per la verifica
df_trips_training = df_trips_no_pos[1:50000,]
df_trips_verif = df_trips_no_pos[50001:dim(df_trips_no_pos)[1],]
df_trips_training = df_trips_training[order(df_trips_training$Zone_ID.1),]

##### 5-fold cross validation ######

N=dim(df_trips_training)[1]
N_train=N/5
# random permutation of labels
shuffle = sample(N)
fold_1 = shuffle[1:floor(N/5)]
fold_2 = shuffle[(floor(N/5)+1):(2*floor(N/5))]
fold_3 = shuffle[(2*floor(N/5)+1):(3*floor(N/5))]
fold_4 = shuffle[(3*floor(N/5)+1):(4*floor(N/5))]
fold_5 = shuffle[(4*floor(N/5)+1):N]

train = list()
train[[1]] = c(fold_1,fold_2,fold_3,fold_4)
train[[2]] = c(fold_1,fold_2,fold_3,fold_5)
train[[3]] = c(fold_1,fold_2,fold_4,fold_5)
train[[4]] = c(fold_1,fold_3,fold_4,fold_5)
train[[5]] = c(fold_2,fold_3,fold_4,fold_5)

fold = list()
fold[[1]]= fold_5
fold[[2]]= fold_4
fold[[3]]= fold_3
fold[[4]]= fold_2
fold[[5]]= fold_1


#### naive bayes classifier #####

naive_error = replicate(5,0)
bayes_classifier=list()

Gaussian_MLE_estimate= function (X)
{
   N=dim(X)[1]
   l=dim(X)[2]
   m_hat=(1/N)*sum(X)
   S_hat=replicate(l,0)
   for (k=1:N){
      S_hat=S_hat+(X[,k]-m_hat)*t((X[,k]-m_hat))
   }
  S_hat=(1/N)*S_hat
  list_temp=list()
  list_temp[[1]]=m_hat
  list_temp[[2]]=S_hat
  return list_temp
}

for(j = 1:4) {
  #5 perchè ho 5 distretti, 10 perchè ho 10 features
  m1_hat = replicate (10, 0)
  S1_hat = replicate (10, 0)
  m2_hat = replicate (10, 0)
  S2_hat = replicate (10, 0)
  m3_hat = replicate (10, 0)
  S3_hat = replicate (10, 0)
  m4_hat = replicate (10, 0)
  S4_hat = replicate (10, 0)
  m5_hat = replicate (10, 0)
  S5_hat = replicate (10, 0)
  for(i = 1 : 10 ) {
     #MLE 
    estimate_1 = Gaussian_ML_estimate(df_trips_training[train[[j]][which(df_trips_training$Zone_ID==1)],])
    estimate_2 = Gaussian_ML_estimate(df_trips_training[train[[j]][which(df_trips_training$Zone_ID==2)],])
    estimate_3 = Gaussian_ML_estimate(df_trips_training[train[[j]][which(df_trips_training$Zone_ID==3)],])
    estimate_4 = Gaussian_ML_estimate(df_trips_training[train[[j]][which(df_trips_training$Zone_ID==4)],])
    estimate_5 = Gaussian_ML_estimate(df_trips_training[train[[j]][which(df_trips_training$Zone_ID==5)],])
  }
}

