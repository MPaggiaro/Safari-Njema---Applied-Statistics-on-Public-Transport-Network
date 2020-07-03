
df_tragitto_clustered[df_tragitto_clustered$clust3==1, c(5) ]<- 'walk'
df_tragitto_clustered[df_tragitto_clustered$clust3==2, c(5)]<- 'chapas'
df_tragitto_clustered[df_tragitto_clustered$clust3==3, c(5)]<- 'car'
colnames(df_tragitto_clustered)[5]<- 'typeOfJourney'

data_no_walk = df_tragitto_clustered[which(df_tragitto_clustered$clust3!=1),]


for (i in unique(data_no_walk$trip_id)){
  tmp = mean(data_no_walk[which(data_no_walk$trip_id==i),]$clust3)
  if (tmp>=2.5){
    data_no_walk[which(data_no_walk$trip_id==i),]$clust3=3}
  else{
    data_no_walk[which(data_no_walk$trip_id==i),]$clust3=2}
}


idx = ifelse(df_tragitto_clustered$clust3==1,0,1)

df_tragitto_clustered[idx,]=data_no_walk

df_arch = df_tragitto_clustered

df_tragitto_clustered[df_tragitto_clustered$clust3==1, c(5) ]<- 'walk'
df_tragitto_clustered[df_tragitto_clustered$clust3==2, c(5)]<- 'chapas'
df_tragitto_clustered[df_tragitto_clustered$clust3==3, c(5)]<- 'car'
colnames(df_tragitto_clustered)[5]<- 'clust3'

