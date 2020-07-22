#cluster sulle velocità istantanee e medie per classificare il tipo di viaggio: 

temp<- unique(df_tracks$day * 100000 + df_tracks$journey_id)

df_tragitto<-df_tracks
df_tragitto<-cbind(df_tragitto, trip_id= df_tracks$day * 100000 + df_tracks$journey_id )
n<- dim(df_tragitto)[1]

indicini = rep(TRUE,n)
for(i in 2:(n-1)){
  
  if((df_tragitto$stop_here[i]==1) &(df_tragitto$trip_id[i-1]==df_tragitto$trip_id[i]) & (df_tragitto$trip_id[i]==df_tragitto$trip_id[i+1])){
    
    indicini[i] = !((df_tragitto$stop_here[i-1]==1) & (df_tragitto$stop_here[i+1]==1))
    
  }
}

df_tragitto = df_tragitto[indicini,]




n<- dim(df_tragitto)[1]
delta_t<- df_tragitto$timestamp[2:n ]- df_tragitto$timestamp[1:n-1]
delta_t<-c(0,delta_t)
#diss<-dist(df_tragitto_shuffled[1210001: dim(df_tragitto_shuffled)[1], c(3,4)])

df_tragitto<-cbind(df_tragitto, delta_t)

vi<-(df_tragitto$speed[2:n ]+ df_tragitto$speed[1:n-1])/2
vi<-c(0, vi)
df_tragitto<-cbind(df_tragitto, vi)

vm<-df_tragitto$distance_from_previous_mts / df_tragitto$delta_t
df_tragitto<-cbind(df_tragitto, vm)

b<- df_tragitto$trip_id[1: n-1]== df_tragitto$trip_id[2: n]
b<-c(FALSE, b)
df_tragitto<- df_tragitto[b, c(19,5,21,22)]

#ciclo su tutti a pezzi di 10.000
df_tragitto_no<-df_tragitto[ which(df_tragitto$vm < 50), ]
n<-10000
clust3<-NULL

index<- seq(1, dim(df_tragitto_no)[1])
df_tragitto_no<-cbind(df_tragitto_no, idx= index)

a<- sample(1:dim(df_tragitto_no)[1])
df_tragitto_shuffled<- df_tragitto_no[a,]

for ( i in seq(n+1,dim(df_tragitto_no)[1], by = n )){
  diss<-dist(df_tragitto_shuffled[(i-n):(i-1), c(3,4)])
  clust<-hclust(diss, method='ward.D2')
  
  temp<-cutree(clust, k=3)+3
  
  is1= temp[which.min(df_tragitto_shuffled[(i-n):(i-1),]$vi)]
  is3= temp[which.max(df_tragitto_shuffled[(i-n):(i-1),]$vi)]
  
  temp[which(temp==is3)] =3
  temp[which(temp==is1)] =1
  temp[which(temp > 3)] =2
  
  
  clust3<-c(clust3, temp)
}

ultimo = seq(n+1,dim(df_tragitto_no)[1], by=n)
ultimo = ultimo[length(ultimo)]

diss<-dist(df_tragitto_shuffled[ultimo: dim(df_tragitto_shuffled)[1], c(3,4)])
clust<-hclust(diss, 'ward.D2')

temp<-cutree(clust, k=3)
is1= temp[which.min(df_tragitto_shuffled[ultimo: dim(df_tragitto_shuffled)[1],]$vi)]
is3= temp[which.max(df_tragitto_shuffled[ultimo: dim(df_tragitto_shuffled)[1],]$vi)]

temp[which(temp==is3)] =3
temp[which(temp==is1)] =1
temp[which(temp > 3)] =2


clust3<- c(clust3, temp)


x11()
plot(df_tragitto_shuffled[, c(3,4)], pch=18, cex=0.05,  col= clust3+1)

table(clust3)

backupino = df_tragitto_shuffled
##Assegno un cluster

df_tragitto_shuffled<-cbind(df_tragitto_shuffled, clust3)

#Riordino il df con gli indici oroginali: 

df_tragitto_clustered<-df_tragitto_shuffled[order(df_tragitto_shuffled$idx), ]

df_tragitto_clustered<-df_tragitto_clustered[,-5]
data_no_walk = df_tragitto_clustered[which(df_tragitto_clustered$clust3!=1),]


for (i in unique(data_no_walk$trip_id)){
  tmp = mean(data_no_walk[which(data_no_walk$trip_id==i),]$clust3)
  if (tmp>=2.5){
    data_no_walk[which(data_no_walk$trip_id==i),]$clust3=3}
  else{
    data_no_walk[which(data_no_walk$trip_id==i),]$clust3=2}
}



idx = ifelse(df_tragitto_clustered$clust3==1,FALSE,TRUE)

backuppone = df_tragitto_clustered
df_tragitto_clustered=backuppone
df_tragitto_clustered[idx,]=data_no_walk

df_arch = df_tragitto_clustered

df_tragitto_clustered[df_tragitto_clustered$clust3==1, c(5) ]<- 'walk'
df_tragitto_clustered[df_tragitto_clustered$clust3==2, c(5)]<- 'chapas'
df_tragitto_clustered[df_tragitto_clustered$clust3==3, c(5)]<- 'car'
colnames(df_tragitto_clustered)[5]<- 'clust3'


x11()

plot(df_arch[, c(3,4)], pch=18, cex=0.05,  col=df_arch$clust3+1)