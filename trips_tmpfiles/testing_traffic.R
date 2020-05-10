library(leaflet)
library(sp)
library(rgdal)

# import data
df_tracks <- read.table("local_data/df_tracks.txt", header=T)



# including map

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

# selection of zone 3:
tracks_3 <- df_tracks[df_tracks$Zone_ID == 3,]

# over the days, count:
tracks_3_night <- tracks_3[tracks_3$hour <= 5,]
tracks_3_morning <- tracks_3[tracks_3$hour >=6 & tracks_3$hour <=9,]
tracks_3_midday <-tracks_3[tracks_3$hour >=10 & tracks_3$hour <=14,]
tracks_3_anoon <-tracks_3[tracks_3$hour >=15 & tracks_3$hour <=18,]
tracks_3_evening <-tracks_3[tracks_3$hour >=19 & tracks_3$hour <=24,]



N = 180-30
morning = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_m_daily <- tracks_3_morning[tracks_3_morning$day==i,]
  tracks_3_m_daily <- tracks_3_m_daily[!duplicated(tracks_3_m_daily$id),]
  morning[i-30] <- dim(tracks_3_m_daily)[1]
}

hist(morning, breaks = 20)
hist ( (morning - mean(morning)) /sd(morning), breaks = 20 )
plot(morning)

night = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_n_daily <- tracks_3_night[tracks_3_night$day==i,]
  tracks_3_n_daily <- tracks_3_n_daily[!duplicated(tracks_3_n_daily$id),]
  night[i-30] <- dim(tracks_3_n_daily)[1]
}

# hist(night, breaks = 20)
# hist ( (night - mean(night)) /sd(night), breaks = 20 )
# plot(night[1:50])

morning = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_m_daily <- tracks_3_morning[tracks_3_morning$day==i,]
  tracks_3_m_daily <- tracks_3_m_daily[!duplicated(tracks_3_m_daily$id),]
  morning[i-30] <- dim(tracks_3_m_daily)[1]
}
###

midday = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_n_daily <- tracks_3_midday[tracks_3_midday$day==i,]
  tracks_3_n_daily <- tracks_3_n_daily[!duplicated(tracks_3_n_daily$id),]
  midday[i-30] <- dim(tracks_3_n_daily)[1]
}


anoon = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_n_daily <- tracks_3_anoon[tracks_3_anoon$day==i,]
  tracks_3_n_daily <- tracks_3_n_daily[!duplicated(tracks_3_n_daily$id),]
  anoon[i-30] <- dim(tracks_3_n_daily)[1]
}


evening = rep(0,N)
# unique:
for (i in seq(31,180)){
  tracks_3_n_daily <- tracks_3_evening[tracks_3_evening$day==i,]
  tracks_3_n_daily <- tracks_3_n_daily[!duplicated(tracks_3_n_daily$id),]
  evening[i-30] <- dim(tracks_3_n_daily)[1]
}

total = night + morning + midday + anoon + evening

# frequencies over the total day traffic:
fr_night = night/total
fr_morning = morning/total
fr_midday = midday/total
fr_anoon = anoon/total
fr_evening = evening/total

# saving the data in a dataset:
traffic.time.slots <- data.frame(night,fr_night,morning,fr_morning,midday,fr_midday,anoon,fr_anoon,evening,fr_evening,total)
write.table(traffic.time.slots, "trips_tmpfiles/ traffic.data") #per usarlo, read.table

###### RUNNARE DA QUI ################################


traffic.time.slots<-read.table("trips_tmpfiles/ traffic.data")

x11()
par(mfrow=c(2,3))
hist(fr_night , breaks= 100)
hist(fr_morning , breaks= 100)
hist(fr_midday , breaks= 100)
hist(fr_anoon, breaks= 100)
hist(fr_evening , breaks= 100)


x11()
par(mfrow=c(2,3))
plot(fr_night)
plot(fr_morning)
plot(fr_midday)
plot(fr_anoon)
plot(fr_evening)


##week days
sabati<- seq(2,150, 7)
dom<- seq(3,150, 7)
weekend<- as.vector(rbind(sabati, dom))

#divisione weekend - week days
weekend.traffic.time.slots<-traffic.time.slots[weekend, ]
week.traffic.time.slots <-traffic.time.slots[-weekend, ]


#Gaussianità analisi: (nelle 5 fascie orarie)---> da valutare meglio

#hist
x11()
par(mfrow=c(2,3))
h <-hist(week.traffic.time.slots$fr_night , breaks= 100)
xfit <- seq(min(week.traffic.time.slots$fr_night), max(week.traffic.time.slots$fr_night), length = 400) 
yfit <- dnorm(xfit) * sd(week.traffic.time.slots$fr_night) + mean(week.traffic.time.slots$fr_night)
yfit <- yfit * diff(h$mids[1:2]) * length(week.traffic.time.slots$fr_night)
lines(xfit, yfit, col = "black", lwd = 2)
hist(week.traffic.time.slots$fr_morning , breaks= 100)
hist(week.traffic.time.slots$fr_midday , breaks= 100)
hist(week.traffic.time.slots$fr_anoon, breaks= 100)
hist(week.traffic.time.slots$fr_evening , breaks= 100)


##qqnorm
x11()
par(mfrow=c(2,3))
qqnorm(week.traffic.time.slots$fr_night)
qqline(week.traffic.time.slots$fr_night)

qqnorm(week.traffic.time.slots$fr_morning)
qqline(week.traffic.time.slots$fr_morning)

qqnorm(week.traffic.time.slots$fr_midday)
qqline(week.traffic.time.slots$fr_midday)

qqnorm(week.traffic.time.slots$fr_anoon)
qqline(week.traffic.time.slots$fr_anoon)

qqnorm(week.traffic.time.slots$fr_evening)
qqline(week.traffic.time.slots$fr_evening)


x11()
par(mfrow=c(2,3))
plot(week.traffic.time.slots$fr_night)
plot(week.traffic.time.slots$fr_morning)
plot(week.traffic.time.slots$fr_midday)
plot(week.traffic.time.slots$fr_anoon)
plot(week.traffic.time.slots$fr_evening)


##### TEST (UNIVARIATE REPEATED MEASURES)
#unità statistica osservata = frequenza relativa di affollamento  nella zona 3 nei diversi giorni

fr_week.traffic<-week.traffic.time.slots[, c(2,4,6,8,10)]
head(fr_week.traffic)

n <- dim(fr_week.traffic)[1]
q <- dim(fr_week.traffic)[2]

M <- sapply(fr_week.traffic,mean)
M
S <- cov(fr_week.traffic)
S

# we build one of the possible contrast matrices to answer
# the question
C <- matrix(c(-1, 1, 0, 0, 0,
               0, -1, 1, 0, 0,
               0, 0, -1, 1, 0,
               0, 0, 0,- 1, 1), 4, 5, byrow=T)
C
# here we are looking at the differebces between time slots
# Test: H0: C%*%mu == 0 vs H1: C%*%mu != 0
alpha   <- .05
delta.0 <- c(0,0,0,0)

#elements for T statistics computation
Md <- C %*% M 
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 )

cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 

T2 < cfr.fisher #reject - no stat evidence to be equal
T2
cfr.fisher


P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P #zero macchina
#almeno una fascia oraria diversa, forte evidenza statistica


# Simultaneous T2 intervals
IC.T2 <- cbind( inf= Md - sqrt(cfr.fisher*diag(Sd)/n) ,
                c=Md, 
                sup = Md + sqrt(cfr.fisher*diag(Sd)/n) )
IC.T2

# Bonferroni intervals 
k     <- q - 1   # number of increments (i.e., dim(C)[1])
cfr.t <- qt(1-alpha/(2*k),n-1)

IC.BF <- cbind( Md - cfr.t*sqrt(diag(Sd)/n) , Md, Md + cfr.t*sqrt(diag(Sd)/n) )
IC.BF


x11()
matplot(t(matrix(1:4,4,3)),t(IC.BF), type='b',pch='',xlim=c(0,4),xlab='',ylab='', main='Confidence intervals')
segments(matrix(1:4,4,1),IC.BF[,1],matrix(1:4,4,1),IC.BF[,3], col='orange', lwd=2)#plot of the IC_BF intervals, like vertical lines
points(1:4, IC.BF[,2], col='orange', pch=16)
points(1:4+.05, delta.0, col='black', pch=16) #delta_0 representation (to check if they lay inside)
segments(matrix(1:4+.1,4,1),IC.T2[,1],matrix(1:4+.1,4,1),IC.T2[,3], col='blue', lwd=2)
points(1:4+.1,IC.T2[,2], col='blue', pch=16)


## NOTIAMO: evidenza statistica in una differenza di frequanza/traffico tra notte e mattina (1)
#           e tra pomeriggio e sera (4)



