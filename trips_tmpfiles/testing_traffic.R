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
tracks_3 <- tracks_3[!is.na(tracks_3$Zone_ID),]

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


## Divisione week and weekend days

sabati<- seq(2,150, 7)
dom<- seq(3,150, 7)
weekend<- as.vector(rbind(sabati, dom))


weekend.traffic.time.slots<-traffic.time.slots[weekend, ]
week.traffic.time.slots <-traffic.time.slots[-weekend, ]


###### WEEK days analysis ####


#hist (valuto gaussianità)
x11()
par(mfrow=c(2,3))
hist(week.traffic.time.slots$fr_night , breaks= 100)
lines(seq(min(week.traffic.time.slots$fr_night), max(week.traffic.time.slots$fr_night), length=2000), 
      dnorm(seq(min(week.traffic.time.slots$fr_night), max(week.traffic.time.slots$fr_night), length=2000),
            mean(week.traffic.time.slots$fr_night),sd(week.traffic.time.slots$fr_night)), col='blue', lty=1)

hist(week.traffic.time.slots$fr_morning , breaks= 100)
lines(seq(min(week.traffic.time.slots$fr_morning), max(week.traffic.time.slots$fr_morning), length=2000), 
      dnorm(seq(min(week.traffic.time.slots$fr_morning), max(week.traffic.time.slots$fr_morning), length=2000),
            mean(week.traffic.time.slots$fr_morning),sd(week.traffic.time.slots$fr_morning)), col='blue', lty=1)
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

dev.off()

#shapiro test essendo univariati

shapiro.test(week.traffic.time.slots$fr_night)
shapiro.test(week.traffic.time.slots$fr_morning)
shapiro.test(week.traffic.time.slots$fr_midday)
shapiro.test(week.traffic.time.slots$fr_anoon)
shapiro.test(week.traffic.time.slots$fr_evening)

#---> DOBBIAMO TROVARE LA GAUSSIANITà MULTIVARIATA
################## GAUSSIANITY ########################

library(car)
library(mvtnorm)

fr_week.traffic<-week.traffic.time.slots[, c(2,4,6,8,10)]
head(fr_week.traffic)
load("/Users/maddalenalischetti/Desktop/Applied Stat/Lab 5 - 16042020/mcshapiro.test.RData")

mcshapiro.test(fr_week.traffic) 
#RIFIUTO -> O MACCHINA
#Lo sapevamo già nonn essendo gaussiane le componenti

#1) provo con Box-Cox --> funziona male: cerco una trasformazione che non sia una potenza

#multivariate Boc-Cox - varie prove studiano anche dim < 5 
a<-as.matrix(fr_week.traffic)
lambda <- powerTransform(a[, c(2,3,4)])# same as = powerTransform(b)
lambda

fr_week.traffic.2.bc<-bcPower(fr_week.traffic[,2], lambda$lambda[1])
fr_week.traffic.3.bc<-bcPower(fr_week.traffic[,3], lambda$lambda[2])
fr_week.traffic.4.bc<-bcPower(fr_week.traffic[,4], lambda$lambda[3])
#fr_week.traffic.5.bc<-bcPower(fr_week.traffic[,5], lambda$lambda[4])

x11()
par(mfrow=c(3,2))
qqnorm(fr_week.traffic[,2], main="morning",col='red')
qqnorm(fr_week.traffic.2.bc, main="BC.morning",col='red')

qqnorm(fr_week.traffic[,3], main="midday",col='red')
qqnorm(fr_week.traffic.3.bc, main="BC.midday",col='red')

qqnorm(fr_week.traffic[,4], main="anoon",col='red')
qqnorm(fr_week.traffic.4.bc, main="BC.anoon",col='red')

shapiro.test(fr_week.traffic.2.bc)
shapiro.test(fr_week.traffic.3.bc)
shapiro.test(fr_week.traffic.4.bc)
#shapiro.test(fr_week.traffic.5.bc)

mcshapiro.test(cbind(fr_week.traffic.2.bc, fr_week.traffic.3.bc,fr_week.traffic.4.bc))



## PROVA CON MANOVA.RM() che non richiede l'ipotesi di gaussianità

#Bisogna creare un dataset che non so come fare
n.obs<-length(fr_week.traffic[,2])
q<-5
n.tot<-n.obs*q
data.fr<-matrix(cbind(fr_week.traffic[,1],fr_week.traffic[,2],fr_week.traffic[,3],fr_week.traffic[,4],fr_week.traffic[,5]), ncol = 1 )
head(data.fr)

groups<-matrix(cbind(rep(1,n.obs),rep(2,n.obs), rep(3,n.obs), rep( 4,n.obs), rep(5,n.obs)), ncol=1)
head(groups)

sub<-seq(1,n.obs, by=1)
sub<-rep(sub, q)

data.fr<-cbind(data.fr, groups, sub)
head(data.fr)

colnames(data.fr)<-c("freq_rel", "time_slot", "Subject")
data.fr<-data.frame(data.fr)
data.fr$freq_rel<- as.numeric(data.fr$freq_rel)
head(data.fr)

model<-RM(freq_rel ~ time_slot, data=data.fr, subject = "Subject") ##NON FUNZIONA
summary(model)




##### TEST (UNIVARIATE REPEATED MEASURES) --> OK SE PROVASSIMO valida la GAUSSIANITà

#unità statistica osservata = frequenza relativa di affollamento  nella zona 3 nei diversi giorni

fr_week.traffic<-week.traffic.time.slots[, c(2,4,6,8,10)]


# fr_week.traffic<- fr_week.traffic[fr_week.traffic$fr_evening <0.20 & 
#                                     fr_week.traffic$fr_anoon >0.22 &
#                                     fr_week.traffic$fr_midday < 0.30 &
#                                     fr_week.traffic$fr_night < 0.09 &
#                                     fr_week.traffic$fr_morning >0.22 &
#                                     fr_week.traffic$fr_morning <0.24 , ]
# dim(fr_week.traffic)[1]


head(fr_week.traffic)

n <- dim(fr_week.traffic)[1]
q <- dim(fr_week.traffic)[2]

M <- sapply(fr_week.traffic,mean)
M
S <- cov(fr_week.traffic)
S

# we build one of the possible contrast matrices to answer
# the question

# C <- matrix(c(-1, 1, 0, 0, 0,
#                0, -1, 1, 0, 0,
#                0, 0, -1, 1, 0,
#                0, 0, 0,- 1, 1), 4, 5, byrow=T)
# C

#Try test with different comparison matrix
C <- matrix(c(-1, 1, 0, 0, 0,
               0, 1, -1, 0, 0,
               0, 1, 0, -1, 0,
               0, 1, 0, 0 , -1), 4, 5, byrow=T)
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

#Con la seconda matrice vediamo una differenza rispetto a tutte la fasce, ma molto più sostanziale vs
#notte e sera (ovvio). Sembra non esserci una forte diversità mattina - midday come ci aspettavamo



################################################################################################################################

#DATI ACCOPPIATI : confronto le varie fasce orarie creando delle differenza univariate

###########################################################################################################################

###TEST DATI ACCOPPIATI MATTINA vs MIDDAY e MIDDAY vs ANOON 
#in WEEK DAYS

head(week.traffic.time.slots)

#MATTINA vs MIDDAY
#compute differences

D<-week.traffic.time.slots$fr_morning - week.traffic.time.slots$fr_midday
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D>-0.05)]
n<-length(D)
n

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 


D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#REJECT the NULL: evidence of difference of traffic between mornig and midday

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)
#-->   statistical evidence that:
# Il traffico nella zona 3 alla mattina è maggiore che a metà giornata


#Manually with multivariate procedure but p=1 = SAME RESULT
D.t2 <- n * (D.mean-delta.0) *D.invcov *(D.mean-delta.0)
D.t2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher


D.t2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value from Fisher test
P <- 1-pf(D.t2*(n-p)/(p*(n-1)), p, n-p)
P



#MIDDAY vs ANOON
#compute differences
head(week.traffic.time.slots)

D<-week.traffic.time.slots$fr_midday - week.traffic.time.slots$fr_anoon
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

dev.off()

#coda destra un pochino pesante

shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D<0.09)]
n<-length(D)
n

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà con tolte due osservazioni

graphics.off()

#PAIRED univariate --> t-test 

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
print("D= midday- anoon")
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)

#p.val 0.0381 (not reject solo all 1%) con alpha 1% accetto l'ipotesi nulla (stessa media) 

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)





#Morning vs ANOON
#compute differences
head(week.traffic.time.slots)

D<-week.traffic.time.slots$fr_morning - week.traffic.time.slots$fr_anoon
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)
#coda sinistra brutta 

dev.off()
shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D<0.15 & D>=-0.06)]
n<-length(D)
n

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #-->ANCORA BRUTTA CODA provo  a togliere altri outliers, avendo eliminato solo 3 obs fino adesso

D<-D[which(D<0.15 & D>-0.05)] 
n<-length(D) #n=101, due obs in meno rispetto a prima, in tot 5 in meno
n

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #PERFETTO

graphics.off()

#PAIRED univariate --> t-test 

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
print("morning - anoon")
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)
#-->p.val  4.963716e-22 --> evidence to reject null (equal mean)
t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#--> evidenza statistica che al mattino il traffico/affollamento nella zona 3
#    è maggiore che al pomeriggio








