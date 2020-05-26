# Week vs Weekend.

# Goal: infer if the traffic distribution ()

#settings
library(rgdal)
library(plyr)
library(rgr)
source("trips_tmpfiles/functions.R", encoding = "UTF-8")

#install.packages("rgr")

#caricare mcshapiro in base alla vostra directory
load("/Users/maddalenalischetti/Desktop/Applied Stat/Lab 5 - 16042020/mcshapiro.test.RData")

if (!exists("df_trips_pos")){
  df_trips_pos <- read.table("local_data/df_trips_pos.txt")
}

my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") , 
  layer="Maputo5distr",
  verbose=FALSE
)

zonesID<-c(1,2,3,4,5)

df.trips.district<-add_zones_labels(df_trips_pos, my_map)

#remove tripes with non classified arrival or departure points
df.trips.district <- df.trips.district[!is.na(df.trips.district$dep_ID),]
df.trips.district <- df.trips.district[!is.na(df.trips.district$arr_ID),]

#GOAL: weekly trend in departure from zone 3

df.trips.district.DEP3 <- df.trips.district[df.trips.district$dep_ID == 3,]
n_tot <- dim(df.trips.district.DEP3)[1] 
n_tot
#70830 trips observed departed from 3
#8804 trips observed departed from 4 (lasciare tutto ugule nei nomi e cambiare il parametro sopra)
#10564 trips observed departed from 1


#APPROACH 1 - MANOVA

#Creo 5 gruppi di n= 150/7 (giorni) oss multivariate , ogni gruppo è un giorno della settimana

monday <-seq(34,150, by=7) #manualmente guardando il caledario 2019 e sapendo che 31= '1st feb'
tuesday<-seq(35,150, by=7) 
wed<-seq(36,150, by=7)
thursday<-seq(37,150, by=7)
friday<-seq(38,150, by=7)

#weekend 
sat<-seq(39,150, by=7)
sunday<-seq(40,150, by=7)

#dataframe pof trips divided by week day 
mon.df<-df.trips.district.DEP3[df.trips.district.DEP3$day %in% monday, ]
tues.df<-df.trips.district.DEP3[df.trips.district.DEP3$day %in% tuesday, ]
wed.df<-df.trips.district.DEP3[df.trips.district.DEP3$day %in% wed, ]
thur.df<-df.trips.district.DEP3[df.trips.district.DEP3$day %in% thursday, ]
frid.df<-df.trips.district.DEP3[df.trips.district.DEP3$day %in% friday, ]
sat.df<-df.trips.district.DEP3[df.trips.district.DEP3$day %in% sat, ]
sun.df<-df.trips.district.DEP3[df.trips.district.DEP3$day %in% sunday, ]

#Create for each day, the multitimensional (p=5) observed unit static ( che descrive la distribuzione delle partenze in base 
# alla zona di arrivo)

week<-data.frame( 10, 10, 10, 10, 10, NA)

colnames(week)<-c(zonesID, "day")
for (i in monday){
  
  freq.i<-mon.df[mon.df$day==i, grep("arr_ID", colnames(df.trips.district))]
  freq.df<-count(freq.i)
  
  #check frequencies vectors for dimension and order (i.e. in case some arrival ID are not considered add 0)
  # nb: se sono tutti present non aggiunge ninete!!
  ##------------------------------------------------
  miss.x<-which(!(zonesID %in% freq.df$x))
  add.row.x<- data.frame(x=miss.x, freq=rep(0, length(miss.x)))
  freq.df<-rbind(freq.df, add.row.x)
  freq.df<-freq.df[order(freq.df$x), ]
  
  ##-----------------------------------------------
  
  #Calcolo vettore di frequenze rel ~ distribione degli arrivi dalla zona nel giorno i
  freq.df$freq<-freq.df$freq/sum(freq.df$freq)
  week<-rbind(week, c(as.numeric(freq.df$freq), 'M'))
}

#sistemo la matrice week che contiene il dataset multivariato per fare l'anova

week<-week[!is.na(week[, 6]),] 


for (i in tuesday){
  
  freq.i<-df.trips.district.DEP3[df.trips.district.DEP3$day==i, grep("arr_ID", colnames(df.trips.district))]
  freq.df<-count(freq.i)
  
  #check frequencies vectors for dimension and order (i.e. in case some arrival ID are not considered add 0)
  # nb: se sono tutti present non aggiunge ninete!!
  ##------------------------------------------------
  miss.x<-which(!(zonesID %in% freq.df$x))
  add.row.x<- data.frame(x=miss.x, freq=rep(0, length(miss.x)))
  freq.df<-rbind(freq.df, add.row.x)
  freq.df<-freq.df[order(freq.df$x), ]
  
  ##-----------------------------------------------
  
  #Calcolo vettore di frequenze rel ~ distribione degli arrivi dalla zona nel giorno i
  freq.df$freq<-freq.df$freq/sum(freq.df$freq)
  
  week<-rbind(week, c(as.numeric(freq.df$freq), 'T') )
}


for (i in wed){
  
  freq.i<-df.trips.district.DEP3[df.trips.district.DEP3$day==i, grep("arr_ID", colnames(df.trips.district))]
  freq.df<-count(freq.i)
  
  #check frequencies vectors for dimension and order (i.e. in case some arrival ID are not considered add 0)
  # nb: se sono tutti present non aggiunge ninete!!
  ##------------------------------------------------
  miss.x<-which(!(zonesID %in% freq.df$x))
  add.row.x<- data.frame(x=miss.x, freq=rep(0, length(miss.x)))
  freq.df<-rbind(freq.df, add.row.x)
  freq.df<-freq.df[order(freq.df$x), ]
  
  ##-----------------------------------------------
  
  #Calcolo vettore di frequenze rel ~ distribione degli arrivi dalla zona nel giorno i
  freq.df$freq<-freq.df$freq/sum(freq.df$freq)
  week<-rbind(week, c(as.numeric(freq.df$freq), 'W'))
}


for (i in thursday){
  
  freq.i<-df.trips.district.DEP3[df.trips.district.DEP3$day==i, grep("arr_ID", colnames(df.trips.district))]
  freq.df<-count(freq.i)
  
  #check frequencies vectors for dimension and order (i.e. in case some arrival ID are not considered add 0)
  # nb: se sono tutti present non aggiunge ninete!!
  ##------------------------------------------------
  miss.x<-which(!(zonesID %in% freq.df$x))
  add.row.x<- data.frame(x=miss.x, freq=rep(0, length(miss.x)))
  freq.df<-rbind(freq.df, add.row.x)
  freq.df<-freq.df[order(freq.df$x), ]
  
  ##-----------------------------------------------
  
  #Calcolo vettore di frequenze rel ~ distribione degli arrivi dalla zona nel giorno i
  freq.df$freq<-freq.df$freq/sum(freq.df$freq)
  week<-rbind(week, c(as.numeric(freq.df$freq), 'Th'))
}

for (i in friday){
  
  freq.i<-df.trips.district.DEP3[df.trips.district.DEP3$day==i, grep("arr_ID", colnames(df.trips.district))]
  freq.df<-count(freq.i)
  
  #check frequencies vectors for dimension and order (i.e. in case some arrival ID are not considered add 0)
  # nb: se sono tutti present non aggiunge ninete!!
  ##------------------------------------------------
  miss.x<-which(!(zonesID %in% freq.df$x))
  add.row.x<- data.frame(x=miss.x, freq=rep(0, length(miss.x)))
  freq.df<-rbind(freq.df, add.row.x)
  freq.df<-freq.df[order(freq.df$x), ]
  
  ##-----------------------------------------------
  
  #Calcolo vettore di frequenze rel ~ distribione degli arrivi dalla zona nel giorno i
  freq.df$freq<-freq.df$freq/sum(freq.df$freq)
  week<-rbind(week, c(as.numeric(freq.df$freq),'F'))
}


for (i in sat){
  
  freq.i<-df.trips.district.DEP3[df.trips.district.DEP3$day==i, grep("arr_ID", colnames(df.trips.district))]
  freq.df<-count(freq.i)
  
  #check frequencies vectors for dimension and order (i.e. in case some arrival ID are not considered add 0)
  # nb: se sono tutti present non aggiunge ninete!!
  ##------------------------------------------------
  miss.x<-which(!(zonesID %in% freq.df$x))
  add.row.x<- data.frame(x=miss.x, freq=rep(0, length(miss.x)))
  freq.df<-rbind(freq.df, add.row.x)
  freq.df<-freq.df[order(freq.df$x), ]
  
  ##-----------------------------------------------
  
  #Calcolo vettore di frequenze rel ~ distribione degli arrivi dalla zona nel giorno i
  freq.df$freq<-freq.df$freq/sum(freq.df$freq)
  week<-rbind(week, c(as.numeric(freq.df$freq),'Sat'))
}

for (i in sunday){
  
  freq.i<-df.trips.district.DEP3[df.trips.district.DEP3$day==i, grep("arr_ID", colnames(df.trips.district))]
  freq.df<-count(freq.i)
  
  #check frequencies vectors for dimension and order (i.e. in case some arrival ID are not considered add 0)
  # nb: se sono tutti present non aggiunge ninete!!
  ##------------------------------------------------
  miss.x<-which(!(zonesID %in% freq.df$x))
  add.row.x<- data.frame(x=miss.x, freq=rep(0, length(miss.x)))
  freq.df<-rbind(freq.df, add.row.x)
  freq.df<-freq.df[order(freq.df$x), ]
  
  ##-----------------------------------------------
  
  #Calcolo vettore di frequenze rel ~ distribione degli arrivi dalla zona nel giorno i
  freq.df$freq<-freq.df$freq/sum(freq.df$freq)
  week<-rbind(week, c(as.numeric(freq.df$freq),'Sun'))
}


############################ 

#Non so perchè si salvano come char, brutto ma devo frozare le colonne a numeric
week[,'1'] <- as.numeric(as.character(week[,'1']))
week[,'2'] <- as.numeric(as.character(week[,'2']))
week[,'3'] <- as.numeric(as.character(week[,'3']))
week[,'4'] <- as.numeric(as.character(week[,'4']))
week[,'5'] <- as.numeric(as.character(week[,'5']))


# test:

isWeekend <- c(which(week[,6]=='Sat'),which(week[, 6]=='Sun') )
isWeekday <- which(week[,6]!='Sat' & week[, 6]!='Sun' )

n1<- length(isWeekday)
n2<- length(isWeekend)

S1<-cov(week[isWeekday,1:5])
S2<-cov(week[isWeekend,1:5])

Spooled<- ((n1-1)*S1 + (n2-1)*S2)/(n1+n2-2)
inv.Spooled<-solve(Spooled, tol=0) 

delta.0<- c(0,0,0,0,0)
alpha<-0.5

m1<-colMeans(week[isWeekday,1:5])
m2<-colMeans(week[isWeekend,1:5])

T2<- (1/n1 + 1/n2)*(m1-m2)%*%inv.Spooled%*%(m1-m2)

cfr.fisher<- (n1+n2-2)/(n1+n2-1-p)*qf(alpha, p, n1+n2-1-p)

#reject?
T2 > cfr.fisher #no stat evidene to reject

pval<- 1- pf(T2, p, n1+n2-1-p )
pval #very large
# no evidence to reject! We see that the distribution of traffic around
# the city is the same every day. Pay attention that this analysis refers
# to the relative traffic: in absolute values (i.e. number of people moving
# per day around the city) there are evident differences between weekend
# and weekdays.