#GOAL: PROVARE con evidenza statistica che tutti i giorni della settimana hanno una stessa distribuzione dei viaggi
#distribuzione qui intesa come frequenza di partenza da una data zona verso le altre o rimanenti nella stessa


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
 
 
 ############################ 
 
 #Non so perchè si salvano come char, brutto ma devo frozare le colonne a numeric
 week[,'1'] <- as.numeric(as.character(week[,'1']))
 week[,'2'] <- as.numeric(as.character(week[,'2']))
 week[,'3'] <- as.numeric(as.character(week[,'3']))
 week[,'4'] <- as.numeric(as.character(week[,'4']))
 week[,'5'] <- as.numeric(as.character(week[,'5']))
 
 
 week<-week[-1, ]
 
#TOLGO LA DIPENDEZA LINEARE
###################################################################################################################
 
 # Model I'd like to fit: one-factor MANOVA
 ##Two-ways MANOVA (complete model with interaction)
 #Xij = mu + tau.i  + Eps.i, j
 #i=1, ..5 (g=5)
 
 
 g<-4
 p<-5
 n<-dim(week)[1] #150 observation
 
 week.freq<-week[, c(1,2,3,4)]
 week.groups<-week[, 6]
 groups <- factor(week.groups, labels = c('M', 'T', 'W', 'Th', 'F')) # Treat.1
 levels(groups)
 
 #Fit the model
 
 fit<-manova(as.matrix(week.freq) ~ groups)
 summary.manova(fit) # I need to put tol=0 because of an error regarding the rank :"i residui hanno rango 4 <5 "
 summary.manova(fit, test="Wilks")
 
 
 ###high Pval---> accept the HO : same distribution ( even from DEP 4 p val =0.89)
 
 summary.aov(fit)
 
 #Check assumptions
 
 iM <- which(week.groups=='M' )
 iT <- which(week.groups=='T' )
 iW <- which(week.groups=='W' )
 iTh <- which(week.groups=='Th' )
 iF <- which(week.groups=='F' ) 
 
 
 #a) gaussinity
 
 #1)
 pval<-c(mcshapiro.test(week.freq[iM, ])$p, 
         mcshapiro.test(week.freq[iT, ])$p, 
         mcshapiro.test(week.freq[iW, ])$p, 
         mcshapiro.test(week.freq[iTh, ])$p, 
         mcshapiro.test(week.freq[iF, ])$p)
 
 pval
 
 #NO gaussianity 
 
 
######################################################################################################################
# Trasformazione logit del dataset per avere dati gaussiani:
 logit.week <- week
 logit.week$`1`<- logit(week$`1`)
 logit.week$`2`<- logit(week$`2`)
 logit.week$`3`<- logit(week$`3`)
 logit.week$`5`<- logit(week$`5`)
 logit.week$`4`<- logit(week$`4`)
 
 #no arrivi in 3 (per non avere dipendeza lineare)
 
 logit.week.freq<-logit.week[ , c(1,2,4,5,6)]

 
 #a) gaussinity
 
 logit.pval<-c(mcshapiro.test(logit.week.freq[logit.week.freq$day=='M', 1:4 ])$p, 
         mcshapiro.test(logit.week.freq[logit.week.freq$day=='T', 1:4 ])$p, 
         mcshapiro.test(logit.week.freq[logit.week.freq$day=='W', 1:4 ])$p, 
         mcshapiro.test(logit.week.freq[logit.week.freq$day=='Th', 1:4 ])$p, 
         mcshapiro.test(logit.week.freq[logit.week.freq$day=='F', 1:4 ])$p)

 logit.pval
 
 #prova outilier su MONDAY
 x11()
 plot(logit.week.freq[logit.week.freq$day=='M', 1:4 ])
 
 n<-dim(logit.week.freq[logit.week.freq$day=='M', 1:4 ])[1]
 n
 mon<-logit.week.freq[logit.week.freq$day=='M', 1:4 ]
 
 mon<-mon[mon$`1` <(-2.2) | mon$`2` < (-2.4), ]
 n<-dim(mon)[1]
 n
 
 x11()
 plot(mon)
 mcshapiro.test(mon)$p #---OK gaussinity 
 
 # set correct index in original logit dataset: 
 mon<-logit.week.freq[logit.week.freq$day=='M', 1:4 ]
 idx.out.mon<-which(mon$`1` >(-2.2) & mon$`2` > (-2.4))
 idx.out.mon
 
 logit.week.freq<-logit.week.freq[ -idx.out.mon , ]

 
 #prova outilier su FRIDAY
 
 x11()
 plot(logit.week.freq[logit.week.freq$day=='F', 1:4 ])
 
 n<-dim(logit.week.freq[logit.week.freq$day=='F', 1:4 ])[1]
 n
 fri<-logit.week.freq[logit.week.freq$day=='F', 1:4 ]
 
 fri<-fri[fri$`2` > (-3.1), ]
 n<-dim(fri)[1]
 n
 
 x11()
 plot(fri)
 mcshapiro.test(fri)$p #---OK gaussinity 
 
 
 # set correct index in dataset: 
 fri<-logit.week.freq[logit.week.freq$day=='F', 1:4 ]
 idx.out.fri<-which(fri$`2` < (-3.1))
 idx.out.fri
 
 #reset whole indexes
 idx.out.fri <- idx.out.fri + (which(logit.week.freq$day=='F')[1] -1)
 idx.out.fri
 logit.week.freq<-logit.week.freq[ -idx.out.fri , ]
 
 
 #chech gaussinity of the groups now: 
 
 logit.pval.new<-c(mcshapiro.test(logit.week.freq[logit.week.freq$day=='M', 1:4 ])$p, 
               mcshapiro.test(logit.week.freq[logit.week.freq$day=='T', 1:4 ])$p, 
               mcshapiro.test(logit.week.freq[logit.week.freq$day=='W', 1:4 ])$p, 
               mcshapiro.test(logit.week.freq[logit.week.freq$day=='Th', 1:4 ])$p, 
               mcshapiro.test(logit.week.freq[logit.week.freq$day=='F', 1:4 ])$p)
 
 logit.pval.new
 logit.pval
 
 #OK we obtain gaussiniity in the groups!!
 
 #b) variance homogeneity : NO EQUAL VARIANCES
 
 S1<-cov(logit.week.freq[logit.week.freq$day=='M', 1:4 ])
 S2<-cov(logit.week.freq[logit.week.freq$day=='T', 1:4 ])
 S3<-cov(logit.week.freq[logit.week.freq$day=='W', 1:4 ])
 S4 <-cov(logit.week.freq[logit.week.freq$day=='Th', 1:4 ])
 S5<-cov(logit.week.freq[logit.week.freq$day=='F', 1:4 ])
 
 x11()
 par(mfrow= c(3,2))
 image(S1, col=heat.colors(100), asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE)) 
 image(S2, col=heat.colors(100), asp=1,axes=FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
 image(S3, col=heat.colors(100), asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
 image(S4, col=heat.colors(100), asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
 image(S5, col=heat.colors(100), asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
 
 #Fit the model
 
 week.groups<-logit.week.freq[, 5]
 groups <- factor(week.groups, labels = c('M', 'T', 'W', 'Th', 'F')) # Treat.1
 levels(groups)
 
 
 fit<-manova(as.matrix(logit.week.freq[, 1:4]) ~ groups)
 summary.manova(fit) 
 summary.manova(fit, test="Wilks") 
 
 summary.aov(fit)
 
 ###high Pval---> accept the HO : same distribution (Wilks  p val =0.9)
 
 
#############################################################################################
 
 #####                  WEEKEND COMPARISON (SATURDAYS AND SUNDAYS)               ############
 
#############################################################################################
 
 
#GOAL 2: Provare a vedere se  i weekend (sab e domanica) hanno la stessa distribuzione di traffico 
 
#in this case p>= 1 and g=2

 #Create dataset:  
 
 weekEND<-data.frame( 10, 10,10,10,10, NA)
 colnames(weekEND)<-c(zonesID, "day")
 p<-5
 g<-2
 
 n1< length(sat)
 n2<- length(sunday)
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
    weekEND<-rbind(weekEND, c(as.numeric(freq.df$freq), 'Sat'))
 }
 
 #sistemo la matrice week che contiene il dataset multivariato per fare l'anova
 
 weekEND<-weekEND[!is.na(weekEND[, 6]),] 
 
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
    
    weekEND<-rbind(weekEND, c(as.numeric(freq.df$freq), 'Sun') )
 }
 
 weekEND[,'1'] <- as.numeric(as.character(weekEND[,'1']))
 weekEND[,'2'] <- as.numeric(as.character(weekEND[,'2']))
 weekEND[,'3'] <- as.numeric(as.character(weekEND[,'3']))
 weekEND[,'4'] <- as.numeric(as.character(weekEND[,'4']))
 weekEND[,'5'] <- as.numeric(as.character(weekEND[,'5']))

 
 iSat <- which(weekEND[,6]=='Sat' )
 iSUN <- which(weekEND[, 6]=='Sun' )
 
 n1<- length(sat)
 n2<- length(sunday)
 
 
 #take only 4 frequencies ( get rid of the 5th one, whihch is linear dependent)
 cols<-c(2,3,4,5)
 S1<-cov(weekEND[iSat,cols])
 S2<-cov(weekEND[iSUN,cols])
 
 Spooled<- ((n1-1)*S1 + (n2-1)*S2)/(n1+n2-2)
 inv.Spooled<-solve(Spooled) 

 delta.0<- c(0,0,0,0)
 alpha<-0.5
 
 m1<-colMeans(weekEND[iSat,cols])
 m2<-colMeans(weekEND[iSUN,cols])
 
 T2<- (1/n1 + 1/n2)*(m1-m2)%*%inv.Spooled%*%(m1-m2)
 
 cfr.fisher<- (n1+n2-2)/(n1+n2-1-p)*qf(alpha, p, n1+n2-1-p)
 
 #regect?
 T2 > cfr.fisher #no stat evidene to reject
 
 pval<- 1- pf(T2, p, n1+n2-1-p )
 pval #very large : H0= equal distribution
 
#They are the same !! 
 
#check assumptions on gaussianity and covariance

 # GAUSSIANITY:  
pval<-c(mcshapiro.test(weekEND[iSat,cols])$p,
        mcshapiro.test(weekEND[iSUN,cols])$p )

pval #ottimo, sembra gaussiano

#plotto i dati per vedere la nuvola, e non strana dipendenza --> sembra ok

x11()
plot(weekEND[iSat,cols])
x11()
plot(weekEND[iSUN,cols])

#VARIANCE HOMOGENEITY
x11()
par(mfrow= c(1, 2))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))

#non proprio, forse accettabile?

#Non abbiamo per niente un large sample, quindi non possiamo usare il risultato asintotitco!
graphics.off()
 
 
