#GOAL: PROVARE con evidenza statistica che tutti i giorni della settimana hanno una stessa distribuzione dei viaggi
#distribuzione qui intesa come frequenza di partenza da una data zona verso le altre o rimanenti nella stessa


#settings
library(rgdal)
library(plyr)
source("trips_tmpfiles/functions.R", encoding = "UTF-8")



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
 
 
 ############################ 
 
 #Non so perchè si salvano come char, brutto ma devo frozare le colonne a numeric
 week[,'1'] <- as.numeric(as.character(week[,'1']))
 week[,'2'] <- as.numeric(as.character(week[,'2']))
 week[,'3'] <- as.numeric(as.character(week[,'3']))
 week[,'4'] <- as.numeric(as.character(week[,'4']))
 week[,'5'] <- as.numeric(as.character(week[,'5']))
 
 
#TOLGO LA DIPENDEZA LINEARE
week<- week[ , c(1,2,4,5,6)]
 
 # Model I'd like to fit: one-factor MANOVA
 ##Two-ways MANOVA (complete model with interaction)
 #Xij = mu + tau.i  + Eps.i, j
 #i=1, ..5 (g=5)
 
 
 g<-4
 p<-5
 n<-dim(week)[1] #150 observation
 
 week.freq<-week[, c(1,2,3,4)]
 week.groups<-week[, 5]
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
 #gaussianity abbastanza ok tranne per il primo gruppo, forse da rivedere mangari lavorando con outliers
 
 # DEP==4 gaussinità ok per tutti i gruppi
 # DEP== 1 gaussinità ok (solo strano warning in mcshapiro.test)
 
 #2)
 S1<-cov(week.freq[iM, ])
 S2<-cov(week.freq[iT, ])
 S3<-cov(week.freq[iW, ])
 S4 <-cov(week.freq[iTh, ])
 S5<-cov(week.freq[iF, ])
 
 x11()
 par(mfrow= c(3,2))
 image(S1, col=heat.colors(100), asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE)) 
 image(S2, col=heat.colors(100), asp=1,axes=FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
 image(S3, col=heat.colors(100), asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
 image(S4, col=heat.colors(100), asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
 image(S5, col=heat.colors(100), asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
 
 #per me accettabile!!
 
# DEP==4 covarianza boh
# DEP== 1 covarianza boh
 
 
 
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
 
 S1<-cov(weekEND[iSat,1:5])
 S2<-cov(weekEND[iSUN,1:5])
 
 Spooled<- ((n1-1)*S1 + (n2-1)*S2)/(n1+n2-2)
 inv.Spooled<-solve(Spooled, tol=0) # attenzione sarebbe numericamente singolare= valore di condizione di reciprocità = 1.36748e-17

 delta.0<- c(0,0,0,0,0)
 alpha<-0.5
 
 m1<-colMeans(weekEND[iSat,1:5])
 m2<-colMeans(weekEND[iSUN,1:5])
 
 T2<- (1/n1 + 1/n2)*(m1-m2)%*%inv.Spooled%*%(m1-m2)
 
 cfr.fisher<- (n1+n2-2)/(n1+n2-1-p)*qf(alpha, p, n1+n2-1-p)
 
 #regect?
 T2 > cfr.fisher #no stat evidene to reject
 
 pval<- 1- pf(T2, p, n1+n2-1-p )
 pval #very large
 
#They are the same !! 
 
#NB: check for singularity and indepeendence assumption
 
