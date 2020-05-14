
source("trips_tmpfiles/pearson_test_LastVersion.R", encoding = "UTF-8")

###########################################################################################
# IDEE: TO DO provare a usare gli orari, confrontare le diverse settimane magari in un ciclo ecc

source("trips_tmpfiles/functions.R", encoding = "UTF-8")

if (!exists("df_trips_pos")){
  df_trips_pos <- read.table("local_data/df_trips_pos.txt")
}

my_map <- readOGR( 
  dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") , 
  layer="Maputo5distr",
  verbose=FALSE
)


df.trips.district<-add_zones_labels(df_trips_pos, my_map)

#remove tripes with non classified arrival or departure points
df.trips.district <- df.trips.district[!is.na(df.trips.district$dep_ID),]
df.trips.district <- df.trips.district[!is.na(df.trips.district$arr_ID),]


#weekly trend in departure from zone 3
lis3<- list()
lis3.sim<- list()
for (i in seq(33,179,7)){ #last monday considered is mondayDay=173 since 180 "does not have the week"
  
      
      m.pvalues<-weekly_test(df.trips.district, mondayDay=i, depZone=3)
      lis3[[ceiling((i-32)/7)]]<- m.pvalues #10 approx might be incorrect
      
      m.pvalues<-weekly_test.sim(df.trips.district, mondayDay=i, depZone=3)
      lis3.sim[[ceiling((i-32)/7)]]<- m.pvalues
      
}

#week trend in departure from zone 1
lis1<- list()
lis1.sim<-list()
for (i in seq(33,179,7)){ #last monday considered is mondayDay=173 since 180 "does not have the week"
  
  m.pvalues<-weekly_test(df.trips.district, mondayDay=i, depZone=1)
  lis1[[ceiling((i-32)/7)]]<- m.pvalues
  
  #with simulated pvalues, to avoid problems with number of obs
  m.pvalues<-weekly_test.sim(df.trips.district, mondayDay=i, depZone=1)
  lis1.sim[[ceiling((i-32)/7)]]<- m.pvalues
  
  
}


#week trend in departure from zone 2
lis2<- list()
lis2.sim<-list()
for (i in seq(33,179,7)){ #last monday considered is mondayDay=173 since 180 "does not have the week"
  
  m.pvalues<-weekly_test(df.trips.district, mondayDay=i, depZone=2)
  lis2[[ceiling((i-32)/7)]]<- m.pvalues
  
  #with simulated pvalues, to avoid problems with number of obs
  m.pvalues<-weekly_test.sim(df.trips.district, mondayDay=i, depZone=2)
  lis2.sim[[ceiling((i-32)/7)]]<- m.pvalues
  
}

#more than 50 warnings for each test in which probably commpare too many 0


#week trend in departure from zone 4
lis4<- list()
lis4.sim<-list()
for (i in seq(33,179,7)){ #last monday considered is mondayDay=173 since 180 "does not have the week"
  
  m.pvalues<-weekly_test(df.trips.district, mondayDay=i, depZone=4)
  lis4[[ceiling((i-32)/7)]]<- m.pvalues
  
  #with simulated pvalues, to avoid problems with number of obs
  m.pvalues<-weekly_test.sim(df.trips.district, mondayDay=i, depZone=4)
  lis4.sim[[ceiling((i-32)/7)]]<- m.pvalues}

#week trend in departure from zone 5
lis5<- list()
lis5.sim<-list()
for (i in seq(33,179,7)){ #last monday considered is mondayDay=173 since 180 "does not have the week"
  
  m.pvalues<-weekly_test(df.trips.district, mondayDay=i, depZone=5)
  lis5[[ceiling((i-32)/7)]]<- m.pvalues
  
  #with simulated pvalues, to avoid problems with number of obs
  m.pvalues<-weekly_test.sim(df.trips.district, mondayDay=i, depZone=5)
  lis5.sim[[ceiling((i-32)/7)]]<- m.pvalues
  
  
}




#####################################################################################
#Prova test multivariate repeated measures
library(MANOVA.RM)

