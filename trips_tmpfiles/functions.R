#utility functions for tmp files

# my_map <- readOGR(
#   dsn= paste0(getwd(),"/trips_tmpfiles/shp_files") ,
#   layer="Maputo5distr",
#   verbose=FALSE
# )

#INPUT : 
#- DATASET of type TRIPS
#- shapefile with the desires region, with data organized as in Maputo5distr.shp

add_zones_labels<- function (df_trips_pos, my_map){

  # departures
  dep_data <- data.frame( Longitude=df_trips_pos$dep_lng, Latitude=df_trips_pos$dep_lat )


  coordinates(dep_data) <- ~ Longitude + Latitude
  proj4string(dep_data) <- proj4string(my_map)

  district <- over(dep_data,my_map)
  district <- district[,c(2,1)]


  df_trips_district <- cbind(df_trips_pos, c(district))
  names(df_trips_district)[names(df_trips_district) == "Zone_ID"] <- "dep_ID"
  names(df_trips_district)[names(df_trips_district) == "name"] <- "dep_zone"

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
  
  return(df_trips_district)

}

#INPUT: 
# - df_trips_district dataframe 
# - mondayDay : starting day of the week you want to study
# - depZone
# - startH= 0 and endH =24 for setting the time slot of interest, otherwise by default all day long

weekly_test <- function(df_trips_district, mondayDay, depZone, startH= 0, endH =24){
  
  N<-7
  baseday=mondayDay
  pval_mtrx <- matrix(0, N,  N) 
  colnames(pval_mtrx)<-c("mon", "tue", "wed","thu", "fri", "sat", "sun")
  rownames(pval_mtrx)<-c("mon", "tue", "wed","thu", "fri", "sat", "sun")
  
  #set time slot
  
  df_trips_district<- df_trips_district[ df_trips_district$dep_hour >= startH && df_trips_district$dep_hour <= endH ,]
  
  
  for ( i in seq(1,N-1, by=1) ){
    df_dist_1 <- df_trips_district[df_trips_district$dep_ID == depZone & df_trips_district$day == baseday + i - 1 , 
                                   grep("arr_ID", colnames(df_trips_district))]
    for ( j in seq(i,N-1) ){
      df_dist_2 <- df_trips_district[df_trips_district$dep_ID == depZone & df_trips_district$day == baseday + j , 
                                     grep("arr_ID", colnames(df_trips_district))]
      
      #df_pearson <- cbind(day1 = count(df_dist_1)[,2],day2 = count(df_dist_2)[,2])
      
      xx<-count(df_dist_1)
      yy<-count(df_dist_2)
      
      #check frequencies vectors for dimension and order
      #for xx
      
      miss.x<-which(!(zonesID %in% xx$x))
      add.row.x<- data_frame(x=miss.x, freq=rep(0, length(miss.x)))
      xx<-rbind(xx, add.row.x)
      xx<-xx[order(xx$x), ]
      
      #for yy
     
      miss.y<-which(!(zonesID %in% yy$x))
      add.row.y<- data_frame(x=miss.y, freq=rep(0, length(miss.y)))
      yy<-rbind(yy, add.row.y)
      yy<-yy[order(yy$x), ]
      
      
      df_pearson <- cbind(day1 = xx[,2],day2 = yy[,2])
      
      
      #print(df_pearson) 
      result <- chisq.test(df_pearson) 
      pval_mtrx[i,j+1] <- result$p.value # collecting p_values
    }
  }
  
  return(pval_mtrx)
}

#exporting matrix of simulated p values
weekly_test.sim <- function(df_trips_district, mondayDay, depZone, startH= 0, endH =24){
  
  N<-7
  baseday=mondayDay
  pval_mtrx <- matrix(0, N,  N) 
  colnames(pval_mtrx)<-c("mon", "tue", "wed","thu", "fri", "sat", "sun")
  rownames(pval_mtrx)<-c("mon", "tue", "wed","thu", "fri", "sat", "sun")
  
  #set time slot
  
  df_trips_district<- df_trips_district[ df_trips_district$dep_hour >= startH && df_trips_district$dep_hour <= endH ,]
  
  
  for ( i in seq(1,N-1, by=1) ){
    df_dist_1 <- df_trips_district[df_trips_district$dep_ID == depZone & df_trips_district$day == baseday + i - 1 , 
                                   grep("arr_ID", colnames(df_trips_district))]
    for ( j in seq(i,N-1) ){
      df_dist_2 <- df_trips_district[df_trips_district$dep_ID == depZone & df_trips_district$day == baseday + j , 
                                     grep("arr_ID", colnames(df_trips_district))]
      
      xx<-count(df_dist_1)
      yy<-count(df_dist_2)
      
      #check frequencies vectors for dimension and order
      #for xx
      
      miss.x<-which(!(zonesID %in% xx$x))
      add.row.x<- data_frame(x=miss.x, freq=rep(0, length(miss.x)))
      xx<-rbind(xx, add.row.x)
      xx<-xx[order(xx$x), ]
      
      #for yy
      
      miss.y<-which(!(zonesID %in% yy$x))
      add.row.y<- data_frame(x=miss.y, freq=rep(0, length(miss.y)))
      yy<-rbind(yy, add.row.y)
      yy<-yy[order(yy$x), ]
      
      
      df_pearson <- cbind(day1 = xx[,2],day2 = yy[,2])
      
      #print(df_pearson) 
      result <- chisq.test(df_pearson, simulate.p.value = TRUE)
      #print(result)
      pval_mtrx[i,j+1] <- result$p.value # collecting p_values
    }
  }
  
  return(pval_mtrx)
}
