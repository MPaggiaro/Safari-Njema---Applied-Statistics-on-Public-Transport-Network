
for (i in 1:max(df_tracks$journey_id)) {
journey_1_list_tracks<-empty_list <- vector(mode = "list", length = 18)


k=1
j=1
for (j in df_tracks$journey_id){
  if (j==i) 
  {journey_1_list_tracks[[k]]= df_tracks[k,1:18]
  k = k+1}
  j=j+1
}


journey_1_tracks <- matrix(unlist(journey_1_list_tracks), ncol = 18, byrow = TRUE)


  
colnames(journey_1_tracks)<-c("id","device_type","distance_from_previous_mts","journey_id","record_id","lat","lng",
"Easting","Northing","timestamp","hour","stop_here","stop_duration","stop_count","extra_stopduration","distance_run","speed","day")



  myfile <- file.path("C:/Users/39346/Documents/appstat_project/local_data/journey/", paste0("Journey", "_", i, ".txt"))  #put here the correct path for your computer
  write.table(journey_1_tracks, file = myfile, sep = " ,", row.names = FALSE, col.names = TRUE,
              quote = FALSE, append = FALSE)
}
