# Importing Tracks files
aggregate_txt = function(ref_path){
  
  # list all .txt files
  files = list.files(ref_path, pattern="\\.txt$")
  # contstruct initial dataframe from first file
  df_res = read.table(paste(ref_path,files[1], sep =""), header=TRUE, sep=" ")
  # adds day column, extracting the number from the filename
  day = as.numeric(rep(gsub('[^0-9]','\\1',files[1]), dim(df_res)[1]))
  df_res= cbind(df_res,day)

  # do the same thing with all the other files and append them to the dataframe
  for (j in 2:length(files)){
    print(paste("Opening: ", ref_path, files[j], sep=""))
    file_path = paste(ref_path,files[j], sep="")
    cur_df = read.table(file_path, header=TRUE, sep=" ")
    day = as.numeric(rep(gsub('[^0-9]','\\1',files[j]), dim(cur_df)[1]))
    cur_df = cbind(cur_df,day)
    df_res = rbind(df_res, cur_df)
  }
  
  #sort the result by day
  df_res = df_res[order(df_res$day),]
  
  return(df_res)
}

df_tracks = aggregate_txt('local_data/Tracks/')
df_trips_no_pos = aggregate_txt('local_data/Trips/')
df_trips_pos = aggregate_txt('local_data/Trips/WithPosInfo/')


# Save tables in txt files in local_data 
write.table(df_tracks, file="local_data/df_tracks.txt", sep=" ", dec=".", col.names=TRUE)
write.table(df_trips_no_pos, file="local_data/df_trips_no_pos.txt", sep=" ", dec=".", col.names=TRUE)
write.table(df_trips_pos, file="local_data/df_trips_pos.txt", sep=" ", dec=".", col.names=TRUE)
