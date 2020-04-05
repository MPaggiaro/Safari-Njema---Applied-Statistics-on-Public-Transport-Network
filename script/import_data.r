files =list.files("local_data/45min/45min/Tracks/")
df_tracks = read.table(paste("local_data/45min/45min/Tracks/",files[1]), header = TRUE, sep = " ")

for (j in 2:length(files)){
  file_path = paste("local_data/45min/45min/Tracks/",files[j], sep = "")
  cur_df = read.table(file_path, header = TRUE, sep = " ");
  df_tracks = rbind(df, cur_df)
}

cnt = 0;
for(j in 1:40){
  if( mod(((j+1)*j/2), 3) == 0)
  {
    print(j)
  }
}

print("ciao maddi")
