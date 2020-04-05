filelist_trips = list.files(path= "C:/Users/39346/Documents/Dataset_SAFARI/Journeys/45min/Trips/",recursive=TRUE, pattern='*.txt', full.names = TRUE)
datalist_trips = lapply(filelist_trips, function(x)read.table(x, header=T)) 
datafr_trips = do.call("rbind", datalist_trips) 

filelist_track = list.files(path= "C:/Users/39346/Documents/Dataset_SAFARI/Journeys/45min/Tracks/",recursive=TRUE, pattern='*.txt', full.names = TRUE)
datalist_track = lapply(filelist_track, function(x)read.table(x, header=T)) 
datafr_track = do.call("rbind", datalist_track) 