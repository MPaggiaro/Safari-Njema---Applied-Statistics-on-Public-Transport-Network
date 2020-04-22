# Visualize data 
source("script/utility.R")


full_import_data()
cutted_df_trips_pos = cut_dataset_by_id(df = df_trips_pos, 1)
plot_starting_points(cutted_df_trips_pos)
plot_arrival_points(cutted_df_trips_pos)
  
# non funziona ancora: plot_connections(cutted_df_trips_pos)
