#setwd("/Users/pietro/Documents/appstat_project")
#rm(list = ls())

library(plyr)

if (!exists("df_tracks")){
  df_trips <- read.table("local_data/df_trips_pos.txt")
}

# TODO: definire le macroaree! Per ora aree identificate come [1,50] [51,100] etc
# importante definirle sufficientemente grandi, perchè Pearson richiede
# una frequenza non nulla in ognuna (almeno 5-10 rilevamenti in ogni area)

df_dist_1 <- df_trips[df_trips$depName <= 50 & df_trips$day == 31, 
                       grep("arrName", colnames(df_trips))]
df_dist_2 <- df_trips[df_trips$depName <= 50 & df_trips$day == 33, 
                       grep("arrName", colnames(df_trips))]

for (i in seq(1,length(df_dist_1))){
  df_dist_1[i] <- floor(df_dist_1[i]/50)
}
for (i in seq(1,length(df_dist_2))){
  df_dist_2[i] <- floor(df_dist_2[i]/50)
}

# TODO: occhio a cbind: se non qualche zona ha frequenza nulla, lunghezze non corrispondono
df_pearson <- cbind(day1 = count(df_dist_1)[,2],day2 = count(df_dist_2)[,2])
chisq.test(df_pearson)


### Pearson test fatto a mano (stesso risultato)
# a <- count(df_dist_1)
# colnames(a)[2] <- "freq1"
# b <- count(df_dist_2)
# colnames(b)[2] <- "freq2"
# df_freq <- join( a, b )
# statX2 <- 0
# card1 <- sum(df_freq$freq1)
# card2 <- sum(df_freq$freq2)
# n <- card1 + card2
# for (i in seq(1,dim(df_freq)[1])){
#   E_i1 <- card1 * (df_freq$freq1[i] + df_freq$freq2[i]) /n
#   E_i2 <- card2 * (df_freq$freq1[i] + df_freq$freq2[i]) /n
#   statX2 <- statX2 + df_freq$freq1[i]^2 / E_i1 + df_freq$freq2[i]^2 / E_i2
# }
# statX2 <- statX2 - n
# 1-pchisq(statX2, dim(df_freq)[1]-1) #pvalue


