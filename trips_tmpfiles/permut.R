##########################################################
#testing mean with permutation test (univariate variables)
##########################################################

##########################################################
# MORNING vs MIDDAY
##########################################################

x1 <- morning
x2 <- midday

#reshuffling with MC
MC_num <- 1e5

#computing reference statistics T0
T0 <- abs(mean(x1)-mean(x2))

#merging the vectors
x_all <- c(x1, x2)
T_distrib <- rep(0,MC_num)

for (iter in 1:MC_num){
  my_shuffle <- sample(x_all,length(x_all))
  new_x1 <- my_shuffle[1:150]
  new_x2 <- my_shuffle[151:300]
  T_distrib[iter] <- abs(mean(new_x1)-mean(new_x2))
  if (iter%%(MC_num/10)==0){
    message(iter/MC_num*100,"%") #check avanzamento
  }
}

# T_distrib is a (sample of a) DISCRETE DISTRIBUTION 

hist(T_distrib,breaks = 100) 

p_val <- length( which(T_distrib>=T0) ) / MC_num
p_val

#
# pval = 55%, NO EVIDENCE FOR REJECTION H0 -> same mean
#

##########################################################
# MORNING vs ANOON
##########################################################

x1 <- morning
x2 <- anoon

#reshuffling with MC
MC_num <- 1e5

#computing reference statistics T0
T0 <- abs(mean(x1)-mean(x2))

#merging the vectors
x_all <- c(x1, x2)
T_distrib <- rep(0,MC_num)

for (iter in 1:MC_num){
  my_shuffle <- sample(x_all,length(x_all))
  new_x1 <- my_shuffle[1:150]
  new_x2 <- my_shuffle[151:300]
  T_distrib[iter] <- abs(mean(new_x1)-mean(new_x2))
  if (iter%%(MC_num/10)==0){
    message(iter/MC_num*100,"%") #check avanzamento
  }
}

p_val <- length( which(T_distrib>=T0) ) / MC_num
p_val

#
# pval = 29%, NO EVIDENCE FOR REJECTION H0 -> same mean
#

##########################################################
# MORNING vs NIGHT
##########################################################

x1 <- morning
x2 <- night

#reshuffling with MC
MC_num <- 1e5

#computing reference statistics T0
T0 <- abs(mean(x1)-mean(x2))

#merging the vectors
x_all <- c(x1, x2)
T_distrib <- rep(0,MC_num)

for (iter in 1:MC_num){
  my_shuffle <- sample(x_all,length(x_all))
  new_x1 <- my_shuffle[1:150]
  new_x2 <- my_shuffle[151:300]
  T_distrib[iter] <- abs(mean(new_x1)-mean(new_x2))
  if (iter%%(MC_num/10)==0){
    message(iter/MC_num*100,"%") #check avanzamento
  }
}

p_val <- length( which(T_distrib>=T0) ) / MC_num
p_val

#
# pval = 0;  chiaramente, medie diversissime
#

##########################################################
# EVENING vs NIGHT
##########################################################

x1 <- evening
x2 <- night

#reshuffling with MC
MC_num <- 1e5

#computing reference statistics T0
T0 <- abs(mean(x1)-mean(x2))

#merging the vectors
x_all <- c(x1, x2)
T_distrib <- rep(0,MC_num)

for (iter in 1:MC_num){
  my_shuffle <- sample(x_all,length(x_all))
  new_x1 <- my_shuffle[1:150]
  new_x2 <- my_shuffle[151:300]
  T_distrib[iter] <- abs(mean(new_x1)-mean(new_x2))
  if (iter%%(MC_num/10)==0){
    message(iter/MC_num*100,"%") #check avanzamento
  }
}

p_val <- length( which(T_distrib>=T0) ) / MC_num
p_val

#
# pval = 0
#

##########################################################
# TESTING ANOVA -> MORNING - MIDDAY - AFTERNOON
##########################################################

FisherF3 <- function(y1,y2,y3){
  g <- 3
  overall_mean <- mean( c(y1,y2,y3) )
  num <- ( (mean(y1)-overall_mean)^2 + (mean(y2)-overall_mean)^2 + (mean(y3)-overall_mean)^2 ) / (g-1)
  den1 <- sum ( (y1-overall_mean)^2 ) / (length(y1)-g)
  den2 <- sum ( (y2-overall_mean)^2 ) / (length(y2)-g)
  den3 <- sum ( (y3-overall_mean)^2 ) / (length(y3)-g)
  
  return ( num/(den1+den2+den3)  )
}

x1 <- morning
x2 <- midday
x3 <- anoon

#reshuffling with MC
MC_num <- 1e5

#computing reference statistics T0
T0 <- FisherF3(x1,x2,x3)

#merging the vectors
x_all <- c(x1, x2,x3)
T_distrib <- rep(0,MC_num)

for (iter in 1:MC_num){
  my_shuffle <- sample(x_all,length(x_all))
  new_x1 <- my_shuffle[1:150]
  new_x2 <- my_shuffle[151:300]
  new_x3 <- my_shuffle[301:450]
  T_distrib[iter] <- FisherF3(new_x1,new_x2,new_x3)
  if (iter%%(MC_num/10)==0){
    message(iter/MC_num*100,"%") #check avanzamento
  }
}

hist(T_distrib,breaks = 100) 

p_val <- length( which(T_distrib>=T0) ) / MC_num
p_val

#
# pval = 21%, NO EVIDENCE FOR REJECTION H0
# -> WE HAVE TO CONCLUDE THAT ALL DATA COME FROM SAME POPULATION
#
# NB: se mettiamo EVENING o NIGHT al posto di anoon, p=0-macchina
# 


######################################################################
##############                                      ##################
##############   DIVIDIAMO WEEKDAYS and WEEKENDS    ##################
##############                                      ##################
######################################################################



x1 <- week.traffic.time.slots$fr_morning
x2 <- week.traffic.time.slots$fr_midday

#reshuffling with MC
MC_num <- 1e5

#computing reference statistics T0
T0 <- abs(mean(x1)-mean(x2))

#merging the vectors
x_all <- c(x1, x2)
T_distrib <- rep(0,MC_num)

for (iter in 1:MC_num){
  my_shuffle <- sample(x_all,length(x_all))
  new_x1 <- my_shuffle[1:150]
  new_x2 <- my_shuffle[151:300]
  T_distrib[iter] <- abs(mean(new_x1)-mean(new_x2))
  if (iter%%(MC_num/10)==0){
    message(iter/MC_num*100,"%") #check avanzamento
  }
}

p_val <- length( which(T_distrib>=T0) ) / MC_num
p_val

# GRANDISSIMA EVIDENZA a rifiutare l'ipotesi di uguale media
# -> in settimana la distribuzione del traffico non è omogenea tra mattina-mezzogiono