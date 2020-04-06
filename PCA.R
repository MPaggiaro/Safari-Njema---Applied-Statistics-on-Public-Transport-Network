
#Standardize the variables of TRIPS_POS

trips.label<-df_trips_pos[, 1:4]
trips.positions<-df_trips_pos[, -(1:4)]

trips.positions.sd<-scale(trips.positions)
trips.positions.sd<-data.frame(trips.positions.sd)

attach(trips.positions.sd)

# VISUALIZATION of original data

#1.distance and duration of travels
x11() 
plot(distance, duration, asp=1, pch='*', ylim = c(-8,8))

x11()
par(mfcol=c(1, 2))
hist(distance, prob=T, xlim = c(-5, 5), density=20, col = 'red', ylim = c(0,0.9))
lines((-1000):1000 /100, dnorm((-1000):1000 /100,mean(distance),sd(distance)), col='blue', lty=2)
hist(duration, prob=T, xlim = c(-5, 5), density=20, col = 'red')
lines((-1000):1000 /100, dnorm((-1000):1000 /100,mean(duration),sd(duration)), col='blue', lty=2)

#2.Visualize varibility in the data

#boxplot non standardized
x11()
boxplot(trips.positions, las=2, col='gold', main='Non-standardized')
#boxplot standardized variables, to make elements comparable
x11()
boxplot(trips.positions.sd, las=2, col='gold', main='Standardized variables')


#Scatter plot of all variables

x11(width=15, height=10)
#pairs(trips.positions.sd, pch='*', col='blue') #Too many variables: meaningless

#try with less variables (the most informative ones): dep_timestamp, dep_hour, arr_timestamp, arr_hour, duration, distance 

trips.small.sd<-trips.positions.sd[, c(5,6,11,12,13,14)]

x11()
pairs(trips.small.sd) #The only clear dependence is about dep and arr times
#Too many statistical units to well visualize the data

#Try with a subset of sample: SCATTERPLOTS ON RANDOM SAMPLE of DATA

x11(width=15, height=10)
sample.trips<- trips.positions.sd[sample(nrow(trips.positions.sd), 1000), ]
pairs(sample.trips, pch='*', col='blue', main= 'Scatterplot')

x11()
sample.trips.small<- trips.small.sd[sample(nrow(trips.small.sd), 1000), ]
pairs(sample.trips.small, pch='*', col='blue', main= 'Scatterplot')


graphics.off()

#Compute PCA on standardized variables (PCA on Correlation matrix)

pc.trips.positions.sd <- princomp(trips.positions.sd, scores=T)
pc.trips.positions.sd 
summary(pc.trips.positions.sd )


# Explained variance 
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(pc.trips.positions.sd$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
abline(h=1, col='blue')
barplot(sapply(trips.positions.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
plot(cumsum(pc.trips.positions.sd$sd^2)/sum(pc.trips.positions.sd$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='red')
abline(h=0.9, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(trips.positions.sd),labels=1:ncol(trips.positions.sd),las=2)

#Looking at threshold 0.9 we can choose 7 PCs, with 0.8 we select the first 5 PCs


#We can look only at the graphical representation of the first 5 PC's 
#(looking at cumulative proportion they explain 80% of the variability)

load.trips <- pc.trips.positions.sd $loadings

x11(width=21, height=7)
par(mfcol = c(3,2))
for(i in 1:5) barplot(load.trips[,i], ylim = c(-1, 1), density=rep(20,17), col = 'red', cex.names =0.9, las=2, main = paste('PC', i))
#difficult interpretation

# let's plot only the most significant loadings

x11(width=20, height=9)
par(mfcol = c(3,2))
for(i in 1:5) {
  barplot(ifelse(abs(load.trips[,i]) < 0.3, 0, load.trips[,i]) , ylim = c(-1, 1),density=rep(20,17), col = 'red', cex.names =0.9,
          main = paste('PC', i), las=2);
  abline(h=0)
  }

# Analysis of the scores

# scores
scores.trips.positions <- pc.trips.positions.sd$scores
scores.trips.positions

# variability of the original variables / scores
x11(width=18, height=9)
layout(matrix(c(1,2),2))
boxplot(trips.positions.sd, las=2, col='gold', main='Original variables')
scores.trips.positions<- data.frame(scores.trips.positions)
boxplot(scores.trips.positions, las=2, col='gold', main='Principal components')

