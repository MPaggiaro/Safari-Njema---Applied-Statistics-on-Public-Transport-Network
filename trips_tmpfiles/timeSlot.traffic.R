#TESTING TRAFFIC for each ZONE comparing different time slots

#Dateset with freq for every zone, using always the 5 district (load them )

traffic.time.slots3<-read.table("trips_tmpfiles/ traffic.data") 
traffic.time.slots1<-read.table("trips_tmpfiles/ traffic.data.1") 
traffic.time.slots2<-read.table("trips_tmpfiles/ traffic.data.2") 
traffic.time.slots4<-read.table("trips_tmpfiles/ traffic.data.4") 
traffic.time.slots5<-read.table("trips_tmpfiles/ traffic.data.5") 




sabati<- seq(2,150, 7)
dom<- seq(3,150, 7)
weekend<- as.vector(rbind(sabati, dom))

week.traffic.time.slots3 <-traffic.time.slots3[-weekend,  c(2,4,6,8,10)]
week.traffic.time.slots1 <-traffic.time.slots1[-weekend,  c(2,4,6,8,10)]
week.traffic.time.slots2 <-traffic.time.slots2[-weekend,  c(2,4,6,8,10)]
week.traffic.time.slots4 <-traffic.time.slots4[-weekend,  c(2,4,6,8,10)]
week.traffic.time.slots5 <-traffic.time.slots5[-weekend,  c(2,4,6,8,10)]

head(week.traffic.time.slots3)


################################################################################################################################

#DATI ACCOPPIATI : confronto le varie fasce orarie creando delle differenza univariate

###########################################################################################################################

###TEST DATI ACCOPPIATI MATTINA vs MIDDAY e MIDDAY vs ANOON   - in WEEK DAYS

#zone3 

#MATTINA vs MIDDAY

#compute differences

D<-week.traffic.time.slots3$fr_morning - week.traffic.time.slots3$fr_midday
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D>-0.05)]
n<-length(D)
n #remove 6 

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 


D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#REJECT the NULL: evidence of difference of traffic between mornig and midday

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)
#-->   statistical evidence that:
# Il traffico nella zona 3 alla mattina è maggiore che a metà giornata

# 
# #Manually with multivariate procedure but p=1 = SAME RESULT
# D.t2 <- n * (D.mean-delta.0) *D.invcov *(D.mean-delta.0)
# D.t2
# 
# cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# cfr.fisher
# 
# 
# D.t2 < cfr.fisher # FALSE: we reject H0 at level 5%
# 
# # we compute the p-value from Fisher test
# P <- 1-pf(D.t2*(n-p)/(p*(n-1)), p, n-p)
# P



#MIDDAY vs ANOON

#compute differences

D<-week.traffic.time.slots3$fr_midday - week.traffic.time.slots3$fr_anoon
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

dev.off()

#coda destra un pochino pesante

shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D<0.09)]
n<-length(D)
n #2 outliers

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà con tolte due osservazioni

graphics.off()

#PAIRED univariate --> t-test 

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
print("D= midday- anoon")
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)

#p.val 0.0381 (not reject solo all 1%) con alpha 1% accetto l'ipotesi nulla (stessa media) 

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#pval = 0.01905 (not reject solo all 1%) con alpha 1% accetto l'ipotesi nulla (stessa media) 
#it seems that midday ha più traffico che il pomeriggio


#Morning vs ANOON
#compute differences

D<-week.traffic.time.slots3$fr_morning - week.traffic.time.slots3$fr_anoon
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)
#coda sinistra brutta 
shapiro.test(D) #strong evidence to reject

dev.off()
#rimuovo outliers

# D<-D[which(D<0.15 & D>=-0.06)]
# n<-length(D)
# n
# 
# x11()
# par(mfrow= c(1,3))
# plot(D, main="without outliers")
# hist(D, col='grey', prob=T,breaks = 50)
# lines(seq(min(D), max(D), length=2000),
#       dnorm(seq(min(D), max(D), length=2000),
#             mean(D),sd(D)), col='blue', lty=1)
# 
# qqnorm(D)
# qqline(D)
# 
# shapiro.test(D) #-->ANCORA BRUTTA CODA provo  a togliere altri outliers, avendo eliminato solo 3 obs fino adesso

D<-D[which(D<0.15 & D>-0.05)] 
n<-length(D) #n=101, due obs in meno rispetto a prima, in tot 5 in meno
n

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #PERFETTO

graphics.off()

#PAIRED univariate --> t-test 

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
print("morning - anoon")
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)
#-->p.val  4.963716e-22 --> evidence to reject null (equal mean)
t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#--> evidenza statistica che al mattino il traffico/affollamento nella zona 3
#    è maggiore che al pomeriggio



#MATTINA vs NIGHT

#compute differences

D<-week.traffic.time.slots3$fr_night - week.traffic.time.slots3$fr_morning
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda destra molto pesante

shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D< (-0.15))]
n<-length(D)
n #remove 5

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 

D.mean   <- mean(D)
D.mean
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#REJECT the NULL: evidence of difference of traffic between mornig and night

t.test(D, mu = delta.0, alternative = 'less', conf.level = 1-alpha) 

#-->   statistical evidence that in the night LESS traffic than in the morning


#ANOON vs EVENING

#compute differences

D<-week.traffic.time.slots3$fr_anoon - week.traffic.time.slots3$fr_evening
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda destra molto pesante

shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D > (0.07))]
n<-length(D)
n #remove 1

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) # Not reject gaussinity only at 1%

graphics.off()

#PAIRED univariate --> t-test 

D.mean   <- mean(D)
D.mean
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#REJECT the NULL: evidence of difference of traffic between mornig and night

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha) 

#-->   statistical evidence that in the evening LESS traffic than in the afternoon



################################################################################################################################

# Same tests for zone 1

head(week.traffic.time.slots1)

####################################################################################################################################

#MATTINA vs MIDDAY

#compute differences

D<-week.traffic.time.slots1$fr_morning - week.traffic.time.slots1$fr_midday
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D> (-0.01))]
n<-length(D)
n #remove 5

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#REJECT the NULL: evidence of difference of traffic between mornig and midday

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#---->STRONG evidence morning MORE traffic than midday



#ANOON vs MIDDAY

#compute differences

D<-week.traffic.time.slots1$fr_anoon - week.traffic.time.slots1$fr_midday
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) # poor

#rimuovo outliers
D<-D[which(D < (0.2) & D > (-0.05))]
n<-length(D)
n #remove 3

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#evidence to reject equality

#REJECT the NULL: evidence of difference of traffic between mornig and midday

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#STRONG evidence morning MORE traffic in the afternoon than in midday in zone 1 




#ANOON vs MORNING

#compute differences

D<-week.traffic.time.slots1$fr_anoon - week.traffic.time.slots1$fr_morning
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) # poor

#rimuovo outliers
D<-D[which(D < (0.1))]
n<-length(D)
n #remove 2

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#evidence to reject equality

#REJECT the NULL: evidence of difference of traffic between anoon and midday

t.test(D, mu = delta.0, alternative = 'less', conf.level = 1-alpha)

#STRONG evidence morning MORE traffic in the morning than in the afternoon



################################################################################################################################

# Same tests for zone 2

head(week.traffic.time.slots2)

####################################################################################################################################

#MATTINA vs MIDDAY

#compute differences

D<-week.traffic.time.slots2$fr_morning - week.traffic.time.slots2$fr_midday
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D> (-0.10))]
n<-length(D)
n #remove 5

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#REJECT the NULL: evidence of difference of traffic between mornig and midday

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#---->STRONG evidence morning MORE traffic than midday zone 2 



#ANOON vs MIDDAY

#compute differences

D<-week.traffic.time.slots2$fr_anoon - week.traffic.time.slots2$fr_midday
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) # poor

#rimuovo outliers
D<-D[which(D > (-0.15))]
n<-length(D)
n #remove 1

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#evidence to reject equality

#REJECT the NULL: evidence of difference of traffic between anoon and midday

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#STRONG evidence morning MORE traffic in the afternoon than in midday in zone 2




#ANOON vs MORNING

#compute differences

D<-week.traffic.time.slots2$fr_anoon - week.traffic.time.slots2$fr_morning
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) # ok

graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#evidence to reject equality

#REJECT the NULL: evidence of difference of traffic between mornig and anoon

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#STRONG evidence morning MORE traffic in the afternoon than in the morning zone 2


################################################################################################################################

# Same tests for zone 4

head(week.traffic.time.slots4)

####################################################################################################################################

#MATTINA vs MIDDAY

#compute differences

D<-week.traffic.time.slots4$fr_morning - week.traffic.time.slots4$fr_midday
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D> (-0.06))]
n<-length(D)
n #remove 2

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#REJECT the NULL: evidence of difference of traffic between mornig and midday

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#---->STRONG evidence morning MORE traffic than midday zone 4 



#ANOON vs MIDDAY

#compute differences

D<-week.traffic.time.slots4$fr_anoon - week.traffic.time.slots4$fr_midday
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) # ok


graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#evidence to reject equality

#REJECT the NULL: evidence of difference of traffic between anoon and midday

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#STRONG evidence morning MORE traffic in the afternoon than in midday in zone 4




#ANOON vs MORNING

#compute differences

D<-week.traffic.time.slots4$fr_anoon - week.traffic.time.slots4$fr_morning
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) # poor

graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#evidence to reject equality

#REJECT the NULL: evidence of difference of traffic between mornig and anoon

t.test(D, mu = delta.0, alternative = 'less', conf.level = 1-alpha)

#STRONG evidence morning MORE traffic in the morning than in the afternoon zone 4 


################################################################################################################################

# Same tests for zone 5

head(week.traffic.time.slots5)

#############################################################################################################
#MATTINA vs MIDDAY

#compute differences

D<-week.traffic.time.slots5$fr_morning - week.traffic.time.slots5$fr_midday
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

#coda sinistra molto pesante

shapiro.test(D) #strong evidence to reject

#rimuovo outliers
D<-D[which(D> (-0.12))]
n<-length(D)
n #remove 2

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#REJECT the NULL: evidence of difference of traffic between mornig and midday

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#---->STRONG evidence morning MORE traffic than midday



#ANOON vs MIDDAY

#compute differences

D<-week.traffic.time.slots5$fr_anoon - week.traffic.time.slots5$fr_midday
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) # ok


graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

# reject equality only at 10% , acept equality at 1 %, 5 %
# NOT string evidence of difference of traffic between anoon and midday

t.test(D, mu = delta.0, alternative = 'greater', conf.level = 1-alpha)

#p-value = 0.03211 , not string evidence of more traffic in the afternoon then midday zone 5



#ANOON vs MORNING

#compute differences

D<-week.traffic.time.slots5$fr_anoon - week.traffic.time.slots5$fr_morning
n<-length(D)
n
p<-1

#check gaussianity on differences
x11()
par(mfrow= c(1,3))
plot(D)
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)



shapiro.test(D) # Ok, accettabile, miglioro 

#rimuovo outliers
D<-D[which(D > (-0.15) & D < (0.14))]
n<-length(D)
n #remove 2

x11()
par(mfrow= c(1,3))
plot(D, main="without outliers")
hist(D, col='grey', prob=T,breaks = 50)
lines(seq(min(D), max(D), length=2000), 
      dnorm(seq(min(D), max(D), length=2000),
            mean(D),sd(D)), col='blue', lty=1)

qqnorm(D)
qqline(D)

shapiro.test(D) #OK GAUSSIANITà

graphics.off()

#PAIRED univariate --> t-test 
D.mean   <- mean(D)
D.var    <- var(D)
D.invcov<- 1/D.var

alpha   <- .05
delta.0 <- 0


# automatically do the t-test with R command
t.test(D, mu = delta.0, alternative = 'two.sided', conf.level = 1-alpha)$p.value

#evidence to reject equality

#REJECT the NULL: evidence of difference of traffic between anoon and morning

t.test(D, mu = delta.0, alternative = 'less', conf.level = 1-alpha)

#STRONG evidence morning MORE traffic in the morning than in the afternoon zone 5 

















