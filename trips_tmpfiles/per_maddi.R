tutti_gaussiani = tan(4*tutti)
tutti_gaussiani = atan(tutti_gaussiani)/pi + 1/2
tutti_gaussiani = log( tutti_gaussiani / (1 - tutti_gaussiani)) / 1.654
#tutti_gaussiani = qnorm(tutti_gaussiani)
#tutti_gaussiani = 1/sqrt(2*pi) * exp(1-1/(pi*tutti_gaussiani))
mu_tot = mean(tutti_gaussiani)
sd_tot = sd(tutti_gaussiani)
tutti_gaussiani_hist = tutti_gaussiani[tutti_gaussiani>-10 & tutti_gaussiani<10]
h <- hist(tutti_gaussiani_hist, breaks=200, density = 10,
          col = "lightgray", xlab = "Accuracy", main = "Overall")

xfit <- seq(min(tutti_gaussiani_hist), max(tutti_gaussiani_hist), length = 400) 

yfit <- dt(xfit, df=1) 
yfit <- dnorm(xfit) 
yfit <- yfit * diff(h$mids[1:2]) * length(tutti_gaussiani_hist)

lines(xfit, yfit, col = "black", lwd = 2)