library("readxl")
library("survival")
dat<- read_excel("C:/Users/chend/OneDrive/Desktop/GIIKER_CUBE_DATA.xlsx")

Time<-dat$Time
mean=mean(Time)
var=var(Time)
Time3<-dat$Time - 6

Moves<-dat$Moves
TPS<-dat$TPS


hist(Time, freq=F, xlab="Solve Time (s)", main="My Solve Times with Normal Model ",ylim=c(0,0.2))
histdat2=hist(Time,plot=F)
dens<-histdat2$density
breaks<-histdat2$breaks
x=seq(7,25,0.1)
y=dnorm(x,mean(Time),sd(Time))
lines(x,y)
logT = log(Time)
hist(logT,freq=F,xlab="Solve Time", main="Log t")
x=seq(2,3.5,0.001)
y=dnorm(x,mean(logT),sd(logT))
lines(x,y)
status=rep(1,length(Time))
fit = survreg(Surv(Time,status)~1,dist="lognormal")
mu = fit$coefficients
sigma = (fit$scale)^2
hist(Time, freq=F, xlab="Solve Time (s)", main="My Solve Times with Log-Normal Model",ylim=c(0,0.23))
x=seq(7,25,0.1)
y=(1/(x*sqrt(2*pi*sigma)))*(exp((-(log(x)-mu)^2)/(2*sigma)))
lines(x,y)
print(breaks)
print(dens)
print(dens*404)

hist(Time, freq=F, xlab="Solve Time (s)", main="My Solve Times with Weibull Model",ylim=c(0,0.23))

fit2 = survreg(Surv(Time3,status)~1,dist="weibull")
alpha = exp(fit2$coefficients)
beta = 1/fit2$scale
x=seq(0,35,0.1)
y=((beta/alpha)*((x-6)/alpha)^(beta-1)*exp(-((x-6)/alpha)^beta))
lines(x,y)

