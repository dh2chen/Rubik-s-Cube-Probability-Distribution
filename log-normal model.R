library("readxl")
library("survival")
dat<- read_excel("C:/Users/chend/OneDrive/Desktop/YushengDuWCATimes.xlsx")
yushengDu<- data.frame(dat)
#urve(1/(x*sqrt(2*pi*0.04642))*exp(-(log(x)-2.1515)^2/2*0.04642),0,18)
curve((1/(x*sqrt(2*pi*0.046)))*(exp((-(log(x)-2.15)^2)/(2*0.046))),0,18)

#print(yushengDu)
Solves<-dat$Solves
print(Solves)
mean(Solves)
hist(Solves, freq=F, xlab="Solve Time (s)", main="Yusheng Du Solve Times with Log-Normal Model",ylim=c(0,0.25))
histdat=hist(Solves,plot=F)

breaks1<-histdat$breaks
density<-histdat$density

status=rep(1,length(Solves))
fit = survreg(Surv(Solves,status)~1,dist="lognormal")

sigma = fit$scale
mu = fit$coefficients

x=seq(0,35,0.001)
y=(1/(x*sigma*sqrt(2*pi)))*(exp((-(log(x)-mu)^2)/(2*sigma^2)))
lines(x,y)

#alpha = exp(fit$coefficients)
#beta = 1/fit$scale
#x=seq(0,35,0.1)
#y=((beta/alpha)*(x/alpha)^(beta-1)*exp(-(x/alpha)^beta))
#lines(x,y)
print(breaks1)
print(density*781*2)
print(density)
hist(Solves, freq=F, xlab="Solve Time (s)", main="Yusheng Du Solve Times with Normal Model",ylim=c(0,0.25))
x=seq(0,35,0.001)
y=dnorm(x,mean(Solves),sd(Solves))
lines(x,y)
hist(Solves, freq=F, xlab="Solve Time (s)", main="Yusheng Du Solve Times",ylim=c(0,0.25))

#boxplot(Solves)
#Solves_out_rm <- Solves[!Solves %in% boxplot.stats(Solves)$out]
#hist(Solves_out_rm,freq=F, xlab="shit", main="cum")
#x=seq(0,35,0.001)
#y=(1/(x*sqrt(2*pi*0.0345)))*(exp((-(log(x)-2.15)^2)/(2*0.0345)))
#lines(x,y)
