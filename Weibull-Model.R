library("readxl")
library("survival")
dat<- read_excel("C:/Users/chend/OneDrive/Desktop/YushengDuWCATimes.xlsx")


Solves<-dat$Solves 
Solves2<-dat$Solves -3

hist(Solves, freq=F, xlab="Solve Time (s)", main="Yusheng Du Solve Times with Weibull Model",ylim=c(0,0.25))
histdat=hist(Solves,plot=F)

status=rep(1,length(Solves2))
fit1 = survreg(Surv(Solves2,status)~1,dist = "weibull")        


alpha = exp(fit1$coefficients)                                 
beta = 1/fit1$scale                                         
x=seq(0,35,0.1)                                                  

y=((beta/alpha)*((x-3)/alpha)^(beta-1)*exp(-((x-3)/alpha)^beta)) 
lines(x,y)
