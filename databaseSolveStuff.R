library("readxl")
library("survival")
dat<- read_excel("C:/Users/chend/OneDrive/Desktop/Jug2.xlsx")

Time<- dat$`Total Time`
mean(Time)
hist(Time, freq=F, xlab="Solve Time (s)", main="database Solve Times")
logT = log(Time)
hist(logT, freq=F, xlab = "Solve Time(s)",main="log T")

Moves<- dat$`Total STM`
mean(Moves)
hist(Moves, freq=F, xlab="Moves", main="database Moves ")