rm(list=ls())
setwd("/Users/katherinefranz/Desktop/IBC_Exercise_10")
library(ggplot2)
simlength = 500
# time points to simulate order (simulate for 100 days)
t = vector (mode = "numeric", length = simlength)
# values for parameters
rN=0.1
N0=99
M0=1
K = 1000000
# a place to store population sizes (value of state variable through time)
Nt= vector (mode = "numeric", length = simlength)
Mt= vector (mode = "numeric", length = simlength)
# set initial values 
Nt[1]=N0
Mt[1]=M0
t[1]=1
# simulate with for loop
lengthy = simlength-1
for (i in 1:lengthy){
  Nt[i+1]=Nt[i]+rN*Nt[i]*(1-((Nt[i]+Mt[i])/K))
  Mt[i+1]=Mt[i]+rN*Mt[i]*(1-((Nt[i]+Mt[i])/K))
  t[i] = i
}
# graph it! 
simulate= data.frame("time"=t, "Normal"=Nt, "Mutant"= Mt)
head(simulate)
b <- ggplot(data=simulate, aes(x=time,y=Normal)) +
  geom_line()+
  xlab("time")+ylab("# cells")+
  theme_classic()
b
