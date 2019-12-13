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
  # equilibrium is reached a bit before t =200, so introduce drug there
    if (i < 200){
      Nt[i+1]=Nt[i]+rN*Nt[i]*(1-((Nt[i]+Mt[i])/K))
      Mt[i+1]=Mt[i]+rN*Mt[i]*(1-((Nt[i]+Mt[i])/K))
    } 
    else {
      Nt[i+1]=Nt[i]+-1*rN*Nt[i]*(1-((Nt[i]+Mt[i])/K))
      Mt[i+1]=Mt[i]+0.5*rN*Mt[i]*(1-((Nt[i]+Mt[i])/K))
    }

  t[i] = i
}

# make dataframe
simulate= data.frame("time"=t, "Normal"=Nt, "Mutant"= Mt)
head(simulate)
simulate_long <- melt(simulate, id="time")  # convert to long format

#plot simulation with ggplot
b <- ggplot(data=simulate_long, aes(x=time,y=value, colour = variable)) +
  geom_line()+
  xlab("time")+ylab("# cells")+
  theme_classic()
b

