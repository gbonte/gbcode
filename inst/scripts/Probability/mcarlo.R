## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



## Monte Carlo estimation of parameters

R=100000
# number of MC trials 

Ez=0.5
Vz=1.7
z=rnorm(R,Ez,sqrt(Vz))
y=z^2

cat(paste("E[z]=",Ez, "; MC est E[z] =",mean(z), "\n"))
# MonteCarlo estimation of E[z]

cat(paste("Var[z]=",Vz,"; MC est Var[z] =",var(z), "\n"))
# MonteCarlo estimation of Var[z]



cat(paste("Var[z^2]=",Vz+Ez^2, "; MC est E[y=z^2] =",
          mean(y), "\n"))
##Var[z]=E[z^2]-E[z]^2 -> E[z^2]=Var[z]+E[z]^2=Vz+Ez^2

#############################################@
Ez=0
Vz=1
z=rnorm(R,Ez,sqrt(Vz))
k=abs(z)


Ek=sqrt(2/pi)
Vk=1-2/pi
## https://www.quora.com/If-Y-X-where-X-has-normal-distribution-N-0-1-what-is-the-density-function-expectation-and-variance-of-Y

cat(paste("E[k]=",Ek, "; MC estimate E[k=|z|] =",mean(k), "\n"))
# MonteCarlo estimation of E[k]

cat(paste("V[k]=",Vk, "; MC estimate V[k=|z|] =",var(k), "\n"))
# MonteCarlo estimation of E[k]
