rm(list=ls())

N=21500
# Numbers from https://hbr.org/2020/12/covid-19-vaccine-trials-are-a-case-study-on-the-challenges-of-data-literacy


# How efficacy rate was computed: first, count the number of people who developed Covid-19 in the vaccinated group. 
# Second, divide that by the number of people who developed it in the placebo group. 
# Third, subtract that quotient from 1, and you’ll get the efficacy rate.

V=numeric(N)
V[1:8]=1

NV=numeric(N)
NV[1:86]=1


B=100000
## number of bootstrap replicates

eff=NULL
for (b in 1:B){
   set.seed(b)
   I=sample(N,rep=TRUE) ## sampling with replacement
   eff=c(eff,sum(V[I])/sum(NV[I]))
   
}

print(quantile(1-eff,c(0.025,0.975)))
hist(100*(1-eff),xlab="% Efficacy",main="")