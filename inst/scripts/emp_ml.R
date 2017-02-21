# TP Modeles stochastiques II
# s_hist.R
# Script: shows the use of maximum likelihood for parameter estimation



eml <- function(m,D,var) {   ## empirical likelihood function (1 argument)
  
  N<- length(D)
  Lik<-1
  for (i in 1:N)
  {
    Lik<-Lik*dnorm(D[i],m,sqrt(var))
    
  }
  -log(Lik)
}

eml2 <- function(m,D) {   ## empirical likelihood function (2 arguments)
  N<- length(D)
  Lik<-1
  for (i in 1:N)
    Lik<-Lik*dnorm(D[i],m[1],sqrt(max(0,m[2])))
  -log(Lik)
}

N <-10

DN<-rnorm(N) # data generation

xmin<-optimize( eml,c(-10,10),D=DN,var=1,lower=-1,upper=1)
# maximization of log likelihood function (1 argument)
xmin

xmin2<-optim( c(-10,10),eml2, D=DN)
# maximization of log likelihood function (2 arguments)

mean(DN)
var(DN)

