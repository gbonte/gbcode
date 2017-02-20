# TP Modeles stochastiques II
# indep.R
# Script: shows the realization of independent and dependent random
# variables


IndepDemo<-function(){

N<-1000
Dx<-rnorm(N,sd=4)
Dy<-rnorm(N)
indep<-1 # put it to 1 to see a dependency effect
plot(xlim=c(-10,10),ylim=c(-2,2),Dx,indep*Dx+Dy)
}
