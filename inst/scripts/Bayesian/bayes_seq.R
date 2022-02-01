## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


sigma<-4
theta0<-13
sigma0<-1
Nmax<-100




Nmax<-20
DN<-rnorm(Nmax)

mu.hat<-mean(DN[1:Nmax])
sigmaB<-sqrt(((sigma^2)/Nmax)*((sigma0^2)/(sigma0^2+((sigma^2)/Nmax))))
thetaB<-(sigmaB^2)*(theta0/(sigma0^2)+Nmax*mu.hat/(sigma^2))


for (N in 1:Nmax){
  mu.hat<-DN[N]
  sigmaN<-sqrt(((sigma^2))*((sigma0^2)/(sigma0^2+((sigma^2)))))
  theta0<-(sigmaN^2)*(theta0/(sigma0^2)+mu.hat/(sigma^2))
  sigma0<-sigmaN
}

