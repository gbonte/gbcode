# Statistical foundations of machine learning
# cheby.R
# Script: shows the Chebyshev relation by means of simulation

cheby<-function(){

  rm(list=ls())
  P.hat<-NULL
  Bound<-NULL
  mu<-0
  N<-100
  for (sig in seq(.1,1,by=.1)){
    for ( d in seq (.1,1,by=.1)){
      z<-rnorm(N,mean=mu,sd=sig); # random sampling of the r.v. z
      P.hat<- cbind(P.hat,sum(abs(z-mu)>=d)/N) # frequency of the event: z-mu>=d
      Bound<-cbind(Bound,min(1,(sig^2)/(d^2))) # upper bound of the Chebyshev relation
    }
  }

  if (any(P.hat>Bound)){  #check if the relation is satisfied
    print("Chebyshev relation NOT satisfied")
  } else  {
    print("Chebyshev relation IS satisfied")
  }
}
