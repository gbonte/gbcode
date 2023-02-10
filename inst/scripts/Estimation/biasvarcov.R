rm(list=ls())
R=100
Ntr=1
Nts=100
sdw=0.1
E=NULL
CC=NULL
V=NULL
B2=NULL
N=NULL

for (r in 1:(R/2)){ ## over functions
  theta=runif(1,1,1)
  that=NULL
  for (rr in 1:(2*R)){## over datasets
    DNtr=rnorm(Ntr,theta,sdw)
    DNts=rnorm(Nts,theta,sdw)
    muhat=mean(DNtr)
    that=c(that,muhat)
    E=c(E,mean((DNts-muhat)^2))
    CC=c(CC,(DNts-theta)*(theta-muhat))
  }
  V=c(V,(that-mean(that))^2)
  B2=c(B2,(theta-mean(that))^2)
  N=c(N,(DNts-theta)^2)
  
}
cat("MSEth=", sdw^2+sdw^2/Ntr,"Vth=",sdw^2/Ntr,"\n")
cat("MSE=",mean(E), "Noise=", mean(N), "B2=",mean(B2), "V=",mean(V), "CC=", mean(CC),
    ":",mean(N)+mean(B2)+mean(V), ":", mean(N)+mean(B2)+mean(V)+2*mean(CC)," \n")
