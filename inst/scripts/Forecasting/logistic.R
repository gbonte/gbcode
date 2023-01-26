rm(list=ls())
library(gbcode)

TT=500
k=3.78

y=c(0.5)

for (t in 1:TT){
  yt=y[length(y)]
  yt1=k*yt*(1-yt)
  y=c(y,yt1)
    
}
N=length(y)

par(mfrow=c(1,4), mai = 0.1*c(1,1,1,1),
    mar = 2.5*c(1,1,1,1))
plot(y,type="l")
pacf(y)

plot(y[-N],y[-1],xlab="y(t)",ylab="y(t+1)",
     main="conditional distribution")

pacf(rnorm(N))

