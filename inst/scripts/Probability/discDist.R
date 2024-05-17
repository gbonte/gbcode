rm(list=ls())
N=50000
sdw=0.35
X=runif(N,-1,1)
Y=NULL
for (i in 1:N)
  Y=c(Y,runif(1,-sqrt(1-X[i]^2), sqrt(1-X[i]^2)))

plot(X,Y,xlim=c(-2,2),ylim=c(-2,2),cex=.1,
     main="Dep. vs ind. bivariate distributions")

points(rnorm(N,sd=sdw),rnorm(N,sd=sdw), 
       col="red",cex=.1)