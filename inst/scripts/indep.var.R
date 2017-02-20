## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


N<-1000


x<-rnorm(N)

y<-runif(N)
y2<-rnorm(N)

plot(x,y,xlim=c(-4,4),ylim=c(-1,2))

plot(x,y2,xlim=c(-4,4),ylim=c(-4,4))

plot(x,2*x+y2,xlim=c(-4,4),ylim=c(-4,4))


plot(x,2*x^2+y2-1,xlim=c(-4,4),ylim=c(-4,4))


plot(x,sin(2*pi*x)+y2/2,xlim=c(-4,4),ylim=c(-4,4))

