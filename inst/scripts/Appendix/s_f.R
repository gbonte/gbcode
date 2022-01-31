## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

# s_f.R
# Script: plot the F density and cumulative distribution


N1<-10
N2<-20
x<-seq(-.1,5,by=.1)
plot(x,df(x,N1,N2),main=paste("F (N1=" ,N1,",N2=",N2,") density"),type="l")


plot(x,pf(x,N1,N2),main=paste("F (N1=" ,N1,",N2=",N2,") cumulative distribution"),
     type="l")


