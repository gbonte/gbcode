## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


# script s_f.R
N1<-10
N2<-20
x<-seq(-.1,5,by=.1)
plot(x,df(x,N1,N2),main=paste("F (N1=" ,N1,",N2=",N2,") density"),type="l")
plot(x,pf(x,N1,N2),main=paste("F (N1=" ,N1,",N2=",N2,") cumulative distribution"),
     type="l")


