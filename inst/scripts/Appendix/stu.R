## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


# script stu.R
N<-10
x<-seq(-5,5,by=.1)
plot(x,dt(x,N),main=paste("Student (N=" ,N,") density"))
plot(x,pt(x,N),main=paste("Student (N=" ,N,") cumulative distribution"))


