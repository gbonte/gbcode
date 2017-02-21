## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

# script stu.R
N<-10
x<-seq(-5,5,by=.1)
plot(x,dt(x,N),main=paste("Student (N=" ,N,") density"))
plot(x,pt(x,N),main=paste("Student (N=" ,N,") cumulative distribution"))


