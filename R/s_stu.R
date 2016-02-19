# TP Modeles stochastiques II
# s_stu.R
# Script: plot the Student density and cumulative distribution

s_stu<-function(){
  N<-10
  x<-seq(-5,5,by=.1)
  plot(x,dt(x,N),main=paste("Student (N=" ,N,") density"))


  plot(x,pt(x,N),main=paste("Student (N=" ,N,") cumulative distribution"))


}
