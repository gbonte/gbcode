## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


#### plotStu ####
#' Plot Student distribution
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Plot the Student density and cumulative distribution
#'
plotStu<-function(N=10){
  x<-seq(-5,5,by=.1)
  par(ask=TRUE)
  plot(x,dt(x,N),main=paste("Student (N=" ,N,") density"),type="l")
  plot(x,pt(x,N),main=paste("Student (N=" ,N,") cumulative distribution"),type="l")


}
