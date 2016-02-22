#### plotF ####
#' Plot F distribution
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Plot the F density and cumulative distribution

plotF<-function(N=10, N2=20){

x<-seq(-.1,5,by=.1)
par(ask=TRUE)
plot(x,df(x,N1,N2),main=paste("F (N1=" ,N1,",N2=",N2,") density"),type="l")

plot(x,pf(x,N1,N2),main=paste("F (N1=" ,N1,",N2=",N2,") cumulative distribution"),
     type="l")

}
