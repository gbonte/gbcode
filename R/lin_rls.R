
#### rls ####
#' Recursive least-squares step
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#'  @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @title Recursive least-squares step with forgetting factor
#'@name rls
#'@param x: new input
#'@param y: new output
#'@param t: current parameter vector
#'@param P: current covariance matrix
#'@return a list with fields:
#'\itemize{
#' \item{\code{t.new}}: updated parameter vector,
#' \item{ \code{P.new}:} updated covariance matrix,
#' \item{ \code{mu}:} forgetting factor, the lower the highr the forgetting (1: no forget)
#'
#'}
#'
#'@examples
#'par(ask=TRUE)
#'n<-1;
#'X<-seq(-pi,pi,by=.02)
#'N<-length(X)
#'y<-sin(X)+0.1*rnorm(N)
#'t<-numeric(2)
#'P<-500*diag(n+1)
#'mu<-0.9 ## forgetting factor
#'for (i in 1:N){
#'  rls.step<-rls(c(1, X[i]),y[i],t,P,mu)
#'  t<-rls.step[[1]]
#'  P<-rls.step[[2]]
#'  plot(X[1:i],y[1:i],xlim=c(-4,4),ylim=c(-2,2),main=paste("Forgetting factor mu=",mu))
#'  lines(X[1:i],cbind(array(1,c(i,1)), X[1:i])%*%t,col="red",)
#'  ## rls fitting
#'}
#'

rls<-function(x,y,t,P,mu=1){
  P.new <-(P-(P%*%x%*%x%*%P)/as.numeric(1+x%*%P%*%x))/mu
  ga <- P.new%*%x
  epsi <- y-x%*%t
  t.new<-t+ga*as.numeric(epsi)
  list(t.new,P.new)
}


