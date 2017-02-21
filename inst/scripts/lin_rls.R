rm(list=ls())
par(ask=TRUE)

n<-1;
 rls<-function(x,y,t,P,mu=1){

P.new <-(P-(P%*%x%*%x%*%P)/as.numeric(1+x%*%P%*%x))/mu
ga <- P.new%*%x
epsi <- y-x%*%t
t.new<-t+ga*as.numeric(epsi)
list(t.new,P.new)
}

X<-seq(-pi,pi,by=.02)
N<-length(X)

y<-sin(X)+0.1*rnorm(N)


t<-numeric(2)
P<-500*diag(n+1)
mu<-0.9
for (i in 1:N){
    rls.step<-rls(c(1, X[i]),y[i],t,P,mu)
    t<-rls.step[[1]]
    P<-rls.step[[2]]
    plot(X[1:i],y[1:i],
         xlim=c(-4,4),
         ylim=c(-2,2),
         main=paste("Forgetting factor mu<-",mu))

    lines(X[1:i],cbind(array(1,c(i,1)), X[1:i])%*%t,
         col="red",
         )

 
}
