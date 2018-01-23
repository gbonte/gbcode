del=1 # sampling interval
p=13
f=1/p
T=seq(0,100,by=del)
TS=sin(2*pi*f*T)
plot(T,TS,type="l")
x=TS


x.spec <- spectrum(x,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec

1/spx[which.max(spy)] ## period estimation
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")