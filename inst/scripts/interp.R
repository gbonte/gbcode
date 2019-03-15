

f<-function(x,ord){
  f<-1
  for (i in 1:ord)
    f<-f+x^i
  f
}


set.seed(1)

n<-1
x<-seq(-2,2,by=0.2)
N<-length(x)
sd.w<-1
O<-3
Y<-f(x,ord=O)+rnorm(N,sd=sd.w)
data.tr<-cbind(Y,x)


R<-12

Remp<-numeric(R)
FPE<-numeric(R)
PSE<-numeric(R)
MSE.loo<-numeric(R)
x.ts<-seq(-2.2,2.2,by=.01)



for (r in 1:R){
  X.ts<-NULL
  X<-NULL
  for (ord in 1:r){
    X<-cbind(X,x^ord)
    X.ts<-cbind(X.ts,x.ts^ord)
  }
  p<-r+1
  DN<-data.frame(cbind(Y,X))
  
  mod<-lm(Y~.,DN)
  sd.w.hat<-sqrt(sum(mod$residuals^2)/(N-p))
  Remp[r]<-sum(mod$residuals^2)
  FPE[r]<-(1+p/N)/(1-p/N)*Remp[r]
  PSE[r]<-Remp[r]+2*p*sd.w.hat^2
  
  e.hat.i<-numeric(N)
  for ( i in 1:N){
    DN.i<-DN[-i,]
    mod.i<-lm(Y~.,DN.i)
    e.hat.i[i]<-Y[i]-predict(mod.i,DN[i,])
  }
  MSE.loo[r]<-mean(e.hat.i^2)
  
  
  Y.ts<-f(x.ts,ord=O)
  data.ts<-data.frame(cbind(Y.ts,X.ts))
  names(data.ts)<-names(DN)
  plot(x.ts,Y.ts,type="l",ylim=c(min(Y),max(Y)))
  points(x,Y)
  lines(x.ts,predict(mod.i,data.ts),col="red")
  title(paste("R.emp=",round(Remp[r],2), " FPE[r]=",round(FPE[r],2)))
  cat(r,"\n")
  par(ask=TRUE)
}

#cat(Remp,"\n")
#cat(FPE,"\n")
cat("Ord=",O,"\n")

plot(Remp)
title("R.emp")
par(ask=TRUE)
cat("which.min.Remp=",which.min(Remp),"\n")

plot(FPE)
title("FPE")
par(ask=TRUE)
cat("which.min.FPE=",which.min(FPE),"\n")

plot(PSE)
title("PSE")
par(ask=TRUE)
cat("which.min.PSE=",which.min(PSE),"\n")

plot(MSE.loo)
par(ask=TRUE)
cat("which.min.MSE.loo=",which.min(MSE.loo),"\n")


##############################
# TEST DATA GENERATION
set.seed(0)
N.ts<-2000
x.ts<-rnorm(N.ts,sd=0.5)
Y.ts<-f(x.ts,ord=O)+rnorm(N,sd=sd.w)
data.ts<-cbind(Y.ts,x.ts)

write(t(data.ts),file="c:/bontempi/prof/mod_Stoch/projet/DTS.txt",ncolumns=2)

##############################


err.ts<-numeric(R)
for ( k in 1:R){
  X<-NULL
  X.ts<-NULL
  for (ord in 1:k){
    X<-cbind(X,x^ord)
    X.ts<-cbind(X.ts,x.ts^ord)
  }
  DN<-data.frame(cbind(Y,X))
  DN.ts<-data.frame(cbind(Y.ts,X.ts))
  names(DN.ts)<-names(DN)
  mod.k<-lm(Y~.,DN)
  err.ts[k]<-mean((Y.ts-predict(mod.k,DN.ts))^2)
}
plot(err.ts,type="l")
cat("err.test.FPE=",err.ts[which.min(FPE)],"\n")



cat("err.test.Remp=",err.ts[which.min(Remp)],"\n")



cat("err.test.loo=",err.ts[which.min(MSE.loo)],"\n")

