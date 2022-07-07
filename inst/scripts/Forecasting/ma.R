## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi




q=10  ## order of MA(q)
N=q*200
D=rnorm(N);

beta=abs(0.75*rnorm(q))
beta=c(1,beta);
Y=NULL
for (i in (q+1):N){
  Y=c(Y,sum(beta*rev(D[(i-q):i])))
}


N=length(Y)
Co_emp=numeric(2*q)
Co_th=numeric(2*q)

for (k in 1:(2*q)){
  Yk=c(numeric(k), Y)
  
  C=cor(Y[(k+1):N],Yk[(k+1):N])
  Co_emp[k]=C
  
  Co_th[k]=0;
  if (k<=q){
    for (j in 1:(q+1-k)){
      Co_th[k]=Co_th[k]+beta[j]*beta[j+k];
    }
    Co_th[k]=Co_th[k]/sum(beta^2)
  }
}

par(mfrow=c(3,1), mai = 0.3*c(1,1,1,1),
    mar = 2*c(1,1,1,1))
plot(Y,xlab='',main=paste("MA(",q,")"))
#par(mar=c(2,1,2,2))
plot(abs(Co_emp),type="l",lty=2,ylab='',xlab='k')
lines(abs(Co_th),lty=1)
#par(mar=c(2,1,2,2))
legend("topright",c('Estimated cor', 'Cor'),cex=0.6,
       lty=c(2,1))
acf(Y)





