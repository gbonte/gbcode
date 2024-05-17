## Monte Carlo script to show that
## p (x2)= E_x1(p(x_2|x_1))

## x1 ~ N(mu1,var1)
## x2=a*x1+b+w. where w is gaussian and E[w]=0 Var[w]=varw
## p(x2|x1)=N(a*x1+b,varw)
## E[x2]=a*E[x1]+b
## Var[x2]=a^2*Var[x1]+Var[w]

a=-0.31
b=1.5
R= 500 ## number of MC trials
mux1=1
X2=seq(-5,5,by=0.01)
var1=2.1
varw=0.5

Px2=NULL
for (r in 1:R){
  x1=rnorm(1,mux1,sd=sqrt(var1))
  Px2=cbind(Px2,dnorm(X2,a*x1+b,sd=sqrt(varw))) ## conditional density
}

px2=apply(Px2,1,mean) ## average of conditional density
plot(X2,px2,type="l",lwd=2) ## MC computation of marginale
lines(X2,dnorm(X2,a*mux1+b,sqrt(a^2*var1+varw)),col="red",lwd=2)