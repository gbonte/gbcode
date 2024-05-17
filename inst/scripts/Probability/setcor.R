
## Create two random variables x and y  such that their correlation is -1 <= rho <= 1
## Let y=ax+w where x is standard E[x]=0, Var[x]=1 and w is indepedent of x
## Since COV([x,y])=E[ax^2+ xw]=aE[x^2]=a and Var[y]=a^2+ Var[w]
## rho= a / Sd[y]=a/sqrt(a^2+Var[w]) -> a^2=rho^2*Var[w]/(1-rho^2)

rho=0.45
N=15000
sdw=0.5
a=sign(rho)*sqrt((rho^2*sdw^2)/(1-rho^2))
x=rnorm(N)
y=a*x+rnorm(N,0,sdw)

print(cor(x,y))

plot(x,y,main=paste("cor(x,y)=",rho),ylim=c(-1,1))


sdw=0.05
a=sign(rho)*sqrt((rho^2*sdw^2)/(1-rho^2))
x=rnorm(N)
y=a*x+rnorm(N,0,sdw)

print(cor(x,y))
points(x,y,col="red",ylim=c(-1,1))

##############@
sdw=0.2
a=sign(rho)*sqrt((rho^2*sdw^2)/(1-rho^2))
x=rnorm(N)
y=a*x+rnorm(N,0,sdw)

print(cor(x,y))
points(x,y,col="green",ylim=c(-1,1))