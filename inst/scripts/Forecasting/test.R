genfreq<-function(N,m=1,FF=20,sdw=0.5){
  ## it generates a time series with F frequencies
  ## F: number of frequencies
  ## N: number of observations
  
  omega=runif(FF,1/(8*N),0.4)
  SD=runif(2*FF,1,2)
  TT=1:N
  if (m==1){
    Y=numeric(N)
    nl=sample(1:3,1)
    for (f in 1:FF){
      if (nl==1)
        Y=Y+SD[f]*sin(2*pi*omega[f]*T)+SD[FF+f]*cos(2*pi*omega[f]*T)+rnorm(N,sd=sdw/FF)
      if (nl==2)
        Y=Y+SD[f]*sin(2*pi*omega[f]*T)*SD[FF+f]*cos(2*pi*omega[f]*T)+rnorm(N,sd=sdw/FF)
      if (nl==3)
        Y=Y*SD[f]*sin(2*pi*omega[f]*T)+abs(SD[FF+f]*cos(2*pi*omega[f]*T))+rnorm(N,sd=sdw/FF)
    }
  }
  if (m>1){
    Y=array(0,c(N,m))
    sdf=0.0001
    nl=1
    for (f in 1:FF){
      if (nl==1)
        for (mm in 1:m)
          Y[,mm]=Y[,mm]+runif(1,-1,1)*sin(2*pi*rnorm(N,omega[f],sd=sdf)*TT)+
            runif(1,-1,1)*cos(2*pi*rnorm(N,omega[f],sd=sdf)*TT)+rnorm(N,sd=sdw/FF)
      if (nl==2)
        for (mm in 1:m)
          Y[,mm]=Y[,mm]+runif(1,1,2)*sin(2*pi*rnorm(1,omega[f],sd=sdw)*TT)*runif(1,1,2)*cos(2*pi*rnorm(1,omega[f],sd=sdw)*TT)+rnorm(N,sd=sdw/FF)
      if (nl==3)
        for (mm in 1:m)
          if (runif(1)<0.75)
            Y[,mm]=Y[,mm]*runif(1,1,2)*sin(2*pi*omega[f]*TT)+abs(runif(1,1,2)*cos(2*pi*omega[f]*TT))+rnorm(N,sd=sdw/FF)
      if (any(is.nan(Y)))
        browser()
    }
    
  }
  
  return(Y)
}
D=genfreq(N=400,m=50,F=50,sdw=0.1)
D=scale(D)
Ntr=350
H=10
Dtr=D[1:Ntr,]
Dts=D[(Ntr+1):(Ntr+H),]
n=3
Dhat=MmultiplestepAhead(Dtr,n,H=H,multi="VARs")
Dhat2=MmultiplestepAhead(Dtr,n,H=H,multi="multicca")
print(mean((Dhat-Dts)^2))
print(mean((Dhat2-Dts)^2))
plot(Dts[,1])
lines(Dhat[,1])
TT=1:N
sdw=0.1
plot(cos(2*pi*rnorm(N,0.013,sd=sdw)*TT),type="l")

