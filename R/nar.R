genlog<-function(N,k=3.8, y0=0.1){
  ## it generates a chaotic logistic discrete time time series
  if (k<3.57){
    stop("No chaotic behavior is generated for such k")
  }
  if (y0<0 | y0>1)
    stop("Initial condition should belong to [0,1]")
  y=c(y0)
  
  for (t in 1:N){
    yt=y[length(y)]
    yt1=k*yt*(1-yt)
    y=c(y,yt1)
    
  }
  return(y[1:N])
}

genfreq<-function(N,m=1,F=20,sdw=0.5){
  ## it generates a time series with F frequencies
  ## F: number of frequencies
  ## N: number of observations
  
  omega=runif(F,1/N,(N-1)/(N))
  SD=runif(2*F,1,2)
  T=1:N
  if (m==1){
    Y=numeric(N)
    nl=sample(1:3,1)
    for (f in 1:F){
      if (nl==1)
        Y=Y+SD[f]*sin(2*pi*rnorm(N,omega[f],sd=sdf)*T)+SD[F+f]*cos(2*pi*rnorm(N,omega[f],sd=sdf)*T)+rnorm(N,sd=sdw/F)
      if (nl==2)
        Y=Y+SD[f]*sin(2*pi*omega[f]*T)*SD[F+f]*cos(2*pi*omega[f]*T)+rnorm(N,sd=sdw/F)
      if (nl==3)
        Y=Y*SD[f]*sin(2*pi*omega[f]*T)+abs(SD[F+f]*cos(2*pi*omega[f]*T))+rnorm(N,sd=sdw/F)
    }
  }
  if (m>1){
    Y=array(0,c(N,m))
    nl=1
    sdf=0.0001
    for (f in 1:F){
      if (nl==1)
        for (mm in 1:m)
          if (runif(1)<0.5)
            Y[,mm]=Y[,mm]+runif(1,-1,1)*sin(2*pi*rnorm(N,omega[f],sd=sdf)*T)+
              runif(1,-1,1)*cos(2*pi*rnorm(N,omega[f],sd=sdf)*T)+rnorm(N,sd=sdw/F)
      if (nl==2)
        for (mm in 1:m)
          if (runif(1)<0.5)
            Y[,mm]=Y[,mm]+runif(1,1,2)*sin(2*pi*rnorm(N,omega[f],sd=sdf)*T)*runif(1,1,2)*cos(2*pi*omega[f]*T)+rnorm(N,sd=sdw/F)
      if (nl==3)
        for (mm in 1:m)
          if (runif(1)<0.5)
            Y[,mm]=Y[,mm]*runif(1,1,2)*sin(2*pi*rnorm(N,omega[f],sd=sdf)*T)+abs(runif(1,1,2)*cos(2*pi*omega[f]*T))+rnorm(N,sd=sdw/F)
    }
  }
  
  return(Y)
}

nar<-function(Y,number=1){
  
  N<-length(Y)
  number=round(number)
  if (number < 1 | number >13)
    stop("Bad number parameter in nar function: only 13 NAR models available")
  if (number==1)
    return(list(y=-0.4*(3-Y[N]^2)/(1+Y[N]^2)+0.6*(3-(Y[N-1]-0.5)^3)/(1+(Y[N-1]-0.5)^4),p=2))
  
  if (number==2)
    return( list(y=(0.4-2*exp(-50*Y[N-5]^2))*Y[N-5]+(0.5-0.5*exp(-50*Y[N-9]^2))*Y[N-9],p=10))
  if (number==3)
    return( list(y=(0.4-2*cos(40*Y[N-5])*exp(-30*Y[N-5]^2))*Y[N-5]+(0.5-0.5*exp(-50*Y[N-9]^2))*Y[N-9],p=10))
  
  if (number==4)
    return(list(y=2*exp(-0.1*Y[N]^2)*Y[N]-exp(-0.1*Y[N-1]^2)*Y[N-1],p=2))
  
  
  if (number==5)
    return(list(y=-2*Y[N]*max(0,sign(-Y[N]))+0.4*Y[N]*max(0,sign(Y[N])),p=1))
  
  if (number==6)
    return(list(y=0.8*log(1+3*Y[N]^2)-0.6*log(1+3*Y[N-2]^2),p=3))
  
  if (number==7)
    return(list(y=1.5 *sin(pi/2*Y[N-1])-sin(pi/2*Y[N-2]),p=3))
  
  if (number==8)
    return(list(y=(0.5-1.1*exp(-50*Y[N]^2))*Y[N]+(0.3-0.5*exp(-50*Y[N-2]^2))*Y[N-2],p=3))
  
  if (number==9)
    return(list(y=0.3*Y[N]+0.6*Y[N-1]+(0.1-0.9*Y[N]+0.8*Y[N-1])/(1+exp(-10*Y[N])),p=2))
  
  
  if (number==10)
    return(list(y=sign(Y[N]),p=2))  ## Sign autoregressive Zhang
  if (number==11)
    return(list(y=0.8*Y[N]-0.8*Y[N]/(1+exp(-10*Y[N])),p=2)) ## STAR1 Zhang
  if (number==12)
    return(list(y=0.3*Y[N]+0.6*Y[N-1]+(0.1-0.9*Y[N]+0.8*Y[N-1])/(1+exp(-10*Y[N])),p=2)) ## STAR2 Zhang
  
  if (number==13)
    return(list(y=0.246*Y[N]*(16-Y[N]),p=1)) ## NOISY LOGISTIC
}



star<-function(Y,number=1,loc=4,linear=TRUE,mix=NULL){
  n<-NCOL(Y)
  N<-NROW(Y)
  y<-numeric(n)
  if (linear){
    if (number==1)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=-0.4*(3-mean(Y[N,neigh])^2)/(1+mean(Y[N,neigh])^2)+0.6*(3-(mean(Y[N-1,neigh])-0.5)^3)/(1+(mean(Y[N-1,neigh])-0.5)^4)
      }
    
    if (number==2)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=(0.4-2*exp(-50*mean(Y[N-5,neigh])^2))*mean(Y[N-5,neigh])+
          (0.5-0.5*exp(-50*mean(Y[N-9,neigh])^2))*mean(Y[N-9,neigh])
      }
    
    if (number==3)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=(0.4-2*cos(40*mean(Y[N-5,neigh]))*exp(-30*mean(Y[N-5,neigh])^2))*mean(Y[N-5,neigh])+(0.5-0.5*exp(-50*mean(Y[N-9,neigh])^2))*mean(Y[N-9,neigh])
      }
    
    
    if (number==4)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=2*exp(-0.1*mean(Y[N,neigh])^2)*mean(Y[N,neigh])-exp(-0.1*mean(Y[N-1,neigh])^2)*mean(Y[N-1,neigh])
      }
    
    
    if (number==5)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=-2*mean(Y[N,neigh])*max(0,sign(-mean(Y[N,neigh])))+0.4*mean(Y[N,neigh])*max(0,sign(mean(Y[N,neigh])))
      }
    
    
    if (number==6)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=0.8*log(1+3*mean(Y[N,neigh])^2)-0.6*log(1+3*mean(Y[N-2,neigh])^2)
      }
    
    if (number==7)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=1.5 *sin(pi/2*mean(Y[N-1,neigh]))-sin(pi/2*mean(Y[N-2,neigh]))
      }
    
    
    if (number==8)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=(0.5-1.1*exp(-50*mean(Y[N,neigh])^2))*mean(Y[N,neigh])+(0.3-0.5*exp(-50*mean(Y[N-2,neigh])^2))*mean(Y[N-2,neigh])
      }
    
    
    if (number==9)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=0.3*mean(Y[N,neigh])+0.6*mean(Y[N-1,neigh])+(0.1-0.9*mean(Y[N,neigh])+0.8*mean(Y[N-1,neigh]))/(1+exp(-10*mean(Y[N,neigh])))
      }
    
    
    if (number==10)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=sign(mean(Y[N,neigh]))
      }
    
    
    if (number==11)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=0.8*mean(Y[N,neigh])-0.8*mean(Y[N,neigh])/(1+exp(-10*mean(Y[N,neigh])))
      }
    
    
    if (number==12)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=0.3*mean(Y[N,neigh])+0.6*mean(Y[N-1,neigh])+(0.1-0.9*mean(Y[N,neigh])+0.8*mean(Y[N-1,neigh]))/(1+exp(-10*mean(Y[N,neigh])))
        
      }
    
    
    if (number==13)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=0.246*mean(Y[N,neigh])*(1-mean(Y[N,neigh]))
        
      }
    if (number==14)
      for (i in 1:n){
        neigh=mix[max(1,i-loc):min(n,i+loc)]
        
        y[i]=nar(apply(abs(Y[max(1,N-20):N,neigh]),1,mean),number=mix[i])$y
        
      }
    
    
    return(y)
  } else {  ##### NOT LINEAR
    if (number==1)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=-0.4*(3-mean(abs(Y[N,neigh]))^2)/(1+mean(abs(Y[N,neigh]))^2)+0.6*(3-(mean(abs(Y[N-1,neigh]))-0.5)^3)/(1+(mean(abs(Y[N-1,neigh]))-0.5)^4)
      }
    
    if (number==2)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=(0.4-2*exp(-50*mean(abs(Y[N-5,neigh]))^2))*mean(abs(Y[N-5,neigh]))+(0.5-0.5*exp(-50*mean(abs(Y[N-9,neigh]))^2))*mean(abs(Y[N-9,neigh]))
      }
    
    if (number==3)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=(0.4-2*cos(40*mean(abs(Y[N-5,neigh])))*exp(-30*mean(abs(Y[N-5,neigh]))^2))*mean(abs(Y[N-5,neigh]))+(0.5-0.5*exp(-50*mean(abs(Y[N-9,neigh]))^2))*mean(abs(Y[N-9,neigh]))
      }
    
    
    if (number==4)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=2*exp(-0.1*mean(abs(Y[N,neigh]))^2)*mean(abs(Y[N,neigh]))-exp(-0.1*mean(abs(Y[N-1,neigh]))^2)*mean(abs(Y[N-1,neigh]))
      }
    
    
    if (number==5)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=-2*mean(abs(Y[N,neigh]))*max(0,sign(-mean(abs(Y[N,neigh]))))+0.4*mean(abs(Y[N,neigh]))*max(0,sign(mean(abs(Y[N,neigh]))))
      }
    
    
    if (number==6)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=0.8*log(1+3*mean(abs(Y[N,neigh]))^2)-0.6*log(1+3*mean(abs(Y[N-2,neigh]))^2)
      }
    
    if (number==7)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=1.5 *sin(pi/2*mean(abs(Y[N-1,neigh])))-sin(pi/2*mean(abs(Y[N-2,neigh])))
      }
    
    
    if (number==8)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=(0.5-1.1*exp(-50*mean(abs(Y[N,neigh]))^2))*mean(abs(Y[N,neigh]))+(0.3-0.5*exp(-50*mean(abs(Y[N-2,neigh]))^2))*mean(abs(Y[N-2,neigh]))
      }
    
    
    if (number==9)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=0.3*mean(abs(Y[N,neigh]))+0.6*mean(abs(Y[N-1,neigh]))+(0.1-0.9*mean(abs(Y[N,neigh]))+0.8*mean(abs(Y[N-1,neigh])))/(1+exp(-10*mean(abs(Y[N,neigh]))))
      }
    
    
    if (number==10)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=sign(mean(abs(Y[N,neigh])))
      }
    
    
    if (number==11)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=0.8*mean(abs(Y[N,neigh]))-0.8*mean(abs(Y[N,neigh]))/(1+exp(-10*mean(abs(Y[N,neigh]))))
      }
    
    
    if (number==12)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=0.3*mean(abs(Y[N,neigh]))+0.6*mean(abs(Y[N-1,neigh]))+(0.1-0.9*mean(abs(Y[N,neigh]))+0.8*mean(abs(Y[N-1,neigh])))/(1+exp(-10*mean(abs(Y[N,neigh]))))
      }
    
    
    if (number==13)
      for (i in 1:n){
        neigh=max(1,i-loc):min(n,i+loc)
        y[i]=0.246*mean(abs(Y[N,neigh]))*(16-mean(abs(Y[N,neigh])))
      }
    
    
    
    return(y)
    
  }
}

#### genar ####
#' Generte a univariate time series using a NAR mode
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{https://tinyurl.com/sfmlh}
#' @title NAR univariate time series generation
#' @param N: number of observations
#' @param s: standard deviation of additive noise
#' @param number : integer number (between 1 and 13) denoting the NAR generating model
#'
#' @export
#' @return vector containing time series observations
#' @examples
#' plot(genar(100),type="l")
#'
#'
genar<-function(N,s=0.1,number=1){
  Y=Inf
  while(max(abs(Y))>10){
    Y<-rnorm(10,sd=s)
    for (i in length(Y):(2*N)){
      NAR<-nar(Y,number)
      nY<-NAR$y
      Y<-c(Y,nY+s*rnorm(1))
    }
    s=s/10
  }
  return(Y[(length(Y)-N+1):length(Y)])
}



genstar<-function(N,n,s=0.1,number=1,loc=4,linear=TRUE,mix=mix){
  Y<-array(rnorm(10*n),c(10,n))
  for (i in NROW(Y):(2*N)){
    nY<-star(Y,number=number,loc=loc,linear=linear,mix=mix)
    
    
    Y<-rbind(Y,nY+rnorm(n,sd=s))
    
  }
  return(Y[(NROW(Y)-N+1):NROW(Y),])
}
