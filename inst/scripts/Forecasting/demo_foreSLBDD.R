rm(list=ls())
library("SLBDD")
library("VARshrink")
require(gbcode)
require(MTS)
library(keras)

#FREDMDApril19     Federal Reserve Bank at St Louis.
#CPIEurope200015   m=33 monthly Price Indexes EUUS
# Stockindexes99world World Stock Indexes: daily stock indices of m=99 financial markets
#UMEdata20002018   Quarterly Economic Series of the European Monetary Union (m=57)
# clothing          Cloth sales in China m=25 for 1805 days
#TaiwanAirBox032017 Hourly PM25 Measurements from Air-Box Devices in Taiwan (m=516)
#PElectricity1344 Electricity Prices in New England and USA- weekly (m=1344)
D = get("PElectricity1344", asNamespace('SLBDD'))
print(dim(D))
visualize=TRUE
season=TRUE
execute=TRUE
methods=c("uni","VAR","VARs","dfml","dfm","multifs")
colors=c("red","green","magenta","cyan","orange","blue")

if (execute){
  n=5
  Nmax<-1500
  mmax<-10
  
  if (NROW(D)>Nmax)
    D=D[1:Nmax,]
  if (NCOL(D)>mmax)
    D=D[,sample(1:NCOL(D),mmax)]
  D=scale(D)
  N=NROW(D)
  m=NCOL(D)
  H=min(10,round(N/5))
  
  E.hat1=NULL
  E.hat2=NULL
  E.hat3=NULL
  E.hat4=NULL
  E.hat5=NULL
  E.hat6=NULL
  
  for ( Ntr in round(seq(N-3*H,N-H,length.out=10))){
    SeasP<-NULL
    
    if (season){
      X=NULL
      bestS<-NULL
      for (i in 1:m){
        S=detectSeason(D[1:Ntr,i],Ls=N,pmin=0.01)
        X=cbind(X,D[,i]-S$spattern-S$strend)
        SeasP<-cbind(SeasP,S$spattern+S$strend)
        bestS<-c(bestS,S$best)
      }
    } else {
      X=D
    }
    print(which(!is.na(bestS)))
    
    
    Xtr=X[1:Ntr,]
    Xts=D[(Ntr+1):(Ntr+H),]
    if (season){
      SPts=SeasP[(Ntr+1):(Ntr+H),]
    } 
    
    Xhat1=MmultiplestepAhead(Xtr,n,H,multi=methods[1],uni="stat_comb")
    cat(".")
    Xhat2=MmultiplestepAhead(Xtr,n,H,multi=methods[2])
    cat(".")
    Xhat3=MmultiplestepAhead(Xtr,n,H,multi=methods[3])
    cat(".")
    Xhat4=MmultiplestepAhead(Xtr,n,H,multi=methods[4],
                             dfmlmodels=c("stat_comb","lazydirect"),cdfml=2,Lcv=5)
    cat(".")
    Xhat5=MmultiplestepAhead(Xtr,n,H,multi=methods[5], dfmlmodels=c("multifs"))
    cat(".")
    Xhat6=MmultiplestepAhead(Xtr,n,H,multi=methods[6],uni="lazydirect")
    cat(".")
    
    
    
    if (season){
      Xhat1=Xhat1+SPts
      Xhat2=Xhat2+SPts
      Xhat3=Xhat3+SPts
      Xhat4=Xhat4+SPts
      Xhat5=Xhat5+SPts
      Xhat6=Xhat6+SPts
    }
    m=NCOL(X)
    e.hat1=apply((Xts-Xhat1)^2,2,mean)
    e.hat2=apply((Xts-Xhat2)^2,2,mean)
    e.hat3=apply((Xts-Xhat3)^2,2,mean)
    e.hat4=apply((Xts-Xhat4)^2,2,mean)
    e.hat5=apply((Xts-Xhat5)^2,2,mean)
    e.hat6=apply((Xts-Xhat6)^2,2,mean)
    
    E.hat1=c(E.hat1,mean(e.hat1))
    E.hat2=c(E.hat2,mean(e.hat2))
    E.hat3=c(E.hat3,mean(e.hat3))
    E.hat4=c(E.hat4,mean(e.hat4))
    E.hat5=c(E.hat1,mean(e.hat5))
    E.hat6=c(E.hat1,mean(e.hat6))
    
    
    cat( methods[1],":",mean(E.hat1)," | ")
    cat( methods[2],":",mean(E.hat2)," | ")
    cat( methods[3],":",mean(E.hat3)," | ")
    cat( methods[4],":",mean(E.hat4)," | ")
    cat( methods[5],":",mean(E.hat5)," |")
    cat( methods[6],":",mean(E.hat6),"\n")
  }
}
if (visualize){
  for (ns in 1:m){
    e.hat1=mean((Xts[,ns]-Xhat1[,ns])^2)
    e.hat2=mean((Xts[,ns]-Xhat2[,ns])^2)
    e.hat3=mean((Xts[,ns]-Xhat3[,ns])^2)
    e.hat4=mean((Xts[,ns]-Xhat4[,ns])^2)
    e.hat5=mean((Xts[,ns]-Xhat5[,ns])^2)
    e.hat6=mean((Xts[,ns]-Xhat6[,ns])^2)
    plot(Xts[,ns],type="l",col="black",lwd=3,ylab=paste("ts.",ns),
         main=paste( methods[1],":",round(mean(e.hat1),2)," | ",
                     methods[2],":",round(mean(e.hat2),2)," | ",
                     methods[3],":",round(mean(e.hat3),2)," | ",
                     methods[4],":",round(mean(e.hat4),2)," | ",
                     methods[5],":",round(mean(e.hat5),2)," |",
                     methods[6],":",round(mean(e.hat6),2),"\n"),
         ylim=range(Xts[,ns])+c(-1,1),cex.main=0.5)
    lines(Xhat1[,ns],col=colors[1])
    lines(Xhat2[,ns],col=colors[2])
    lines(Xhat3[,ns],col=colors[3])
    lines(Xhat4[,ns],col=colors[4])
    lines(Xhat5[,ns],col=colors[5])
    lines(Xhat6[,ns],col=colors[6])
    legend("topleft",
           c("real",methods),
           col=c("black",colors),lty=1,cex=0.5)
    browser()
  }
}


