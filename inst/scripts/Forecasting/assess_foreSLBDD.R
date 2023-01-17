rm(list=ls())
library("SLBDD")
library("VARshrink")
require(gbcode)
require(MTS)
library(keras)
require(pls)

files=c("qgdp","tenstocks","ibmspko","mexpimpcnus","gdpsimple6c8018","FREDMDApril19","PElectricity1344","Stockindexes99world",
        "UMEdata20002018", "clothing","TaiwanAirBox032017")
# mexpimpcnus Monthly Exports and Imports of China and United States.
# gdpsimple6c8018
#FREDMDApril19     Federal Reserve Bank at St Louis.
#CPIEurope200015   m=33 monthly Price Indexes EUUS
# Stockindexes99world World Stock Indexes: daily stock indices of m=99 financial markets
#UMEdata20002018   Quarterly Economic Series of the European Monetary Union (m=57)
# clothing          Cloth sales in China m=25 for 1805 days
#TaiwanAirBox032017 Hourly PM25 Measurements from Air-Box Devices in Taiwan (m=516)
#PElectricity1344 Electricity Prices in New England and USA- weekly (m=1344)
Fhat<-NULL
data("mts-examples")

for ( f in 1:length(files)){
  if (f<=3){
    D = get(files[f], asNamespace('MTS'))
    if (f==2 || f==3)
      D=D[,2:NCOL(D)]
    if (f==1)
      D=D[,3:NCOL(D)]
  }else{
    D = get(files[f], asNamespace('SLBDD'))
  }
  if (files[f]=="Stockindexes99world"){
    D=D[,2:NCOL(D)]
    D=apply(log(D),2,diff)
    ## compute returns
  }
  print(dim(D))
  print(D[1:3,1])
  Nsets=1  
  Lcv=3 ## number of rolling validations
  n=3
  Nmax<-1000 ## max number of series samples
  mmax<-50 ## max number of series
  
  if (NCOL(D)<mmax)
    Nsets=1
  visualize=FALSE
  season=TRUE
  execute=TRUE
  methods=c("uni","VARs","dfm","dfm","multifs2","multicca","multifs","uni","multirr","uni")
  #methods=c("uni","VAR","VARs","uni","uni","uni","uni","multifs","uni","multifs2")

  
  
  if (NROW(D)>Nmax)
    D=D[1:Nmax,]
  
  N=NROW(D)
  H=max(2,min(10,round(N/30)))
  
  E.hat1=NULL
  E.hat2=NULL
  E.hat3=NULL
  E.hat4=NULL
  E.hat5=NULL
  E.hat6=NULL
  E.hat7=NULL
  E.hat8=NULL
  E.hat9=NULL
  E.hat10=NULL
  
  for (r in 1:Nsets){
    set.seed(f+r)
    D2=D
    if (NCOL(D2)>mmax)
      D2=D2[,sample(1:NCOL(D2),mmax)]
    D2=scale(D2)
    N=NROW(D2)
    m=NCOL(D2)
    
    for ( Ntr in round(seq(N-3*H,N-H,length.out=Lcv))){
      
      SeasP<-NULL
      
      if (season){
        X=NULL
        bestS<-NULL
        for (i in 1:m){
          S=detectSeason(D2[1:Ntr,i],Ls=N,pmin=0.01)
          X=cbind(X,D2[,i]-S$spattern-S$strend)
          SeasP<-cbind(SeasP,S$spattern+S$strend)
          bestS<-c(bestS,S$best)
        }
      } else {
        X=D2
      }
      print(which(!is.na(bestS)))
      
      
      Xtr=X[1:Ntr,]
      Xts=D2[(Ntr+1):(Ntr+H),]
      if (season){
        SPts=SeasP[(Ntr+1):(Ntr+H),]
      } 
      #  methods=c("uni","VAR","VARs","dfm","dfm","dfml","dfml","multifs","multifs2","multipls")
      
      Xhat1=MmultiplestepAhead(Xtr,n,H,multi=methods[1],uni="stat_comb")
      cat(".")
      Xhat2=MmultiplestepAhead(Xtr,n,H,multi=methods[2])
      cat(".")
      Xhat3=MmultiplestepAhead(Xtr,n,H,multi=methods[3])
      cat(".")
      Xhat4=MmultiplestepAhead(Xtr,n,H,multi=methods[4],
                               dfmlmodels=c("stat_comb"))
      cat(".")
      Xhat5=MmultiplestepAhead(Xtr,n,H,multi=methods[5],
                               dfmlmodels=c("mimorr"))
      cat(".")
      Xhat6=MmultiplestepAhead(Xtr,n,H,multi=methods[6],
                               uni="mimocca")
      cat(".")
      Xhat7=MmultiplestepAhead(Xtr,n,H,multi=methods[7],cdfml=2, Lcv=5,
                               dfmlmodels=c("lazydirect","stat_comb"))
      cat(".")
      Xhat8=MmultiplestepAhead(Xtr,n,H,multi=methods[8],mod="lin")
      cat(".")
      Xhat9=MmultiplestepAhead(Xtr,n,H,multi=methods[9],uni="mimorr")
      cat(".")
      Xhat10=MmultiplestepAhead(Xtr,n,H,multi=methods[10],
                                uni="lindirect")
      cat(".")
      
      
      if (season){
        Xhat1=Xhat1+SPts
        Xhat2=Xhat2+SPts
        Xhat3=Xhat3+SPts
        Xhat4=Xhat4+SPts
        Xhat5=Xhat5+SPts
        Xhat6=Xhat6+SPts
        Xhat7=Xhat7+SPts
        Xhat8=Xhat8+SPts
        Xhat9=Xhat9+SPts
        Xhat10=Xhat10+SPts
      }
      m=NCOL(X)
      e.hat1=apply((Xts-Xhat1)^2,2,mean)
      e.hat2=apply((Xts-Xhat2)^2,2,mean)
      e.hat3=apply((Xts-Xhat3)^2,2,mean)
      e.hat4=apply((Xts-Xhat4)^2,2,mean)
      e.hat5=apply((Xts-Xhat5)^2,2,mean)
      e.hat6=apply((Xts-Xhat6)^2,2,mean)
      e.hat7=apply((Xts-Xhat7)^2,2,mean)
      e.hat8=apply((Xts-Xhat8)^2,2,mean)
      e.hat9=apply((Xts-Xhat9)^2,2,mean)
      e.hat10=apply((Xts-Xhat10)^2,2,mean)
      
      E.hat1=c(E.hat1,mean(e.hat1))
      E.hat2=c(E.hat2,mean(e.hat2))
      E.hat3=c(E.hat3,mean(e.hat3))
      E.hat4=c(E.hat4,mean(e.hat4))
      E.hat5=c(E.hat5,mean(e.hat5))
      E.hat6=c(E.hat6,mean(e.hat6))
      E.hat7=c(E.hat7,mean(e.hat7))
      E.hat8=c(E.hat8,mean(e.hat8))
      E.hat9=c(E.hat9,mean(e.hat9))
      E.hat10=c(E.hat10,mean(e.hat10))
      
      
      cat( files[f],":: r=",r, "::", methods[1],":",mean(E.hat1)," | ")
      cat( methods[2],":",mean(E.hat2)," | ")
      cat( methods[3],":",mean(E.hat3)," | ")
      cat( methods[4],":",mean(E.hat4)," | ")
      cat( methods[5],":",mean(E.hat5)," |")
      cat( methods[6],":",mean(E.hat6)," |")
      cat( methods[7],":",mean(E.hat7)," |")
      cat( methods[8],":",mean(E.hat8)," |")
      cat( methods[9],":",mean(E.hat9)," |")
      cat( methods[10],":",mean(E.hat10),"\n")
    }
  } ## for r 
  Fhat<-rbind(Fhat,cbind(numeric(length(E.hat1))+f,E.hat1,E.hat2,E.hat3,E.hat4,E.hat5,
              E.hat6,E.hat7,E.hat8,E.hat9,E.hat10))
  colnames(Fhat)=c("file",methods)
  save(file="assess.Rdata",list=c("Fhat","Nmax","mmax","Lcv","Nsets"))
  
} ## for f

