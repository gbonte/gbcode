
rm(list=ls())
library("SLBDD")
library("psych")
require(gbcode)
library(keras)
library(doParallel)
library(MTS)
library(VARshrink)

ncores=5
#cl <- makeForkCluster(ncores)
#registerDoParallel(cl)


R=10
methods=c("uni","uni","dfm","dfml","multifs","VARs")
Nval=5
order=3
#
#FREDMDApril19     Federal Reserve Bank at St Louis.
#CPIEurope200015   m=33 monthly Price Indexes EUUS
# Stockindexes99world World Stock Indexes: daily stock indices of m=99 financial markets
#UMEdata20002018   Quarterly Economic Series of the European Monetary Union (m=57)
# clothing          Cloth sales in China m=25 for 1805 days
#TaiwanAirBox032017 Hourly PM25 Measurements from Air-Box Devices in Taiwan (m=516)
#PElectricity1344 Electricity Prices in New England and USA- weekly (m=1344)
season=TRUE
logret=numeric(10)
logret[3]=1
names=c("FREDMDApril19","CPIEurope200015","Stockindexes99world",
        "UMEdata20002018","clothing",
        "TaiwanAirBox032017","PElectricity1344")
HH<-c(10,20,50)
seqn=c(20,50,100)
allE1=NULL
allE2=NULL
allE3=NULL
allE4=NULL
allE5=NULL
allE6<-NULL
Nmax=1000
for (nn in 4:length(names)){
  
  DD = get(names[nn], asNamespace('SLBDD'))
  w=which(colnames(DD) %in% c("caldt","Date","hour") )
  if (length(w)>0)
    DD=DD[,-w]
  print(dim(DD))
  N=NROW(DD)
  if (logret[nn]>0){
    D=DD  
    DD=DD[-N,]*0
    for (i in 1:NCOL(D))
      DD[,i]=log(D[-1,i]/D[-NROW(D),i])
  } 
  
  if (N>Nmax)
    DD=DD[1:Nmax,]
  
  DD=scale(DD)
  NX=NROW(DD)
  for (mmax in seqn){
    set.seed(mmax)
    D=DD
    if (NCOL(DD)>mmax)
      D=DD[,sample(1:NCOL(DD),mmax)]
    m=NCOL(D)
    SeasP<-NULL
    Ntr=round(3*NX/4)
    if (season){
      X=NULL
      bestS=NULL
      for (i in 1:m){
        SS=detectSeason(D[1:Ntr,i],Ls=NX,pmin=0.1)
        X=cbind(X,D[,i]-SS$spattern-SS$strend)
        bestS<-c(bestS,SS$best)
        SeasP<-cbind(SeasP,SS$spattern+SS$strend)
        
      }
    } else {
      X=D
    }
    
    browser()
    wna=which(is.na(apply(X,1,sum)))
    if (length(wna)>0)
      X=X[-wna,]
    
    NX=NROW(X)
    n=NCOL(X)
    number=3
    for (H in c(2,HH[which(HH<(NX/5))])){ ## horizons
      
      
      cat(names[nn]," N=",NROW(X)," n=",NCOL(X),
          "Ntr=",Ntr,"\n")
      S=seq(Ntr,NX-H,length.out=Nval)
      
      FF<-foreach(s=S)%dopar%{
        ##for (s in S){ 
        Itr<-1:s
        Xtr=X[1:s,]
        Xts=D[(s+1):min(NX,(s+H)),]
        if (season){
          SPts=SeasP[(s+1):min(NX,(s+H)),]
        } 
        Xhat1=MmultiplestepAhead(Xtr,order,H,multi=methods[1])
        Xhat2=MmultiplestepAhead(Xtr,order,H,multi=methods[2],unimethod="stat_comb")
        Xhat3=MmultiplestepAhead(Xtr,order,H,multi=methods[3],
                                 dfmlmodels=c("lindirect"))
        Xhat4=MmultiplestepAhead(Xtr,order,H,multi=methods[4],cdfml=3,
                                 dfmlmodels=c("lazydirect","lindirect"),)
        Xhat5=MmultiplestepAhead(Xtr,order,H,multi=methods[5])
        Xhat6=MmultiplestepAhead(Xtr,order,H,multi=methods[6])
        
        if (season){
          Xhat1=Xhat1+SPts
          Xhat2=Xhat2+SPts
          Xhat3=Xhat3+SPts
          Xhat4=Xhat4+SPts
          Xhat5=Xhat5+SPts
          Xhat6=Xhat6+SPts
        }
        
        e.hat1=apply((Xts-Xhat1)^2,2,mean)
        e.hat2=apply((Xts-Xhat2)^2,2,mean)
        e.hat3=apply((Xts-Xhat3)^2,2,mean)
        e.hat4=apply((Xts-Xhat4)^2,2,mean)
        e.hat5=apply((Xts-Xhat5)^2,2,mean)
        e.hat6=apply((Xts-Xhat6)^2,2,mean)
        
        list(allE1=c(H,number,n,mean(e.hat1),nn),
             allE2=c(H,number,n,mean(e.hat2),nn),
             allE3=c(H,number,n,mean(e.hat3),nn),
             allE4=c(H,number,n,mean(e.hat4),nn),
             allE5=c(H,number,n,mean(e.hat5),nn),
             allE6=c(H,number,n,mean(e.hat6),nn))
        
      } ## for s
      
      for (f in 1:length(FF)){
        allE1=rbind(allE1,FF[[f]]$allE1)
        allE2=rbind(allE2,FF[[f]]$allE2)
        allE3=rbind(allE3,FF[[f]]$allE3)
        allE4=rbind(allE4,FF[[f]]$allE4)
        allE5=rbind(allE5,FF[[f]]$allE5)
        allE6=rbind(allE6,FF[[f]]$allE6)
        
      }
      ##### PRINT OUT OF RESULTS
      
      
      cat("\n **** \n ","names[nn]=", names[nn],"n=",n,"number=",number, " H=",H, "\n")
      cat( methods[1],":",mean(allE1[,4])," | ")
      cat( methods[2],":",mean(allE2[,4])," | ")
      cat( methods[3],":",mean(allE3[,4])," | ")
      cat( methods[4],":",mean(allE4[,4])," | ")
      cat( methods[5],":",mean(allE5[,4])," |")
      cat( methods[6],":",mean(allE6[,4]),"\n")
      
      cat("\n")
      
      
    }## for H
    
    
  }
  
  save(file="MM.Rdata",list=c("allE1","allE2","allE3",
                              "allE4","allE5","allE6"))
}
