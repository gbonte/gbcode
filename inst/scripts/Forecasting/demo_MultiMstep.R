
rm(list=ls())
library("SLBDD")
library("psych")
require(gbcode)
library(keras)
library(doParallel)
library(MTS)
library(VARshrink)

ncores=5
cl <- makeForkCluster(ncores)
registerDoParallel(cl)

Nmax<-500
R=10
methods=c("uni","comb","dfm","dfml","multifs","VARs")
Nval=5
m=3
#
#FREDMDApril19     Federal Reserve Bank at St Louis.
#CPIEurope200015   m=33 monthly Price Indexes EUUS
# Stockindexes99world World Stock Indexes: daily stock indices of m=99 financial markets
#UMEdata20002018   Quarterly Economic Series of the European Monetary Union (m=57)
# clothing          Cloth sales in China m=25 for 1805 days
#TaiwanAirBox032017 Hourly PM25 Measurements from Air-Box Devices in Taiwan (m=516)
#PElectricity1344 Electricity Prices in New England and USA- weekly (m=1344)
frequency=c(12,12,1,4,12,24,4,7)
logret=numeric(10)
logret[3]=1
names=c("FREDMDApril19","CPIEurope200015","Stockindexes99world",
        "UMEdata20002018","clothing",
        "TaiwanAirBox032017","PElectricity1344")
HH<-c(10,20,50)
seqn=c(10,20,50,100)
allE1=NULL
allE2=NULL
allE3=NULL
allE4=NULL
allE5=NULL
allE6<-NULL

for (nn in 1:length(names)){
  
  D = get(names[nn], asNamespace('SLBDD'))
  print(dim(D))
  
  
  w=which(colnames(D) %in% c("caldt","Date","hour") )
  if (length(w)>0)
    D=D[,-w]
  
  
  if (NROW(D)>Nmax)
    D=D[1:Nmax,]
  
  
  for (n in seqn){ 
    set.seed(n)
    if (n <=NCOL(D)){
      In=sample(1:NCOL(D),n) ## random selection subset of series
      X=D[,In]*0
      
      for (i in 1:n){
        if (frequency[nn]>2){
          A=ts(D[,In[i]],frequency =frequency[nn])
          decomposeA=stats::decompose(A,"additive")
          X[,i]=decomposeA$random
        } else {
          
          X[,i]=D[,In[i]]
          
        }
        ##plot(decomposeA$seasonal)
      } 
      
      if (logret[nn]>0){
        D=X  
        X=D[-N,]*0
        for (i in 1:n)
          X[,i]=log(D[-1,i]/D[-NROW(D),i])
      } 
      
      
      wna=which(is.na(apply(X,1,sum)))
      if (length(wna)>0)
        X=X[-wna,]
      X=scale(X)
      N=NROW(X)
      n=NCOL(X)
      
      for (number in c(3)){ # controls the size of training set
        for (H in c(2,HH[which(HH<(N/5))])){ ## horizons
          set.seed(number+n+H)
          Ntr=round(number*N/4)
          
          cat(names[nn]," N=",NROW(X)," n=",NCOL(X),
              "Ntr=",Ntr,"\n")
          S=seq(Ntr,N-H,length.out=Nval)
          FF<-foreach(s=S)%dopar%{
          ##for (s in S){
            Itr<-1:s
            Xtr=X[1:s,]
            Xts=X[(s+1):min(N,(s+H)),]
            
            Xhat1=MmultiplestepAhead(Xtr,m,H,multi=methods[1],uni="stat_theta")
            Xhat2=MmultiplestepAhead(Xtr,m,H,multi=methods[2],mod="rf",pc0=3)
            Xhat3=MmultiplestepAhead(Xtr,m,H,multi=methods[3],pc0=2,cdfml=2,
                                     dfmlmodels=c("lindirect"),Kmin=3,C=3)
            Xhat4=MmultiplestepAhead(Xtr,m,H,multi=methods[4],cdfml=3,pc0=2,
                                     dfmlmodels=c("lazydirect","stat_comb"),Kmin=3,C=3)
            Xhat5=MmultiplestepAhead(Xtr,m,H,multi=methods[5],mod="rf")
            Xhat6=MmultiplestepAhead(Xtr,m,H,multi=methods[6],mod="rf")
            
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
      }  ## for number
      
    }
  } ## for n
  save(file="MM.Rdata",list=c("allE1","allE2","allE3",
                              "allE4","allE5","allE6"))
}
