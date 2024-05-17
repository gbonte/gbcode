rm(list=ls())
library(reticulate)
library("VARshrink")
require(gbcode)
library(doParallel)

ncores=5
cl <- makeForkCluster(ncores)
registerDoParallel(cl)


Fhat<-NULL
savefile<-"assessMVp.Rdata"
methods=c("UNI","DFML","MIMO_rr","MIMO_pls","MISO_rr",
          "MIMOSO_rr","DFM","MITER_rr","MIMO_cca",
          "VARs", "darts_nbeats","darts_tft","darts_TCN",
          "darts_transformer","darts_nhits","darts_RNN","darts_blockRNN",
          "darts_lightGBM","darts_xGBM","darts_RandomForest")

load("./data/STdata_big.Rdata")
for ( f in 1:length(STnames))
  if (NROW(STdata[[f]])>160) {
    D=STdata[[f]]
    wna<-which(apply(D,2,sd)<0.001)
    if (length(wna)>0)
      D=D[,-wna]
    cat(STnames[[f]],"\n"
    )
    print(dim(D))
    #  print(D[1:3,1])
    Nsets=5  
    Lcv=ncores ## number of rolling validations
    n=3
    Nmax<-1000 ## max number of series samples
    mmax<-50  ## max number of series
    
    if (NCOL(D)<mmax)
      Nsets=1
    visualize=FALSE
    season=TRUE
    execute=TRUE
    
    
    if (NROW(D)>Nmax)
      D=D[1:Nmax,]
    
    N=NROW(D)
    H=10
    cat("H=",H,"\n")
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
    E.hat11=NULL
    E.hat12=NULL
    E.hat13=NULL
    E.hat14=NULL
    E.hat15=NULL
    E.hat16=NULL
    E.hat17=NULL
    E.hat18=NULL
    E.hat19=NULL
    E.hat20=NULL
    
    
    for (r in 1:Nsets){
      set.seed(f+r)
      D2=D
      if (NCOL(D2)>mmax)
        D2=D2[,sample(1:NCOL(D2),mmax)]
      D2=scale(D2)
      N=NROW(D2)
      m=NCOL(D2)
      
      NtrCV=round(seq(N-3*H,N-H,length.out=Lcv))
      
      FF<-foreach( Ntr=NtrCV )%dopar%{
        ##            for (Ntr in NtrCV){
        SeasP<-NULL
        
        if (season){
          X=NULL
          bestS<-NULL
          for (i in 1:m){
            D2[,i]=remNA(D2[,i])
            S=detectSeason(D2[1:Ntr,i],Ls=N,pmin=0.01)
            X=cbind(X,D2[,i]-S$spattern-S$strend)
            SeasP<-cbind(SeasP,S$spattern+S$strend)
            bestS<-c(bestS,S$best)
          }
        } else {
          X=D2
        }
        
        
        
        
        Xtr=X[1:Ntr,]
        Xts=D2[(Ntr+1):(Ntr+H),]
        if (season){
          SPts=SeasP[(Ntr+1):(Ntr+H),]
        } 
        
        Xhat1=MmultiplestepAhead(Xtr,n,H,multi=methods[1],uni="arima")
        
        
        cat(".")
        Xhat2=MmultiplestepAhead(Xtr,n,H,multi=methods[2],cdfml=2,dfmlmodels = "MIMO")
        
        cat(".")
        Xhat3=MmultiplestepAhead(Xtr,n,H,multi=methods[3])
        
        cat(".")
        
        Xhat4=MmultiplestepAhead(Xtr,n,H,multi=methods[4])
        
        cat(".")
        
        Xhat5=MmultiplestepAhead(Xtr,n,H,multi=methods[5])
        
        cat(".")
        
        Xhat6=MmultiplestepAhead(Xtr,n,H,multi=methods[6])
        
        cat(".")
        
        Xhat7=MmultiplestepAhead(Xtr,n,H,multi=methods[7])
        
        
        cat(".")
        
        Xhat8=MmultiplestepAhead(Xtr,n,H,multi=methods[8])
        
        cat(".")
        Xhat9=MmultiplestepAhead(Xtr,n,H,multi=methods[9])
        cat(".")
        Xhat10=MmultiplestepAhead(Xtr,n,H,multi=methods[10])
        cat(".")
        Xhat11=MmultiplestepAhead(Xtr,n,H,multi=methods[11])
        cat(".")
        Xhat12=MmultiplestepAhead(Xtr,n,H,multi=methods[12])
        cat(".")
        Xhat13=MmultiplestepAhead(Xtr,n,H,multi=methods[13])
        
        cat(".")
        
        Xhat14=MmultiplestepAhead(Xtr,n,H,multi=methods[14])
        
        cat(".")
        
        Xhat15=MmultiplestepAhead(Xtr,n,H,multi=methods[15])
        
        cat(".")
        
        Xhat16=MmultiplestepAhead(Xtr,n,H,multi=methods[16])
        
        cat(".")
        
        Xhat17=MmultiplestepAhead(Xtr,n,H,multi=methods[17])
        cat(".")
        
        Xhat18=MmultiplestepAhead(Xtr,n,H,multi=methods[18])
        
        cat(".")
        Xhat19=MmultiplestepAhead(Xtr,n,H,multi=methods[19])
        cat(".")
        Xhat20=MmultiplestepAhead(Xtr,n,H,multi=methods[20])
        
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
          Xhat11=Xhat11+SPts
          Xhat12=Xhat12+SPts
          Xhat13=Xhat13+SPts
          Xhat14=Xhat14+SPts
          Xhat15=Xhat15+SPts
          Xhat16=Xhat16+SPts
          Xhat17=Xhat17+SPts
          Xhat18=Xhat18+SPts
          Xhat19=Xhat19+SPts
          Xhat20=Xhat20+SPts
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
        e.hat11=apply((Xts-Xhat11)^2,2,mean)
        e.hat12=apply((Xts-Xhat12)^2,2,mean)
        e.hat13=apply((Xts-Xhat13)^2,2,mean)
        e.hat14=apply((Xts-Xhat14)^2,2,mean)
        e.hat15=apply((Xts-Xhat15)^2,2,mean)
        e.hat16=apply((Xts-Xhat16)^2,2,mean)
        e.hat17=apply((Xts-Xhat17)^2,2,mean)
        e.hat18=apply((Xts-Xhat18)^2,2,mean)
        e.hat19=apply((Xts-Xhat19)^2,2,mean)
        e.hat20=apply((Xts-Xhat20)^2,2,mean)
        
        list(mhat1=mean(e.hat1),
             mhat2=mean(e.hat2),
             mhat3=mean(e.hat3),
             mhat4=mean(e.hat4),
             mhat5=mean(e.hat5),
             mhat6=mean(e.hat6),
             mhat7=mean(e.hat7),
             mhat8=mean(e.hat8),
             mhat9=mean(e.hat9),
             mhat10=mean(e.hat10),
             mhat11=mean(e.hat11),
             mhat12=mean(e.hat12),
             mhat13=mean(e.hat13),
             mhat14=mean(e.hat14),
             mhat15=mean(e.hat15),
             mhat16=mean(e.hat16),
             mhat17=mean(e.hat17),
             mhat18=mean(e.hat18),
             mhat19=mean(e.hat19),
             mhat20=mean(e.hat20))
      } ## foreach
      for ( ff in 1:length(FF)){
        E.hat1=c(E.hat1,FF[[ff]]$mhat1)
        E.hat2=c(E.hat2,FF[[ff]]$mhat2)
        E.hat3=c(E.hat3,FF[[ff]]$mhat3)
        E.hat4=c(E.hat4,FF[[ff]]$mhat4)
        E.hat5=c(E.hat5,FF[[ff]]$mhat5)
        E.hat6=c(E.hat6,FF[[ff]]$mhat6)
        E.hat7=c(E.hat7,FF[[ff]]$mhat7)
        E.hat8=c(E.hat8,FF[[ff]]$mhat8)
        E.hat9=c(E.hat9,FF[[ff]]$mhat9)
        E.hat10=c(E.hat10,FF[[ff]]$mhat10)
        E.hat11=c(E.hat11,FF[[ff]]$mhat11)
        E.hat12=c(E.hat12,FF[[ff]]$mhat12)
        E.hat13=c(E.hat13,FF[[ff]]$mhat13)
        E.hat14=c(E.hat14,FF[[ff]]$mhat14)
        E.hat15=c(E.hat15,FF[[ff]]$mhat15)
        E.hat16=c(E.hat16,FF[[ff]]$mhat16)
        E.hat17=c(E.hat17,FF[[ff]]$mhat17)
        E.hat18=c(E.hat18,FF[[ff]]$mhat18)
        E.hat19=c(E.hat19,FF[[ff]]$mhat19)
        E.hat20=c(E.hat20,FF[[ff]]$mhat20)
      }
      if (FALSE){
        cat( "f=",f,":: r=",r, "::", methods[1],":",mean(E.hat1)," | ")
        cat( methods[2],":",mean(E.hat2)," | ")
        cat( methods[3],":",mean(E.hat3)," | ")
        cat( methods[4],":",mean(E.hat4)," | ")
        cat( methods[5],":",mean(E.hat5)," |")
        cat( methods[6],":",mean(E.hat6)," |")
        cat( methods[7],":",mean(E.hat7)," |")
        cat( methods[8],":",mean(E.hat8)," |")
        cat( methods[9],":",mean(E.hat9)," |")
        cat( methods[10],":",mean(E.hat10)," |")
        cat( methods[11],":",mean(E.hat11)," |")
        cat( methods[12],":",mean(E.hat12)," | ")
        cat( methods[13],":",mean(E.hat13)," | ")
        cat( methods[14],":",mean(E.hat14)," | ")
        cat( methods[15],":",mean(E.hat15)," |")
        cat( methods[16],":",mean(E.hat16)," |")
        cat( methods[17],":",mean(E.hat17)," |")
        cat( methods[18],":",mean(E.hat18)," |")
        cat( methods[19],":",mean(E.hat19)," |")
        cat( methods[20],":",mean(E.hat20)," \n")
      }
    } ## for r 
    Fhat<-rbind(Fhat,cbind(numeric(length(E.hat1))+f,E.hat1,E.hat2,E.hat3,
                           E.hat4,E.hat5,
                           E.hat6,E.hat7,E.hat8,E.hat9,E.hat10,
                           E.hat11,E.hat12,E.hat13,
                           E.hat14,E.hat15,
                           E.hat16,E.hat17,E.hat18,E.hat19,E.hat20))
    colnames(Fhat)=c("file",methods)
    
    if (NROW(Fhat)>1)
        print(apply(Fhat[,2:NCOL(Fhat)],2,mean))
    else
        print(Fhat[2:NCOL(Fhat)])
    save(file=savefile,list=c("Fhat","Nmax","mmax","Lcv","Nsets"))
    
  } ## for f

