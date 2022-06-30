
rm(list=ls())

library(gbcode)
library(keras)
library(doParallel)

ncores=5
#cl <- makeForkCluster(ncores)
#registerDoParallel(cl)

N<-500
methods=c("lstm","uni","dfm","dfml","comb","multifs")
Nval=5
n=3 ## embedding order

m=10  ## number of time series
allE1=NULL
allE2=NULL
allE3=NULL
allE4=NULL
allE5=NULL
allE6<-NULL
for (m in c(20,50,100,500))
  for (number in 1:12){ # number of series generators
    for (H in c(5,20,50)){ ## horizons
      for (it in c(0,10,20)){ ## increment of noise varaince
        
        set.seed(number+it)
        
        #### DATA GENERATION
        GX=genstar(N,m,number=number,s=0.1+0.01*it,loc=4)
        X=scale(GX)
        
        Ntr=round(2*N/3)
        S=seq(Ntr,N-H,length.out=Nval)
        FF<-foreach(s=S)%do%{
          Itr<-1:s
          Xtr=X[1:s,]
          Xts=X[(s+1):min(N,(s+H)),]
          
          Xhat1=MmultiplestepAhead(Xtr,3*n,H,multi=methods[1])
          Xhat2=MmultiplestepAhead(Xtr,n,H,multi=methods[2],uni="stat_comb")
          Xhat3=MmultiplestepAhead(Xtr,n,H,multi=methods[3],pc0=2,
                                   dfmlmodels=c("lindirect"))
          Xhat4=MmultiplestepAhead(Xtr,n,H,multi=methods[4], cdfml=3,pc0=2,
                                   dfmlmodels=c("lindirect","lazydirect","stat_comb"),
                                   verbose=FALSE)
          Xhat5=MmultiplestepAhead(Xtr,n,H,multi=methods[5])
          Xhat6=MmultiplestepAhead(Xtr,n,H,multi=methods[6])
          
          e.hat1=apply((Xts-Xhat1)^2,2,mean)
          e.hat2=apply((Xts-Xhat2)^2,2,mean)
          e.hat3=apply((Xts-Xhat3)^2,2,mean)
          e.hat4=apply((Xts-Xhat4)^2,2,mean)
          e.hat5=apply((Xts-Xhat5)^2,2,mean)
          e.hat6=apply((Xts-Xhat6)^2,2,mean)
          
          list(allE1=c(H,number,m,mean(e.hat1),it),
               allE2=c(H,number,m,mean(e.hat2),it),
               allE3=c(H,number,m,mean(e.hat3),it),
               allE4=c(H,number,m,mean(e.hat4),it),
               allE5=c(H,number,m,mean(e.hat5),it),
               allE6=c(H,number,m,mean(e.hat6),it))
          
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
        
        
        cat("\n **** \n n=",n,"number=",number, " H=",H,"it=",it, "\n")
        cat( methods[1],":",mean(allE1[,4])," | ")
        cat( methods[2],":",mean(allE2[,4])," | ")
        cat( methods[3],":",mean(allE3[,4])," | ")
        cat( methods[4],":",mean(allE4[,4])," | ")
        cat( methods[5],":",mean(allE5[,4])," |")
        cat( methods[6],":",mean(allE6[,4]),"\n")
        
        cat("\n")
        
        
      }## for H
    }  ## for number
    
    save(file="MMSynth2.Rdata",list=c("methods","N","allE1","allE2","allE3",
                                      "allE4","allE5","allE6"))
  } ## for n
#

