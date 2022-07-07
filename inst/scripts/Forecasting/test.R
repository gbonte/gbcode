
rm(list=ls())

library(gbcode)
library(keras)
library(parallel)
library(doParallel)


ncores=2
cl <- parallel::makeForkCluster(ncores)
doParallel::registerDoParallel(cl)

N<-100
methods=c("uni","uni","dfm","dfml","uni","multifs")
Nval=5
m=3

n=20
allE1=NULL
allE2=NULL
allE3=NULL
allE4=NULL
allE5=NULL
allE6<-NULL
for (number in 1:14){ # number of series
  for (H in c(5,10,20)){ ## horizons
    for (it in c(0,10)){ ## increment of noise varaince
      
      set.seed(number+it)
      
      #### DATA GENERATION
      GX=genstar(N,n,number=number,s=0.1+0.01*it,loc=4)
      
      
      XX=array(rnorm(100*20),c(100,20))
      Ntr=round(2*N/3)
      S=seq(Ntr,N-H,length.out=Nval)
      for (s in S){
      #FF<-foreach(s=S) %do% {
        Xtr<-XX[1:s,]
        Xts<-XX[(s+1):min(N,(s+H)),]

        Xhat3=MmultiplestepAhead(Xtr,m,H,multi=methods[4],pc0=2,
                                 dfmlmodels=c("lindirect"))
        
         ehat3=mean(apply((Xts-Xhat3)^2,2,mean))
        ehat4=ehat3
        ehat5=ehat4
        ehat6=ehat5
        ehat1=ehat3
        ehat2=ehat3
        print(ehat3)
        list(
             allE3=ehat3,
             allE4=ehat4)
       
      } ## for s
      browser()
      for (f in 1:length(FF)){
        allE1=rbind(allE1,FF[[f]]$allE1)
        allE2=rbind(allE2,FF[[f]]$allE2)
        allE3=rbind(allE3,FF[[f]]$allE3)
        allE4=rbind(allE4,FF[[f]]$allE4)
        allE5=rbind(allE5,FF[[f]]$allE5)
        allE6=rbind(allE6,FF[[f]]$allE6)
        
      }
      ##### PRINT OUT OF RESULTS
      
      
      cat("\n **** \n n=",n,"number=",number, " H=",H, "\n")
      cat( methods[1],":",mean(allE1[,4])," | ")
      cat( methods[2],":",mean(allE2[,4])," | ")
      cat( methods[3],":",mean(allE3[,4])," | ")
      cat( methods[4],":",mean(allE4[,4])," | ")
      cat( methods[5],":",mean(allE5[,4])," |")
      cat( methods[6],":",mean(allE6[,4]),"\n")
      
      cat("\n")
      
      
    }## for H
  }  ## for number
  
  save(file="MMSynth.Rdata",list=c("methods","N","allE1","allE2","allE3",
                                   "allE4","allE5","allE6"))
} ## for n
#

