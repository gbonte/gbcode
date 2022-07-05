
detectSeason<-function(TS,maxs=20,Ls=100){
  if (length(TS)<20 || sd(TS)<0.1)
    return (1)
  if (any(is.infinite(TS)))
    return (1)
  if (sd(TS,na.rm=TRUE)<0.1)
    return (1)
  seas=1
  trnd=lm(TS ~ seq(TS))$fit
  PVS=numeric(maxs)+Inf
  
  S<-TS-trnd
  #print(S) 
  
  
  for (s in 2:maxs){
    PV=NULL
    m_S = t(matrix(data = S, nrow = s))
    for (i in 1:s){
      for (j in setdiff(1:s,i)){
        xs=m_S[,i]
        ys=unlist(m_S[,j])
        
        PV=c(PV,t.test(xs,ys)$p.value)
      }#
      
      
      
      PVS[s]=median(PV)
    }# for s
    
    
  }# add
  bests=which.min(PVS) 
  m_S = t(matrix(data = S, nrow = bests))
  spattern=apply(m_S,2,mean)
  spattern=rep(spattern,length.out=Ls)
  return(list(best=bests,spattern=spattern))
}#

TT=1:100
Y=sin(2*pi*TT/10)
plot(TT,Y,type="l")
S=detectSeason(Y)
plot(S$spattern,type="l")