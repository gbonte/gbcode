
smape<-function(ts1,ts2,Cent,Sc){
  ts1<-Cent+ts1*Sc
  ts2<-Cent+ts2*Sc
  mean(abs(ts1-ts2)/(ts1+ts2)/2)*100
 
}


nlcor<-function(x,y){
  require(lazy)
  N<-length(x)
  I<-sample(1:N,round(N/3))
  
  data<-data.frame(x[I],y[I])
 
  y.lazy <- lazy(y ~ x,data,control=lazy.control(linIdPar=c(round(N/2),N)))
  yh<-predict(y.lazy, newdata=x[-I])$h
 
  cor(y[-I], yh)
}

remNA<-function(TS){

  return(approx(seq(TS),TS,seq(TS))$y)
  
}


MakeInput<-function(ts, n, delay,hor=1,w=1){

no.data<-NROW(ts)
no.var<-NCOL(ts)
a<-NROW(n)
b<-NCOL(n) 
if (a!=no.var) 
	stop('Error in the size of n')

 
N<-no.data-max(n)-max(delay) 
 
Input<-array(0,c(N,sum(n))) 
Output<-array(0,c(N,hor))

for (i in 1:N) { 
  for (j in 1:no.var) { 
##    for (k in 1:n[j]) { 
##      Input[i,sum(n[1:j-1])+k]<-ts[i+n[j]-k+max(n)-n[j]+max(delay)-delay[j],j] 
      
##    }
    k<-1:n[j]
    Input[i,sum(n[1:j-1])+k]<-ts[i+n[j]-k+max(n)-n[j]+max(delay)-delay[j],j]

    Output[i,1:hor]<-numeric(hor)+NA
    M<-min(no.data,(i+max(n+delay)+hor-1))
 
    Output[i,1:(M-(i+max(n+delay))+1)]<-ts[(i+max(n+delay)):M,w] 
  } 
  
}

list(inp=Input,out=Output)
}



MakeInput2<-function(ts, n, delay,hor=1,w=1){
  ## put in embedding (input/output) form the multivariate time series in ts
  ## ts [no.data,no.var]
  ## n [no.var]: vector of embedding orders
  ## delay [no.var]: vector of delays
  ## hor [no.var]: vector of horizons
  ## w: index of variables appearing in $out
  ## example:
  ## TS<-array(1:25,c(5,5))
  ## MakeInput2(TS,rep(2,5),delay=rep(0,5),hor=rep(1,5),w=1:5)
  

no.data<-NROW(ts)
no.var<-NCOL(ts)
a<-NROW(n)
b<-NCOL(n) 
if (a!=no.var) 
	stop('Error in the size of embedding n')
if (length(delay)!=no.var) 
	stop('Error in the size of delay')
 if (length(hor)!=length(w)) 
	stop('Error in the size of horizon hor')
N<-no.data-max(n)-max(delay) 
 
Input<-array(0,c(N,sum(n))) 
Output<-array(0,c(N,sum(hor)))

for (i in 1:N) { 
  for (j in 1:no.var) { 
##    for (k in 1:n[j]) { 
##      Input[i,sum(n[1:j-1])+k]<-ts[i+n[j]-k+max(n)-n[j]+max(delay)-delay[j],j] 
      
##    }
    k<-1:n[j]
    Input[i,sum(n[1:j-1])+k]<-ts[i+n[j]-k+max(n)-n[j]+max(delay)-delay[j],j]

    for (ww in 1:length(w)){
       if (ww==1)
        iw<-0
       else
         iw<-sum(hor[1:(ww-1)])
       
       Output[i,(iw+1):(sum(hor[1:ww]))]<-numeric(hor[ww])+NA
      M<-min(no.data,(i+max(n)+max(delay)+hor[ww]-1))

     
      Output[i,(iw+1):(iw+M-(i+max(n)+max(delay))+1)]<-ts[(i+max(n)+max(delay)):M,w[ww]]
    
    }
  } 
  
}

list(inp=Input,out=Output)
}
