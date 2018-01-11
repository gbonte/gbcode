rm(list=ls())
library(gbcode)
dir<-"/Users/gianluca/Dropbox/bontempi_office/Rlang/nuits/"
##dir<-"C:/Documents and Settings/gianluca/Bureau/bontempi/R/"

source(paste(dir,"gsloo.R",sep=""))
source(paste(dir,"util.R",sep=""))

no.series<-1

series<-c("A_all","chao","leuven","sunspots2")
nvar<-c(15,15,30,50)
s<-series[no.series]


name1<-paste(dir,s,".txt",sep="")
RD<-read.table(name1,sep="\t",dec=".")


TS<-RD[,1]

N<-NROW(TS)
TS<-array(resid(lm(TS ~ seq(TS))),c(N,1)) ## removing trends

TS<-scale(TS)
Cent<-attr(TS,"scaled:center")
Sc<-attr(TS,"scaled:scale")

TS1<-TS[1:1000,1]

TS2<-TS[1001:1200,1]


t=1:length(TS1)#seq(0,10,by=0.01)
y=TS1
n=10
H=20
X<-NULL
Y<-NULL
for (i in n:(length(t)-H)){
  for (h in 0:H){
    X=rbind(X,c(h,y[(i-n+1):(i)]))
    Y=c(Y,y[i+h])
  }
  
  
}

N<-NROW(X)
print(N)
Itr<-1:round(N/2)
Its<-setdiff(1:N,Itr)
Xs<-scale(X)
Yhat<-pred("rf",Xs[Itr,],Y[Itr],Xs[Its,],class=FALSE)
print(mean((Yhat-Y[Its])^2)/var(Y[Its]))
dim(X)

Ns<-length(y)

Tshat<-numeric(H)
for (h in 1:H){
  q<-scale(array(c(h,y[(Ns-n+1):(Ns)]),c(1,n+1)),attr(Xs,"scaled:center"),attr(Xs,"scaled:scale"))
  Tshat[h]<-pred("rf",Xs,Y,q,class=FALSE)
  
}
mean((TS2[1:H]-Tshat)^2)/var(TS2[1:H])

plot(TS2[1:H])
lines(Tshat)


Tshat2=multiplestepAhead(TS1, n, H, D = 0, method = "direct", Kmin = 3, C = 2,  FF = 0)
mean((TS2[1:H]-Tshat2)^2)/var(TS2[1:H])
lines(Tshat2,col="red")