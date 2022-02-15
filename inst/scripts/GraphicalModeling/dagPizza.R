rm(list=ls())

library(gRain)

set.seed(0)

lx1<-c("ownI","ownB")
lx2<-c("cookI","cookB")
ly <- c("goodPizza","badPizza")

group<-1
x1    <- cptable(~x1, values=c(60,40),levels=lx1)
x2    <- cptable(~x2|x1, values=c(90,10,60,40),levels=lx2)
y.x2    <- cptable(~y|x2, values=c(70,30,20,80),levels=ly)


cpt <- compileCPT(list(x1, x2, y.x2))

bn <- grain(cpt)
tt   <- querygrain(bn, type="joint")
ptable<-tt%>%as.data.frame.table
colnames(ptable)[4]="prob"


querygrain(bn, nodes=c("x1", "y"), type="joint")

answ1=querygrain(setEvidence(bn, 
                             evidence=list( x1="ownB")),nodes="y") 


answ2=querygrain(setEvidence(bn, 
                             evidence=list( x1="ownI")),nodes="y") 

answ3=querygrain(setEvidence(bn, 
                             evidence=list( x2="cookI")),nodes="y")  
answ4=querygrain(setEvidence(bn, 
                             evidence=list( x2="cookB")),nodes="y") 

answ4=querygrain(setEvidence(bn, 
                             evidence=list( x2="cookB",x1="ownI")),nodes="y") 


querygrain(setEvidence(bn, 
                       evidence=list( x2="cookI",x1="ownB")),nodes="y") 
