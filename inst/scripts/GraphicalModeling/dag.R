rm(list=ls())

library(gRain)

set.seed(0)

yn <- c("yes","no")

group<-1
x1    <- cptable(~x1, values=c(40,60),levels=yn)
y.x1x2  <- cptable(~y|x1:x2, values=c(40,60,60,40,60,40,60,40),levels=yn)
x2    <- cptable(~x2, values=c(60,40),levels=yn)

x3.y  <- cptable(~x3|y, values=c(60,40,40,60),levels=yn)

cpt <- compileCPT(list(x1, x2, y.x1x2,x3.y ))

bn <- grain(cpt)
tt   <- querygrain(bn, type="joint")
ptable<-tt%>%as.data.frame.table
colnames(ptable)[5]="prob"

answ1=querygrain(setEvidence(bn, 
                             evidence=list( x1="yes",x2="no", x3="no")),nodes="y") 


answ2=querygrain(setEvidence(bn, evidence=list( y="no",x1="yes")),nodes="x3") 

answ3=querygrain(setEvidence(bn, evidence=list( y="no",x1="yes")),nodes="x2") 
answ4=querygrain(setEvidence(bn, evidence=list( x2="no",x3="no")),nodes="x1") 

Id=which(ptable$y=="no" )
In=which(ptable$x2=="no" & ptable$y=="no")
sum(ptable[In,"prob"])/sum(ptable[Id,"prob"])
sum(ptable[In,"prob"])/sum(ptable[Id,"prob"])

assign(paste("Q1.G", group, ".D", sep = ""), ptable)

assign(paste("Q1.G", group, ".A1" ,sep = ""), answ1)
assign(paste("Q1.G", group, ".A2" ,sep = ""), answ2)
assign(paste("Q1.G", group, ".A3" ,sep = ""), answ3)
assign(paste("Q1.G", group, ".A4" ,sep = ""), answ4)

answ5=querygrain(bn, nodes=c("x1", "y"), type="joint")
assign(paste("Q1.G", group, ".A5" ,sep = ""), answ5)
answ6=querygrain(bn, nodes=c("x1", "x3"), evidence=list( y="no"), type="joint")
assign(paste("Q1.G", group, ".A6" ,sep = ""), answ6)



