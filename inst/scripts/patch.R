Patch<-function(){
set.seed(0)

placebo<-c(9243,9671,11792,13357,9055,6290,12412,18806)
oldpatch<-c(17649,12013,19979,21816,13850,9806,17208,29044)
newpatch<-c(16449,14614,17274,23798,12560,10157,16570,26325)

data<-data.frame(placebo,oldpatch,newpatch)

N<-nrow(data)

B<-4000

theta.hat<-abs(mean(data[,"newpatch"])-mean(data[,"oldpatch"]))/
  (mean(data[,"oldpatch"])-mean(data[,"placebo"]))

thetaB<-numeric(B)
for (b in 1:B){
  Db<-data[sample(N,N,replace=TRUE),]
  thetaB[b]<-abs(mean(Db[,"newpatch"])-mean(Db[,"oldpatch"]))/
    (mean(Db[,"oldpatch"])-mean(Db[,"placebo"]))
}



hist(thetaB,
     main=paste("Bias=", round(abs(theta.hat-mean(thetaB)),2),
       "; Stdev=", round(sd(thetaB),2)))
abline(v=theta.hat,col="red")
abline(v=0.2,col="green")

print(paste("Probability that theta.hat >0.2=",sum(thetaB>0.2)/B))
}
