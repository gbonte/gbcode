
A<-NULL
rm(list=ls())
A<-NULL
A<-rbind(A,c(17,1,11.9))
A<-rbind(A,c(19,1.09,12.8))
A<-rbind(A,c(49,0.2,14.3))
A<-rbind(A,c(59,0.18,15.3))
A<-rbind(A,c(21,1.02,12.2))
A<-rbind(A,c(19,1.04,12.5))
A<-rbind(A,c(17,1.06,12.6))
A<-rbind(A,c(62,0.0,16.3))
A<-rbind(A,c(21,1.08,12.7))
A<-rbind(A,c(61,0.18,17.7))
A<-rbind(A,c(45,0.5,14.0))
A<-rbind(A,c(65,0.17,17.6))

A<-data.frame(A)

colnames(A)<-c("age","cola","seconds")
thr<-30
ind<-which(A[,1]<thr)

ml<-lm(seconds~cola,data=A[ind,])

plot(A[ind,"cola"],A[ind,"seconds"],xlab="Liters per day",
     ylab="100m performance (seconds)")

hat<-predict(ml,A[ind,])
lines(A[ind,"cola"],hat)





