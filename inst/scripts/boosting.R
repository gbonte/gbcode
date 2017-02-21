## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

## script boosting.R

library(class)
library(mlbench)
library(tree)
data(Pima.tr)
N.tr<-40
N<-nrow(Pima.tr)
Pima.tr$type<-2*as.integer(Pima.tr$type=='Yes')-1
train<-Pima.tr[1:N.tr,]
w<-rep(1/N.tr,N.tr)

test<-Pima.tr[(N.tr+1):N,]

m<-15

misc<-rep(NA,m);
alpha<-rep(NA,m)
set.seed(555)
tree.model <-tree(type ~ npreg+glu+bp+skin+bmi+ped+age, train,
                  control=tree.control(N.tr,mincut=10),weights=w*N.tr)
pred <- sign(predict(tree.model,test))
misc.tree <- sum(as.integer(test$type != sign(pred)))/length(pred)


pred.test<-rep(0,N-N.tr)
for (j in 1:m)
{
  set.seed(555)
  I<-sample(seq(1,N.tr),prob=w,replace=TRUE)
  tree.model <-tree(type ~ npreg+glu+bp+skin+bmi+ped+age, train[I,],control=tree.control(N.tr,mincut=10))
  
  pred.train <-sign(predict(tree.model,train))
  
  misc[j] <- sum(w*as.integer(train$type != pred.train))/sum(w)
  alpha[j]<-log((1-misc[j])/misc[j])
  w<- w*exp(alpha[j]*as.integer(train$type != pred.train))
  pred.test<-pred.test+alpha[j]*predict(tree.model,test)
  
  if (misc[j]>=0.49)
    w<-rep(1/N.tr,N.tr)
}


misc.boosting <- sum(test$type != sign(pred.test))/length(pred.test)


cat("Misclassification single tree=",misc.tree,"\n")
cat("Misclassification boosting=",misc.boosting,"\n")


