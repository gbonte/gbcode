## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

## script arcing.R

library(class)
library(mlbench)
library(tree)
data(BreastCancer)
N.tr<-400
N<-nrow(BreastCancer)
I<-BreastCancer$Class=='benign'
BreastCancer$Class2[I]<-1
BreastCancer$Class2[! I]<- -1
train<-BreastCancer[1:N.tr,]
w<-rep(1/N.tr,N.tr)

test<-BreastCancer[(N.tr+1):N,]

m<-15

misc<-rep(NA,m);
alpha<-rep(NA,m)
set.seed(555)
tree.model <-tree(Class2 ~ Cl.thickness+Cell.size+Cell.shape+     
                    +Marg.adhesion+Epith.c.size+Bare.nuclei+Bl.cromatin+Normal.nucleoli+Mitoses
                  , train,control=tree.control(N.tr,mincut=50),weights=w*N.tr)

pred <- sign(predict(tree.model,test))
misc.tree <- sum(test$Class2 != pred)/length(pred)

pred.test<-rep(0,N-N.tr)
mi<-rep(0,N.tr)

for (j in 1:m)
{
  set.seed(555)
  I<-sample(seq(1,N.tr),prob=w,replace=TRUE)
  tree.model <-tree(Class2 ~ Cl.thickness+Cell.size+Cell.shape+     
                      Marg.adhesion+Epith.c.size+Bare.nuclei+Bl.cromatin+Normal.nucleoli+Mitoses,
                    train[I,],control=tree.control(N.tr,mincut=50))
  
  pred.train <-sign(predict(tree.model,train))
  
  mi<-mi+as.integer(train$Class2 != pred.train)
  
  w<- (1+mi^4)/(sum(1+mi^4))
  pred.test<-pred.test+sign(predict(tree.model,test))
  
}

pred.test<-pred.test/m
misc.arcing <- sum(test$Class2 != sign(pred.test))/length(pred.test)

cat("Misclassification single tree=",misc.tree,"\n")
cat("Misclassification boosting=",misc.arcing,"\n")
