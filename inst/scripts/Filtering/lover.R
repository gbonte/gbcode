source("hmm_vit.R")
# X= Good, Neutral, Bad
A=t(array(c(0.2,0.3,0.5,0.2,0.2,0.6,0,0.2,0.8),c(3,3)))
B=t(array(c(0.7,0.2,0.1,0.3,0.4,0.3,0,0.1,0.9),c(3,3)))

O=c(1,3,2,1,3)
p=rep(1/3,3) ## same a priori sequence
H=hmm.vit(A,B,p,O)
cat("Viterbi sequence=",c("Good","Neutral","Bad")[H$states],"\n")