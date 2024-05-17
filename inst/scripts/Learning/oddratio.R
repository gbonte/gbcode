rm(list=ls())
## Relation between FPR, FNR , priori (prevalence of disease) and oddsratio decision
## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7082531/
## Suppose that when a test is positive, I would like to be relatively sure that the patient is sick and in partcilar satisfy the relation
## P(sick|+)/P(healthy|+) > oddsratio

oddratio=2

fpr=0.05 ## P(+| healthy)=FPR
fnr=0.05 ## P(-| sick)=FNR

ps=0.1 ## P(sick)=prevalence

## Relation
##.  ps*P(+|sick)/ [(1-ps)*P(+|healthy)] > oddratio 
#.   ps*(1-fnr) / [(1-ps)*fpr] > oddratio

cat("ps*(1-fnr)/((1-ps)*fpr)"= ps*(1-fnr)/((1-ps)*fpr),"\n")

########
## plot FPR/FNR

## fpr < 1/oddratio *(ps/(1-ps))*(1-fnr)

fnr=seq(0,0.5,by=0.01)
ps=0.2
oddratio=1
plot(fnr, 1/oddratio *(ps/(1-ps))*(1-fnr),
     xlab="FNR",ylab="FPR",type="l",main=paste("prel=",ps), ylim=c(0,0.3))

for (oddratio in c(2,3,4))
lines(fnr, 1/oddratio *(ps/(1-ps))*(1-fnr),col="red")


