
library(tidyverse)

P<-function(model,...){
  M<-as_tibble(Model)
  args <- rlang::enexprs(...)
  
  sum(M%>%filter(!!! args)%>%select(p))
}



Model<-data.frame(owner=
c("IT","BE","IT","BE","IT","BE","IT","BE"),
cook=c("IT","IT","BE","BE","IT","IT","BE","BE"),
pizza=c("GOOD","GOOD","GOOD","GOOD","BAD","BAD","BAD","BAD"),
p=c(0.378,0.168,0.012,0.032,0.162,0.072,0.048,0.128))

P(Model,pizza=="GOOD")
P(Model,pizza=="GOOD", owner=="IT")/P(Model,owner=="IT")
P(Model,pizza=="GOOD", owner=="BE")/P(Model,owner=="BE")

## P(pizza|owner="italian",cook="italian") =  P(pizza|cook="italian")=P(pizza|owner="belgian",cook="italian")
P(Model,pizza=="GOOD",cook=="IT",owner=="IT")/P(Model,cook=="IT",owner=="IT")
P(Model,pizza=="GOOD",cook=="IT")/P(Model,cook=="IT")
P(Model,pizza=="GOOD",cook=="IT",owner=="BE")/P(Model,cook=="IT",owner=="BE")

## P(pizza|owner="italian",cook="italian") =  P(pizza|cook="italian")=P(pizza|owner="belgian",cook="italian")
P(Model,pizza=="GOOD",cook=="BE",owner=="IT")/P(Model,cook=="BE",owner=="IT")
P(Model,pizza=="GOOD",cook=="BE")/P(Model,cook=="BE")
P(Model,pizza=="GOOD",cook=="BE",owner=="BE")/P(Model,cook=="BE",owner=="BE")


